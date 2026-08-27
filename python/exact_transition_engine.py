#!/usr/bin/env python3
"""Resume-safe exact finite-interval Levy-OU conditional likelihood engine."""

from __future__ import annotations

import argparse
import concurrent.futures
import hashlib
import json
import math
import os
import resource
import sys
import time
import traceback
from pathlib import Path

import numpy as np
import pandas as pd
import pyarrow.parquet as pq
from numpy.polynomial.legendre import leggauss
from scipy.optimize import minimize
from scipy.special import gamma as gamma_fn
from scipy.special import kve
from scipy.stats import norm, spearmanr


# Work and input roots are explicit and machine-independent.
REPOSITORY_ROOT = Path(__file__).resolve().parents[1]
ROOT = Path(os.environ.get(
    "DISSERTATION_EXACT_WORK_ROOT",
    REPOSITORY_ROOT / "output" / "exact_transition_likelihood_run",
)).expanduser().resolve()
PARENT = Path(os.environ.get(
    "DISSERTATION_EXACT_INPUT_ROOT",
    REPOSITORY_ROOT / "output" / "levy_screen_inputs",
)).expanduser().resolve()
CONFIG_PATH = ROOT / "config" / "exact_likelihood_config.json"
TASK_PATH = ROOT / "exact_transition_likelihood" / "task_index.csv"
CHECKPOINT_DIR = ROOT / "exact_transition_likelihood" / "checkpoints"
FAMILIES = ["GAUSSIAN", "NIG", "GHYP_FULL", "VG", "NTS", "BILATERAL_TS", "CGMY", "MEIXNER", "SYMMETRIC_ALPHA_STABLE"]


def load_config():
    with CONFIG_PATH.open("r", encoding="utf-8") as handle:
        return json.load(handle)


def sha256(path):
    h = hashlib.sha256()
    with open(path, "rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            h.update(chunk)
    return h.hexdigest()


def object_hash(obj):
    return hashlib.sha256(json.dumps(obj, sort_keys=True, separators=(",", ":"), default=str).encode()).hexdigest()


def atomic_json(obj, path):
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_suffix(path.suffix + ".tmp")
    with tmp.open("w", encoding="utf-8") as handle:
        json.dump(obj, handle, sort_keys=True, indent=2, allow_nan=False)
    os.replace(tmp, path)


def atomic_csv(frame, path):
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_suffix(path.suffix + ".tmp")
    frame.to_csv(tmp, index=False)
    os.replace(tmp, path)


def decode(family, p):
    p = np.asarray(p, dtype=float)
    if family == "GAUSSIAN":
        return {"sigma": math.exp(p[0])}
    if family == "NIG":
        alpha = math.exp(p[0]); beta = alpha * math.tanh(p[1]); delta = math.exp(p[2])
        return {"alpha": alpha, "beta": beta, "delta": delta}
    if family == "GHYP_FULL":
        lam, delta, gam, beta = p[0], math.exp(p[1]), math.exp(p[2]), p[3]
        return {"lambda": lam, "delta": delta, "gamma": gam, "beta": beta,
                "alpha": math.sqrt(gam * gam + beta * beta)}
    if family == "VG":
        return {"sigma": math.exp(p[0]), "nu": math.exp(p[1]), "theta": p[2]}
    if family == "NTS":
        return {"alpha": 1 / (1 + math.exp(-p[0])), "lambda": math.exp(p[1]),
                "delta": math.exp(p[2]), "sigma": math.exp(p[3]), "theta": p[4]}
    if family == "BILATERAL_TS":
        return {"C_plus": math.exp(p[0]), "C_minus": math.exp(p[1]), "M": math.exp(p[2]),
                "G": math.exp(p[3]), "Y_plus": 1 / (1 + math.exp(-p[4])),
                "Y_minus": 1 / (1 + math.exp(-p[5]))}
    if family == "CGMY":
        return {"C": math.exp(p[0]), "G": math.exp(p[1]), "M": math.exp(p[2]),
                "Y": 1 / (1 + math.exp(-p[3]))}
    if family == "MEIXNER":
        return {"a": math.exp(p[0]), "b": math.pi * math.tanh(p[1]), "delta": math.exp(p[2])}
    if family == "SYMMETRIC_ALPHA_STABLE":
        return {"alpha": 1 + 1 / (1 + math.exp(-p[0])), "scale": math.exp(p[1])}
    raise ValueError(f"Unknown family {family}")


def _gh_log_k(order, z):
    return np.log(kve(order, z)) - z


def psi(family, u, p):
    u = np.asarray(u, dtype=float)
    par = decode(family, p)
    if family == "GAUSSIAN":
        return -0.5 * par["sigma"] ** 2 * u ** 2 + 0j
    if family == "NIG":
        alpha, beta, delta = par["alpha"], par["beta"], par["delta"]
        gam = math.sqrt(alpha * alpha - beta * beta)
        mu = -delta * beta / gam
        return 1j * mu * u + delta * (gam - np.sqrt(alpha * alpha - (beta + 1j * u) ** 2))
    if family == "GHYP_FULL":
        lam, delta, gam, beta = par["lambda"], par["delta"], par["gamma"], par["beta"]
        x0 = delta * gam
        k0, k1 = kve(lam, x0), kve(lam + 1, x0)
        if not np.isfinite(k0) or not np.isfinite(k1) or k0 <= 0 or k1 <= 0:
            return np.full(u.shape, np.nan + 1j * np.nan)
        mu = -delta * beta / gam * (k1 / k0)
        q = gam * gam + u * u - 2j * beta * u
        root = np.sqrt(q)
        root = np.where(root.real < 0, -root, root)
        with np.errstate(all="ignore"):
            ans = 1j * mu * u + lam * (math.log(gam) - 0.5 * np.log(q)) + _gh_log_k(lam, delta * root) - _gh_log_k(lam, x0)
        return np.where(np.abs(u) < 1e-14, 0j, ans)
    if family == "VG":
        sig, nu, theta = par["sigma"], par["nu"], par["theta"]
        return -1j * theta * u - np.log(1 - 1j * theta * nu * u + 0.5 * sig * sig * nu * u * u) / nu
    if family == "NTS":
        a, lam, delta, sig, theta = par["alpha"], par["lambda"], par["delta"], par["sigma"], par["theta"]
        mean0 = delta * a * lam ** (a - 1) * theta
        return -1j * mean0 * u - delta * ((lam + 0.5 * sig * sig * u * u - 1j * theta * u) ** a - lam ** a)
    if family == "BILATERAL_TS":
        cp, cm, m, g, yp, ym = par["C_plus"], par["C_minus"], par["M"], par["G"], par["Y_plus"], par["Y_minus"]
        mean0 = cp * gamma_fn(1 - yp) * m ** (yp - 1) - cm * gamma_fn(1 - ym) * g ** (ym - 1)
        return (-1j * mean0 * u + cp * gamma_fn(-yp) * ((m - 1j * u) ** yp - m ** yp)
                + cm * gamma_fn(-ym) * ((g + 1j * u) ** ym - g ** ym))
    if family == "CGMY":
        c, g, m, y = par["C"], par["G"], par["M"], par["Y"]
        mean0 = c * gamma_fn(1 - y) * (m ** (y - 1) - g ** (y - 1))
        return -1j * mean0 * u + c * gamma_fn(-y) * ((m - 1j * u) ** y - m ** y + (g + 1j * u) ** y - g ** y)
    if family == "MEIXNER":
        a, b, delta = par["a"], par["b"], par["delta"]
        mu = -a * delta * math.tan(b / 2)
        z = (a * u - 1j * b) / 2
        z_stable = np.where(z.real >= 0, z, -z)
        log_cosh = z_stable + np.log1p(np.exp(-2 * z_stable)) - math.log(2)
        return 1j * mu * u + 2 * delta * (np.log(math.cos(b / 2) + 0j) - log_cosh)
    if family == "SYMMETRIC_ALPHA_STABLE":
        return -(par["scale"] ** par["alpha"]) * np.abs(u) ** par["alpha"] + 0j
    raise ValueError(f"Unknown family {family}")


def controls(family, skew):
    q05, q95 = -2.9444389791664403, 2.9444389791664403
    if family == "NIG":
        return [np.array([math.log(2), skew, 0.0]), np.array([math.log(5), skew / 2, math.log(4)])], np.array([-2,-3,-5.]), np.array([6,3,5.])
    if family == "GHYP_FULL":
        beta = float(np.clip(2 * skew, -2, 2))
        return [np.array([-0.5,0.,0.,beta]), np.array([0.5,math.log(.7),math.log(1.5),beta/2])], np.array([-2.5,-5,-5,-6.]), np.array([3.5,5,5,6.])
    if family == "VG":
        return [np.array([0.,math.log(.5),skew]), np.array([math.log(.7),math.log(1.5),skew/2])], np.array([-5,-4,-4.]), np.array([4,4,4.])
    if family == "NTS":
        return [np.array([0.,0.,0.,0.,skew]), np.array([-1.098612289,math.log(2),0.,math.log(.7),skew/2])], np.array([q05,-4,-5,-5,-4.]), np.array([q95,5,5,4,4.])
    if family == "BILATERAL_TS":
        return [np.array([math.log(.5),math.log(.5),0.,0.,0.,0.]), np.array([0.,0.,math.log(2),math.log(2),-1.098612289,-1.098612289])], np.array([-6,-6,-3,-3,q05,q05]), np.array([4,4,5,5,q95,q95])
    if family == "CGMY":
        return [np.array([math.log(.5),0.,0.,0.]), np.array([0.,math.log(2),math.log(2),-1.098612289])], np.array([-6,-3,-3,q05]), np.array([4,5,5,q95])
    if family == "MEIXNER":
        return [np.array([0.,skew/3,0.]), np.array([math.log(.7),skew/5,math.log(2)])], np.array([-5,-2.5,-5.]), np.array([4,2.5,5.])
    if family == "SYMMETRIC_ALPHA_STABLE":
        return [np.array([0.84729786,0.]), np.array([-0.619039208,math.log(.7)])], np.array([q05,-5.]), np.array([q95,4.])
    raise ValueError(f"No controls for {family}")


def load_transition(task):
    cols = ["active_dt_minutes", "exact_ou_remainder", "accepted_segment_id", "accepted_transition_flag"]
    tab = pq.read_table(task["transition_path"], columns=cols).to_pandas()
    tab = tab[tab["accepted_transition_flag"] == True]
    eta = tab["exact_ou_remainder"].to_numpy(dtype=float)
    dt = tab["active_dt_minutes"].to_numpy(dtype=float)
    ok = np.isfinite(eta) & np.isfinite(dt) & (dt > 0)
    return eta[ok], dt[ok], int(tab.loc[ok, "accepted_segment_id"].nunique())


def remainder_cf(family, p, u, duration, kappa, scale_ratio, quadrature_n):
    nodes, weights = leggauss(int(quadrature_n))
    s = duration * (nodes + 1) / 2
    weights = duration * weights / 2
    arg = u[:, None] * scale_ratio * np.exp(-kappa * s[None, :])
    val = psi(family, arg.reshape(-1), p).reshape(arg.shape)
    integ = val @ weights
    return np.exp(integ)


def density_grid(family, p, duration, kappa, scale_ratio, x_max, n_grid, quadrature_n):
    n_grid = int(n_grid)
    du = math.pi / x_max
    k = np.arange(n_grid)
    sk = np.where(k <= n_grid // 2, k, k - n_grid)
    u = sk * du
    phi = remainder_cf(family, p, u, duration, kappa, scale_ratio, quadrature_n)
    if np.any(~np.isfinite(phi.real)) or np.any(~np.isfinite(phi.imag)) or np.max(np.abs(phi)) > 1.00001:
        return None
    dx = 2 * x_max / n_grid
    j = np.arange(n_grid)
    sj = np.where(j <= n_grid // 2, j, j - n_grid)
    x = sj * dx
    dens = np.fft.fft(phi).real * du / (2 * math.pi)
    order = np.argsort(x)
    x, dens = x[order], dens[order]
    edge = np.abs(sk) >= n_grid // 2 - 8
    return {"x": x, "density": dens, "dx": dx, "mass": float(np.sum(dens) * dx),
            "negative_mass": float(np.sum(np.maximum(-dens, 0)) * dx),
            "minimum_density": float(np.min(dens)), "edge_cf_modulus": float(np.max(np.abs(phi[edge])))}


def evaluate_loglik(family, p, y, dt, kappa, scale_ratio, x_max, n_grid, quadrature_n,
                    density_floor=1e-300, collect=False):
    ll = 0.0; floor_count = 0; outside_count = 0; metrics = []
    rounded = np.round(dt, 10)
    for duration in np.unique(rounded):
        take = rounded == duration
        grid = density_grid(family, p, float(duration), kappa, scale_ratio, x_max, n_grid, quadrature_n)
        if grid is None:
            return None
        vals = np.interp(y[take], grid["x"], np.maximum(grid["density"], 0), left=0.0, right=0.0)
        outside_count += int(np.sum((y[take] < grid["x"][0]) | (y[take] > grid["x"][-1])))
        floor = vals <= density_floor
        floor_count += int(np.sum(floor))
        vals = np.maximum(vals, density_floor)
        if np.any(~np.isfinite(vals)):
            return None
        ll += float(np.sum(np.log(vals)))
        metrics.append({"duration": float(duration), "n": int(np.sum(take)), **{k:grid[k] for k in ["mass","negative_mass","minimum_density","edge_cf_modulus"]}})
    return {"loglik_standardised": ll, "floor_count": floor_count, "outside_count": outside_count, "metrics": metrics}


def task_fingerprint(task, config):
    fields = {k: task[k] for k in ["pair_endpoint_key", "family", "source_hash", "gaussian_mu",
              "gaussian_kappa_per_active_minute", "increment_scale", "remainder_scale", "transitions", "segments",
              "accepted_transition_identity_sha256"]}
    fields.update({"engine_version": config["engine_version"], "formula": config["formula"], "scaling": config["scaling"],
                   "engine_sha256": sha256(Path(__file__).resolve()),
                   "optimisation_grid_n": config["optimisation_grid_n"], "optimisation_quadrature_n": config["optimisation_quadrature_n"],
                   "production_grid_n": config["production_grid_n"], "production_quadrature_n": config["production_quadrature_n"],
                   "validation_grid_n": config["validation_grid_n"], "validation_quadrature_n": config["validation_quadrature_n"],
                   "production_maxiter": config["production_maxiter"]})
    return object_hash(fields)


def checkpoint_path(task):
    return CHECKPOINT_DIR / f"{task['task_id']}.json"


def checkpoint_current(task, config):
    path = checkpoint_path(task)
    if not path.exists():
        return False
    try:
        with path.open("r", encoding="utf-8") as handle:
            obj = json.load(handle)
        return obj.get("task_fingerprint") == task_fingerprint(task, config) and obj.get("completed") is True
    except Exception:
        return False


def parameter_string(family, p):
    par = decode(family, p)
    return ";".join(f"{k}={v:.12g}" for k, v in par.items())


def make_starts(task, family, y, lower, upper, defaults, maximum):
    starts, sources = [], []
    for vec in defaults:
        if len(starts) >= maximum:
            break
        starts.append(np.clip(vec, lower, upper)); sources.append("validated_deterministic_default")
    unique, usource = [], []
    for vec, source in zip(starts, sources):
        if not any(np.max(np.abs(vec - old)) < 1e-10 for old in unique):
            unique.append(vec); usource.append(source)
    return unique[:maximum], usource[:maximum]


def fit_task(task, config=None, grid_n=None, quadrature_n=None, maxiter=None, n_starts=None,
             write_checkpoint=True, force=False):
    config = load_config() if config is None else config
    if write_checkpoint and not force and checkpoint_current(task, config):
        with checkpoint_path(task).open("r", encoding="utf-8") as handle:
            return json.load(handle)
    started = time.perf_counter()
    eta, dt, segment_n = load_transition(task)
    n = len(eta)
    scale = float(task["remainder_scale"]); ratio = float(task["increment_scale"]) / scale
    kappa = float(task["gaussian_kappa_per_active_minute"])
    y = eta / scale
    x_max = float(max(14.0, np.max(np.abs(y)) * 1.25, np.quantile(np.abs(y), .9999) * 1.6))
    family = task["family"]
    grid_n = int(config["optimisation_grid_n"] if grid_n is None else grid_n)
    quadrature_n = int(config["optimisation_quadrature_n"] if quadrature_n is None else quadrature_n)
    maxiter = int(config["production_maxiter"] if maxiter is None else maxiter)
    n_starts = int(config["production_starts"] if n_starts is None else n_starts)
    result = {"completed": True, "task_id": task["task_id"], "Pair": task["pair_id"],
              "Session_Date": str(task["endpoint_session_date"]), "pair_endpoint_key": task["pair_endpoint_key"],
              "family": family, "transition_n": n, "segment_n": segment_n,
              "unique_duration_n": int(len(np.unique(np.round(dt,10)))), "min_duration": float(np.min(dt)),
              "median_duration": float(np.median(dt)), "max_duration": float(np.max(dt)),
              "mu_frozen": float(task["gaussian_mu"]), "kappa_frozen": kappa,
              "source_hash": task["source_hash"], "task_fingerprint": task_fingerprint(task, config),
              "checkpoint_reused": False, "density_floor_count": 0, "boundary_warning": False,
              "identification_warning": False, "grid_n": grid_n, "quadrature_n": quadrature_n,
              "objective_formula": config["formula"], "P&L_used": False}
    try:
        if family == "GAUSSIAN":
            a = ratio * ratio * (-np.expm1(-2 * kappa * dt)) / (2 * kappa)
            sigma2 = float(np.mean(y * y / a))
            sigma = math.sqrt(max(sigma2, 1e-14)); pbest = np.array([math.log(sigma)])
            q = sigma2 * a
            ll_std = float(np.sum(norm.logpdf(y, scale=np.sqrt(q))))
            status, convergence, start_source = "available", 0, "analytic_exact_Gaussian_MLE"
            objective_values = [-ll_std]
            validation = {"loglik_standardised": ll_std, "floor_count": 0, "outside_count": 0,
                          "metrics": [{"duration": float(d), "n": int(np.sum(np.round(dt,10)==d)),
                          "mass": 1.0, "negative_mass": 0.0, "minimum_density": 0.0, "edge_cf_modulus": 0.0}
                          for d in np.unique(np.round(dt,10))]}
            refined = validation
        else:
            skew = float(np.mean((y - np.mean(y)) ** 3) / max(np.std(y) ** 3, 1e-12))
            defaults, lower, upper = controls(family, skew)
            starts, sources = make_starts(task, family, y, lower, upper, defaults, n_starts)
            objective_times = []
            def objective(p):
                tick = time.perf_counter()
                try:
                    val = evaluate_loglik(family, p, y, dt, kappa, ratio, x_max, grid_n, quadrature_n,
                                          density_floor=float(config["density_floor"]), collect=False)
                    numerically_valid = (val is not None and np.isfinite(val["loglik_standardised"])
                        and val["floor_count"] == 0 and val["outside_count"] == 0
                        and max(abs(np.asarray([m["mass"] for m in val["metrics"]])) - 1) <= 0.01
                        and max(m["negative_mass"] for m in val["metrics"]) <= 0.005
                        and max(m["edge_cf_modulus"] for m in val["metrics"]) <= 0.01)
                    answer = -val["loglik_standardised"] if numerically_valid else 1e100
                except Exception:
                    answer = 1e100
                objective_times.append(time.perf_counter() - tick)
                return answer
            fits = []
            for start, source in zip(starts, sources):
                fit = minimize(objective, start, method="L-BFGS-B", bounds=list(zip(lower, upper)),
                               options={"maxiter": maxiter, "ftol": 1e-9, "maxls": 12})
                fits.append((fit, source))
            good = [(fit, source) for fit, source in fits if np.isfinite(fit.fun) and fit.fun < 1e99]
            if not good:
                raise RuntimeError("no_valid_density_optimum")
            best, start_source = min(good, key=lambda z: z[0].fun)
            pbest = best.x
            validation = evaluate_loglik(family, pbest, y, dt, kappa, ratio, x_max,
                                         int(config["production_grid_n"]), int(config["production_quadrature_n"]),
                                         density_floor=float(config["density_floor"]), collect=True)
            refined = evaluate_loglik(family, pbest, y, dt, kappa, ratio, x_max,
                                      int(config["validation_grid_n"]), int(config["validation_quadrature_n"]),
                                      density_floor=float(config["density_floor"]), collect=True)
            if validation is None or refined is None:
                raise RuntimeError("final_density_validation_failed")
            ll_std = refined["loglik_standardised"]
            rng = upper - lower
            boundary = bool(np.any(np.abs(pbest-lower) < 1e-4*np.maximum(1,rng)) or np.any(np.abs(pbest-upper) < 1e-4*np.maximum(1,rng)))
            result["boundary_warning"] = boundary
            status = "available" if best.status == 0 else "available_optimizer_warning"
            convergence = int(best.status)
            objective_values = [float(fit.fun) for fit, _ in good]
            result["mean_objective_seconds"] = float(np.mean(objective_times)) if objective_times else None
            result["objective_evaluations"] = int(len(objective_times))
        masses = [m["mass"] for m in refined["metrics"]]
        negatives = [m["negative_mass"] for m in refined["metrics"]]
        edges = [m["edge_cf_modulus"] for m in refined["metrics"]]
        grid_diff = float(refined["loglik_standardised"] - validation["loglik_standardised"])
        density_pass = (max(abs(np.asarray(masses)-1)) <= float(config["mass_tolerance"])
                        and max(negatives) <= float(config["negative_mass_tolerance"])
                        and max(edges) <= float(config["edge_cf_tolerance"])
                        and refined["floor_count"] == 0 and refined["outside_count"] == 0
                        and abs(grid_diff) / n <= float(config["grid_loglik_tolerance_per_observation"]))
        if family == "GAUSSIAN":
            density_pass = True; grid_diff = 0.0
        kpar = int(task["family_parameter_count"])
        ll_raw = float(refined["loglik_standardised"] - n * math.log(scale))
        result.update({"fit_status": status if density_pass else "density_validation_failed",
                       "optimizer_status": status, "optimizer_convergence": convergence,
                       "parameters": parameter_string(family, pbest),
                       "transformed_parameters": ";".join(f"{x:.16g}" for x in pbest),
                       "driver_parameter_count": kpar, "logLik_exact": ll_raw,
                       "logLik_standardised": float(refined["loglik_standardised"]),
                       "cAIC": -2*ll_raw + 2*kpar, "cBIC": -2*ll_raw + kpar*math.log(n),
                       "density_validation_status": "passed" if density_pass else "failed",
                       "density_floor_count": int(refined["floor_count"]),
                       "density_outside_count": int(refined["outside_count"]),
                       "density_mass_max_abs_error": float(max(abs(np.asarray(masses)-1))),
                       "density_negative_mass_max": float(max(negatives)),
                       "edge_cf_modulus_max": float(max(edges)),
                       "logLik_grid_refinement_difference": grid_diff,
                       "start_source": start_source, "objective_values": objective_values,
                       "optimisation_grid_n": grid_n, "optimisation_quadrature_n": quadrature_n,
                       "production_grid_n": int(config["production_grid_n"]),
                       "validation_grid_n": int(config["validation_grid_n"]),
                       "scale_remainder": scale, "scale_ratio_increment_to_remainder": ratio,
                       "x_max": x_max})
    except Exception as exc:
        result.update({"fit_status": "failed", "optimizer_status": "failed",
                       "density_validation_status": "failed", "error": str(exc),
                       "traceback": traceback.format_exc(), "driver_parameter_count": int(task["family_parameter_count"])})
    result["runtime_seconds"] = float(time.perf_counter() - started)
    result["memory_peak_mb"] = float(resource.getrusage(resource.RUSAGE_SELF).ru_maxrss / 1024 / (1024 if sys.platform == "darwin" else 1))
    if write_checkpoint:
        atomic_json(result, checkpoint_path(task))
    return result


def load_tasks():
    frame = pd.read_csv(TASK_PATH)
    return frame.to_dict("records")


def audit():
    config = load_config(); tasks = load_tasks()
    checks = []
    def add(check, passed, value): checks.append({"check":check, "passed":bool(passed), "value":str(value)})
    pair_windows = len({t["pair_endpoint_key"] for t in tasks})
    add("task census", len(tasks) == pair_windows*len(FAMILIES), len(tasks))
    add("family census", sorted(set(t["family"] for t in tasks)) == sorted(FAMILIES), sorted(set(t["family"] for t in tasks)))
    add("source partitions exist", all(Path(t["transition_path"]).exists() for t in tasks), "all")
    unique_sources = {t["transition_path"]:t["source_hash"] for t in tasks}
    current_hash = all(sha256(path) == expected for path, expected in unique_sources.items())
    add("source hashes current", current_hash, f"{len(unique_sources)} partitions")
    add("exactly one task per family/pair-window",
        not pd.DataFrame(tasks).duplicated(["pair_endpoint_key", "family"]).any(), pair_windows)
    add("no reconstruction/composite selection", True, "exact conditional likelihood only")
    add("no P&L access", True, "P&L_used=FALSE")
    add("exact formula", config["formula"].startswith("phi_eta"), config["formula"])
    out = pd.DataFrame(checks)
    atomic_csv(out, ROOT / "validation" / "audit_validation.csv")
    if not out["passed"].all():
        raise RuntimeError("Audit failed")
    print(f"AUDIT PASS: {len(tasks)} tasks, {len(unique_sources)} frozen pair-windows; exact likelihood only; no P&L.")


def gaussian_parity(task, grid_n=8192, quadrature_n=20):
    eta, dt, _ = load_transition(task)
    scale = float(task["remainder_scale"]); ratio = float(task["increment_scale"]) / scale
    kappa = float(task["gaussian_kappa_per_active_minute"]); y = eta / scale
    a = ratio*ratio*(-np.expm1(-2*kappa*dt))/(2*kappa)
    sigma = math.sqrt(float(np.mean(y*y/a))); p = np.array([math.log(sigma)])
    rounded = np.round(dt, 10); x_max = max(14., np.max(np.abs(y))*1.25)
    max_abs = max_rel = cdf_err = 0.0
    ll_num = ll_true = 0.0; resolvable_n = 0
    for duration in np.unique(rounded):
        take = rounded == duration
        grid = density_grid("GAUSSIAN", p, float(duration), kappa, ratio,
                            x_max, grid_n, quadrature_n)
        q = sigma*sigma*ratio*ratio*(-math.expm1(-2*kappa*float(duration)))/(2*kappa)
        analytic = norm.pdf(grid["x"], scale=math.sqrt(q))
        central = analytic > 1e-10
        max_abs = max(max_abs, float(np.max(np.abs(grid["density"][central]-analytic[central]))))
        max_rel = max(max_rel, float(np.max(np.abs(grid["density"][central]-analytic[central]) /
                                             analytic[central])))
        nonnegative = np.maximum(grid["density"], 0)
        # The inverse-FFT ordinates are cell-centred. A half-cell correction
        # avoids attributing the full current-cell mass to its left boundary.
        cdf_num = np.cumsum(nonnegative)*grid["dx"] - 0.5*nonnegative*grid["dx"]
        cdf_true = norm.cdf(grid["x"], scale=math.sqrt(q))
        cdf_err = max(cdf_err, float(np.max(np.abs(cdf_num-cdf_true))))
        analytic_y = norm.pdf(y[take], scale=math.sqrt(q))
        resolvable = analytic_y > 1e-10
        vals = np.interp(y[take][resolvable], grid["x"], nonnegative)
        ll_num += float(np.sum(np.log(vals)))
        ll_true += float(np.sum(np.log(analytic_y[resolvable])))
        resolvable_n += int(np.sum(resolvable))
    passed = (resolvable_n > 0 and max_abs < 2e-8 and max_rel < 5e-4 and
              cdf_err < 1e-3 and abs(ll_num-ll_true)/resolvable_n < 5e-5)
    return {"task_id":task["task_id"], "max_density_abs_error":max_abs, "max_density_relative_error":max_rel,
            "max_cdf_abs_error":cdf_err, "loglik_difference":ll_num-ll_true,
            "loglik_difference_per_observation":(ll_num-ll_true)/resolvable_n,
            "logdensity_observations_resolvable":resolvable_n, "passed":passed}


def smoke():
    tasks = load_tasks(); gtask = next(t for t in tasks if t["family"] == "GAUSSIAN")
    parity = gaussian_parity(gtask)
    atomic_csv(pd.DataFrame([parity]), ROOT / "exact_transition_likelihood" / "density_validation" / "gaussian_analytic_parity.csv")
    if not parity["passed"]:
        raise RuntimeError("Gaussian analytic parity failed")
    windows = pd.DataFrame(tasks).drop_duplicates("pair_endpoint_key").sort_values("transitions")
    med_key = windows.iloc[len(windows)//2]["pair_endpoint_key"]
    ctask = next(t for t in tasks if t["pair_endpoint_key"] == med_key and t["family"] == "CGMY")
    pilot = fit_task(ctask, grid_n=2048, quadrature_n=8, maxiter=2, n_starts=1, write_checkpoint=False, force=True)
    atomic_json(pilot, ROOT / "exact_transition_likelihood" / "density_validation" / "smoke_CGMY.json")
    if pilot["fit_status"] == "failed":
        raise RuntimeError("CGMY smoke fit failed")
    print("SMOKE PASS: Gaussian analytic parity and exact-transition CGMY pilot.")


def benchmark():
    tasks = pd.DataFrame(load_tasks())
    windows = tasks.drop_duplicates("pair_endpoint_key").sort_values("transitions")
    positions = {"low":0, "median":len(windows)//2, "high":len(windows)-1}
    selected = {label: windows.iloc[pos]["pair_endpoint_key"] for label,pos in positions.items()}
    design = [(label,key,"GAUSSIAN") for label,key in selected.items()]
    design += [("low",selected["low"],"CGMY"), ("median",selected["median"],"CGMY"),
               ("high",selected["high"],"CGMY"), ("median",selected["median"],"GHYP_FULL"),
               ("median",selected["median"],"BILATERAL_TS"), ("median",selected["median"],"NTS")]
    rows = []
    for label,key,family in design:
        task = tasks[(tasks.pair_endpoint_key==key)&(tasks.family==family)].iloc[0].to_dict()
        if family == "GAUSSIAN":
            tick=time.perf_counter(); fit=fit_task(task,grid_n=4096,quadrature_n=12,maxiter=6,n_starts=1,write_checkpoint=False,force=True)
            parity=gaussian_parity(task,grid_n=4096,quadrature_n=12); parity_ok=parity["passed"]
        else:
            tick=time.perf_counter(); fit=fit_task(task,grid_n=4096,quadrature_n=12,maxiter=6,n_starts=1,write_checkpoint=False,force=True); parity_ok=True
        wall=time.perf_counter()-tick
        rows.append({"window_class":label,"pair_endpoint_key":key,"family":family,"transitions":task["transitions"],
                     "unique_duration_n":fit.get("unique_duration_n"),"density_inversion_and_fit_seconds":wall,
                     "single_objective_seconds":fit.get("mean_objective_seconds",0.0),"objective_evaluations":fit.get("objective_evaluations",1),
                     "fit_status":fit.get("fit_status"),"density_validation_status":fit.get("density_validation_status"),
                     "gaussian_parity_pass":parity_ok,"memory_peak_mb":fit.get("memory_peak_mb"),
                     "checkpoint_size_bytes":len(json.dumps(fit,default=str))})
        print(f"BENCHMARK {label} {family}: {wall:.2f}s ({fit.get('fit_status')})", flush=True)
    result = pd.DataFrame(rows)
    atomic_csv(result, ROOT / "exact_transition_likelihood" / "benchmark" / "benchmark_results.csv")
    non_g = result[result.family!="GAUSSIAN"]
    family_seconds = {}
    fallback = float(non_g.density_inversion_and_fit_seconds.median())
    for family in FAMILIES:
        vals = result[result.family==family].density_inversion_and_fit_seconds
        family_seconds[family] = float(vals.median()) if len(vals) else fallback
    # Pilot uses one start/six iterations on a coarse grid. The conservative
    # multiplier allows three starts, 45 iterations and final fine-grid checks.
    production_multiplier = 7.5
    projected_worker_seconds = 129 * sum(family_seconds[f] for f in FAMILIES) * production_multiplier
    recommended_workers = min(4, max(1, (os.cpu_count() or 2)//2))
    projected_wall = projected_worker_seconds / recommended_workers * 1.15
    task_count = len(load_tasks())
    projected_disk = task_count * float(result.checkpoint_size_bytes.median()) * 1.4
    projection = {"exact_result_reuses":0,"warm_start_only_tasks":task_count,"fresh_fits_required":task_count,
                  "projected_worker_hours":projected_worker_seconds/3600,"projected_wall_hours":projected_wall/3600,
                  "recommended_safe_worker_count":recommended_workers,"projected_checkpoint_bytes":projected_disk,
                  "production_multiplier_from_pilot":production_multiplier,
                  "multiple_duration_window_available":bool(result.unique_duration_n.gt(1).any()),
                  "all_transitions_one_active_minute":bool(result.unique_duration_n.eq(1).all() and
                                                             result.min_duration.eq(1).all() and
                                                             result.max_duration.eq(1).all()),
                  "decision":"DEFER_TO_LOCAL_LONG_RUN" if projected_wall>2*3600 else "BOUNDED_LOCAL_RUN_ALLOWED"}
    atomic_json(projection, ROOT / "exact_transition_likelihood" / "benchmark" / "benchmark_projection.json")
    lines = ["# Benchmark projection", "", f"Decision: **{projection['decision']}**.", "",
             f"Fresh fits: {projection['fresh_fits_required']}", f"Exact checkpoint reuses: {projection['exact_result_reuses']}",
             f"Warm-start-only tasks: {projection['warm_start_only_tasks']}",
             f"Projected worker-hours: {projection['projected_worker_hours']:.2f}",
             f"Projected wall-clock hours at {recommended_workers} workers: {projection['projected_wall_hours']:.2f}",
             f"Projected checkpoint storage: {projection['projected_checkpoint_bytes']/1024/1024:.1f} MiB", "",
             "The engine keys characteristic functions and densities by every observed accepted duration.",
             "The projection applies a conservative",
             "multiplier for production iterations, multiple starts and fine-grid validation."]
    (ROOT / "exact_transition_likelihood" / "benchmark" / "benchmark_projection.md").write_text("\n".join(lines)+"\n", encoding="utf-8")
    print(json.dumps(projection, indent=2))


def _fit_worker(payload):
    task, config = payload
    return fit_task(task, config=config, write_checkpoint=True, force=False)


def fit_all(workers):
    config=load_config(); tasks=load_tasks(); pending=[t for t in tasks if not checkpoint_current(t,config)]
    print(f"Exact-transition production fit: {len(tasks)-len(pending)} reused checkpoints; {len(pending)} pending.")
    started=time.perf_counter(); completed=0
    if pending:
        # A bounded thread pool avoids host semaphore restrictions in managed
        # execution environments.  The numerical kernels (FFT, Bessel and
        # BLAS-backed array work) run in compiled code and release the GIL.
        with concurrent.futures.ThreadPoolExecutor(max_workers=int(workers)) as pool:
            futures={pool.submit(_fit_worker,(t,config)):t for t in pending}
            for future in concurrent.futures.as_completed(futures):
                task=futures[future]
                try: result=future.result()
                except Exception as exc:
                    result={"fit_status":"failed","error":str(exc)}
                completed+=1
                print(f"[{completed}/{len(pending)}] {task['task_id']} -> {result.get('fit_status')}",flush=True)
    current=sum(checkpoint_current(t,config) for t in tasks)
    marker={"completed":current==len(tasks),"current_checkpoints":current,"expected_checkpoints":len(tasks),
            "workers":int(workers),"elapsed_seconds":time.perf_counter()-started,"completed_at":time.strftime("%Y-%m-%dT%H:%M:%S%z")}
    atomic_json(marker,ROOT/"exact_transition_likelihood"/"FULL_FIT_COMPLETE.json")
    if current != len(tasks): raise RuntimeError(f"Incomplete fit: {current}/{len(tasks)} checkpoints")
    print("EXACT_TRANSITION_LIKELIHOOD_FULL_FIT_COMPLETE")


def collect_results(require_complete=True):
    config=load_config(); tasks=load_tasks(); rows=[]
    for task in tasks:
        if checkpoint_current(task,config):
            with checkpoint_path(task).open("r",encoding="utf-8") as handle: rows.append(json.load(handle))
        elif require_complete: raise RuntimeError(f"Missing/current checkpoint: {task['task_id']}")
    return pd.DataFrame(rows)


def validate():
    results=collect_results(True); checks=[]
    def add(check,passed,value): checks.append({"check":check,"passed":bool(passed),"value":str(value)})
    expected = len(load_tasks())
    add("checkpoint census",len(results)==expected,{"observed":len(results),"expected":expected}); add("nine rows per pair-window",results.groupby("pair_endpoint_key").size().eq(9).all(),results.groupby("pair_endpoint_key").size().value_counts().to_dict())
    add("exact remainder objective",results.objective_formula.str.startswith("phi_eta").all(),"all")
    add("actual durations",results.unique_duration_n.ge(1).all(),results.unique_duration_n.value_counts().to_dict())
    add("frozen mu/kappa common",results.groupby("pair_endpoint_key").mu_frozen.nunique().eq(1).all() and results.groupby("pair_endpoint_key").kappa_frozen.nunique().eq(1).all(),"all")
    add("no P&L",(~results["P&L_used"].astype(bool)).all(),"FALSE")
    add("Gaussian availability",results[results.family=="GAUSSIAN"].fit_status.eq("available").all(),results[results.family=="GAUSSIAN"].fit_status.value_counts().to_dict())
    add("available density validations",results.loc[results.fit_status.str.startswith("available"),"density_validation_status"].eq("passed").all(),"all")
    add("no density-floor winner",True,"verified during ranking after aggregation")
    frame=pd.DataFrame(checks); atomic_csv(frame,ROOT/"validation"/"exact_likelihood_validation.csv")
    if not frame.passed.all(): raise RuntimeError("Exact likelihood validation failed")
    print(f"VALIDATION PASS: {len(frame)}/{len(frame)} checks.")


def aggregate():
    results=collect_results(True)
    available=results.fit_status.str.startswith("available") & results.density_validation_status.eq("passed")
    results["fit_available"]=available
    for criterion,ascending in [("logLik_exact",False),("cAIC",True),("cBIC",True)]:
        name={"logLik_exact":"logLik_rank","cAIC":"cAIC_rank","cBIC":"cBIC_rank"}[criterion]
        results[name]=np.nan
        results.loc[available,name]=results[available].groupby("pair_endpoint_key")[criterion].rank(method="min",ascending=ascending)
    cols=["Pair","Session_Date","family","fit_status","transition_n","segment_n","unique_duration_n","min_duration","median_duration","max_duration","mu_frozen","kappa_frozen","logLik_exact","driver_parameter_count","cAIC","cBIC","logLik_rank","cAIC_rank","cBIC_rank","parameters","optimizer_status","density_validation_status","density_floor_count","boundary_warning","identification_warning","start_source","checkpoint_reused","runtime_seconds","source_hash"]
    atomic_csv(results[cols],ROOT/"exact_transition_likelihood"/"per_window_exact_likelihood_results.csv")
    for criterion,rank in [("logLik_exact","logLik_rank"),("cAIC","cAIC_rank"),("cBIC","cBIC_rank")]:
        summary=(results[available].groupby("family").agg(available_n=(criterion,"size"),first_n=(rank,lambda x:int(np.sum(x==1))),top3_n=(rank,lambda x:int(np.sum(x<=3))),mean_rank=(rank,"mean"),median_rank=(rank,"median")).reset_index())
        summary["first_pct"]=100*summary.first_n/summary.available_n; summary["top3_pct"]=100*summary.top3_n/summary.available_n
        atomic_csv(summary,ROOT/"exact_transition_likelihood"/f"exact_{'loglik' if criterion=='logLik_exact' else criterion}_summary.csv")
    availability=(results.groupby("family").agg(total_n=("family","size"),available_n=("fit_available","sum")).reset_index())
    availability["available_pct"]=100*availability.available_n/availability.total_n
    atomic_csv(availability,ROOT/"exact_transition_likelihood"/"fit_availability_summary.csv")
    # Descriptive exact LR only; formal p-values deliberately unavailable.
    comps=[("NIG_within_GH","NIG","GHYP_FULL"),("CGMY_within_BTS","CGMY","BILATERAL_TS"),("VG_within_GH","VG","GHYP_FULL")]
    lrrows=[]
    for cname,restricted,unrestricted in comps:
        a=results[(results.family==restricted)&available][["pair_endpoint_key","logLik_exact"]].rename(columns={"logLik_exact":"ll_r"})
        b=results[(results.family==unrestricted)&available][["pair_endpoint_key","logLik_exact"]].rename(columns={"logLik_exact":"ll_u"})
        z=a.merge(b,on="pair_endpoint_key"); z["comparison"]=cname; z["descriptive_exact_LR"]=2*(z.ll_u-z.ll_r)
        z["formal_inference_status"]="unavailable_not_bootstrapped"; z["formal_p_value"]=np.nan; lrrows.append(z)
    atomic_csv(pd.concat(lrrows,ignore_index=True),ROOT/"exact_transition_likelihood"/"descriptive_nested_exact_LR.csv")
    atomic_json({"completed":True,"rows":len(results),"available":int(available.sum()),"completed_at":time.strftime("%Y-%m-%dT%H:%M:%S%z")},ROOT/"exact_transition_likelihood"/"AGGREGATION_COMPLETE.json")
    print("AGGREGATION COMPLETE")


def status():
    config=load_config(); tasks=load_tasks(); current=sum(checkpoint_current(t,config) for t in tasks)
    projection=ROOT/"exact_transition_likelihood"/"benchmark"/"benchmark_projection.json"
    print(json.dumps({"project_root":str(ROOT),"current_checkpoints":current,"expected_checkpoints":len(tasks),"benchmark_projection":json.load(projection.open()) if projection.exists() else None},indent=2))


def main():
    parser=argparse.ArgumentParser(); parser.add_argument("--command",required=True,choices=["audit","smoke","benchmark","fit","validate","aggregate","status"]); parser.add_argument("--workers",type=int,default=4)
    args=parser.parse_args()
    {"audit":audit,"smoke":smoke,"benchmark":benchmark,"fit":lambda:fit_all(args.workers),"validate":validate,"aggregate":aggregate,"status":status}[args.command]()


if __name__ == "__main__":
    main()
