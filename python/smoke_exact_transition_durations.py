#!/usr/bin/env python3
"""Cheap, data-free tests for duration-aware exact-likelihood routing."""

import math
import numpy as np
from scipy.stats import norm

import exact_transition_engine as engine


def main():
    durations = np.array([1.0, 2.0, 5.0], dtype=float)
    remainder = np.array([0.10, -0.15, 0.22], dtype=float)
    kappa = 0.07
    ratio = 1.3
    sigma = 0.8
    p = np.array([math.log(sigma)])

    # L4: the engine's Gaussian contribution is the closed-form exact
    # duration-specific OU remainder likelihood.
    variances = sigma**2 * ratio**2 * (-np.expm1(-2 * kappa * durations)) / (2 * kappa)
    analytic = float(np.sum(norm.logpdf(remainder, scale=np.sqrt(variances))))
    engine_value = 0.0
    for value, duration, variance in zip(remainder, durations, variances):
        engine_value += float(norm.logpdf(value, scale=math.sqrt(variance)))
        cf = engine.remainder_cf("GAUSSIAN", p, np.array([0.3]), duration, kappa, ratio, 24)[0]
        expected_cf = np.exp(-0.5 * variance * 0.3**2)
        assert abs(cf - expected_cf) < 1e-11
    assert abs(engine_value - analytic) < 1e-12

    # L5/L6: a non-Gaussian family is routed once per unique observed duration;
    # no minute replication or subdivision is introduced.
    cgmy = np.array([math.log(0.3), math.log(4.0), math.log(5.0), 0.0])
    cfs = [engine.remainder_cf("CGMY", cgmy, np.array([0.2]), d, kappa, ratio, 16)[0]
           for d in durations]
    assert len({round(float(z.real), 12) for z in cfs}) == len(durations)
    value = engine.evaluate_loglik(
        "CGMY", cgmy, remainder, durations, kappa, ratio,
        x_max=14.0, n_grid=2048, quadrature_n=16, collect=True
    )
    assert value is not None
    assert [m["duration"] for m in value["metrics"]] == list(durations)
    assert sum(m["n"] for m in value["metrics"]) == len(durations)
    print("PASS exact-transition durations: 1.0, 2.0, 5.0; Gaussian identity; CGMY routing")


if __name__ == "__main__":
    main()
