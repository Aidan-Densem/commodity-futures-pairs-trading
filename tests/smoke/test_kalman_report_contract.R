# K1: unequal active-time increments enter process covariance once.
y <- c(1, 1.1, .9, 1.05); x <- c(.2, .3, .25, .35); q <- .01; ve <- .1
unequal <- kalman_affine_filter(y, x, c(1, 1, 5, 1), q, ve)
unit <- kalman_affine_filter(y, x, c(1, 1, 1, 1), q, ve)
smoke_equal(unequal$predicted_covariance_path[, , 3] -
              unit$predicted_covariance_path[, , 3], diag(4 * q, 2),
            tolerance = 1e-12, message = "K1: q*Delta covariance failed")

# K2: equal spacing agrees with the direct fixed-step recursion.
manual <- function(y, x, q, ve) {
  state <- c(0, 1); covariance <- diag(2); path <- matrix(NA_real_, length(y), 2)
  for (i in seq_along(y)) {
    h <- c(1, x[[i]]); pp <- covariance + diag(q, 2)
    e <- y[[i]] - sum(h * state); f <- drop(h %*% pp %*% h) + ve
    k <- drop(pp %*% h) / f; state <- state + k * e
    covariance <- (pp - tcrossprod(k, h) %*% pp); covariance <- (covariance + t(covariance)) / 2
    path[i, ] <- state
  }
  path
}
fixed <- kalman_affine_filter(y, x, rep(1, length(y)), q, ve)
smoke_equal(fixed$state_path, manual(y, x, q, ve), tolerance = 1e-12,
            message = "K2: fixed-step reduction failed")

# K3--K6: deterministic log-parameter MLE, refilter, boundary state and freeze.
set.seed(4); n <- 150L; sx <- stats::rnorm(n)
theta <- matrix(NA_real_, n, 2); theta[1, ] <- c(.2, 1.1)
for (i in 2:n) theta[i, ] <- theta[i - 1, ] + stats::rnorm(2, 0, sqrt(1e-3))
sy <- theta[, 1] + theta[, 2] * sx + stats::rnorm(n, 0, sqrt(1e-2))
dt <- rep(1, n)
diagnostic <- fit_formation_kalman_parameters(sy, sx, dt)
smoke_expect(diagnostic$q > 0 && diagnostic$ve > 0 && is.finite(diagnostic$loglik),
             "K3: positive log-parameter MLE failed")
smoke_expect(identical(diagnostic$parameterization, "log(q), log(v_e)"),
             "K3: optimisation is not documented in log parameters")
refilter <- kalman_affine_filter(sy, sx, dt, diagnostic$q, diagnostic$ve)
smoke_equal(refilter$loglik, diagnostic$refilter$loglik, message = "K4: refilter loglik mismatch")
smoke_equal(refilter$state_path, diagnostic$refilter$state_path, message = "K4: refilter path mismatch")
smoke_equal(refilter$final_state, tail(refilter$state_path, 1L),
            message = "K5: final state is not the post-update state")

formation <- data.frame(midpoint_y = exp(sy), midpoint_x = exp(sx), active_dt_minutes = dt,
                        close_y = 1, close_x = 1)
public <- estimate_formation_kalman_hedge(formation, diagnostic$q, diagnostic$ve)
smoke_equal(public$frozen$alpha, public$final_state[["alpha"]], message = "K6: alpha not frozen")
smoke_equal(public$frozen$beta, public$final_state[["beta"]], message = "K6: beta not frozen")
smoke_equal(mean(public$frozen$spread), 0, tolerance = 1e-12, message = "K6: spread not centred")

# K7: trading observations cannot update the frozen coordinate.
trading <- formation[1:4, ]; base <- apply_frozen_spread(trading, public$frozen)
trading$midpoint_y <- trading$midpoint_y * 2
changed <- apply_frozen_spread(trading, public$frozen)
smoke_expect(!isTRUE(all.equal(base, changed)), "K7: trading data did not affect trading spread")
smoke_equal(public$frozen$alpha, public$final_state[["alpha"]], message = "K7: frozen alpha changed")

# K8: Close is immaterial.
formation$close_y <- 1e100; formation$close_x <- -1e100
close_changed <- estimate_formation_kalman_hedge(formation, diagnostic$q, diagnostic$ve)
smoke_equal(close_changed$state_path, public$state_path, message = "K8: Close changed Kalman input")

# K9: exact public object contract.
smoke_expect(identical(names(public), c("final_state", "final_covariance", "state_path", "loglik",
                                       "frozen", "formation_rows")), "K9: public top-level schema changed")
smoke_expect(identical(names(public$frozen), c("alpha", "beta", "centre", "spread")),
             "K9: frozen schema changed")
smoke_expect(is.numeric(public$final_state) && identical(dim(public$final_covariance), c(2L, 2L)) &&
               identical(dim(public$state_path), c(n, 2L)) && is.numeric(public$loglik) &&
               is.integer(public$formation_rows), "K9: public types/dimensions changed")
cat("KALMAN_REPORT_CONTRACT_PASS K1-K9\n")
