set.seed(44); n <- 80L; dt <- c(0, rep(c(1, 2, 5, 1), length.out = n - 1L))
kappa <- .03; mu <- .2; sigma <- .04; z <- numeric(n); z[[1L]] <- mu
for (i in 2:n) {
  phi <- exp(-kappa * dt[[i]])
  z[[i]] <- mu + phi * (z[[i - 1L]] - mu) +
    sigma * sqrt(-expm1(-2 * kappa * dt[[i]]) / (2 * kappa)) * stats::rnorm(1)
}
fit <- estimate_exact_gaussian_ou(z, dt, c(FALSE, rep(TRUE, n - 1L)), minimum_transitions = 30L)
smoke_expect(isTRUE(fit$exact_irregular_transition) && fit$kappa_per_active_minute > 0 &&
               fit$gaussian_diffusion_scale > 0 && is.finite(fit$loglik),
             "Exact irregular Gaussian OU interface failed")
cat("EXACT_GAUSSIAN_OU_PASS\n")
