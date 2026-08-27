gig_validate_parameters <- function(lambda, chi, psi) {
  reasons <- character()
  if (length(lambda) != 1L || !is.finite(lambda)) {
    reasons <- c(reasons, "lambda_not_finite_scalar")
  }
  if (length(chi) != 1L || !is.finite(chi) || chi <= 0) {
    reasons <- c(reasons, "chi_not_positive")
  }
  if (length(psi) != 1L || !is.finite(psi) || psi <= 0) {
    reasons <- c(reasons, "psi_not_positive")
  }
  if (length(reasons)) {
    stop("Invalid strict-interior GIG parameters: ",
      paste(reasons, collapse = ";"), call. = FALSE)
  }
  invisible(TRUE)
}

gig_raw_moments <- function(lambda, chi, psi, orders = 1:4) {
  gig_validate_parameters(lambda, chi, psi)
  orders <- as.numeric(orders)
  ou_gh_assert(all(is.finite(orders)), "GIG moment orders must be finite.")
  delta <- sqrt(chi)
  gamma <- sqrt(psi)
  zeta <- delta * gamma
  values <- vapply(orders, function(order) {
    exp(order * log(delta / gamma) +
      gh_log_bessel_ratio(lambda, order, zeta))
  }, numeric(1L))
  names(values) <- paste0("raw", orders)
  values
}

gig_cumulants <- function(lambda, chi, psi, maximum_order = 4L) {
  maximum_order <- as.integer(maximum_order)
  ou_gh_assert(maximum_order >= 1L && maximum_order <= 4L,
    "The frozen GIG cumulant implementation supports orders 1 through 4.")
  raw <- gig_raw_moments(lambda, chi, psi, 1:4)
  m1 <- raw[[1L]]
  m2 <- raw[[2L]]
  m3 <- raw[[3L]]
  m4 <- raw[[4L]]
  values <- c(
    m1,
    m2 - m1^2,
    m3 - 3 * m2 * m1 + 2 * m1^3,
    m4 - 4 * m3 * m1 - 3 * m2^2 + 12 * m2 * m1^2 - 6 * m1^4
  )
  values <- values[seq_len(maximum_order)]
  names(values) <- paste0("kappa", seq_along(values))
  values
}

gig_draw_gigrvg <- function(n, lambda, chi, psi, seed = NULL) {
  gig_validate_parameters(lambda, chi, psi)
  ou_gh_use_vendor_library()
  ou_gh_assert(requireNamespace("GIGrvg", quietly = TRUE),
    "The pinned GIGrvg reference generator is unavailable.")
  if (!is.null(seed)) set.seed(as.integer(seed)[[1L]])
  GIGrvg::rgig(as.integer(n), lambda = lambda, chi = chi, psi = psi)
}

gig_draw_generalized_hyperbolic <- function(
    n, lambda, chi, psi, seed = NULL
) {
  gig_validate_parameters(lambda, chi, psi)
  ou_gh_assert(requireNamespace("GeneralizedHyperbolic", quietly = TRUE),
    "GeneralizedHyperbolic is unavailable.")
  if (!is.null(seed)) set.seed(as.integer(seed)[[1L]])
  GeneralizedHyperbolic::rgig(
    as.integer(n), param = c(chi = chi, psi = psi, lambda = lambda)
  )
}
