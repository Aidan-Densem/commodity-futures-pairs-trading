gh_continuous_sqrt <- function(z) {
  value <- sqrt(as.complex(z))
  flip <- Re(value) < 0 | (abs(Re(value)) < 1e-15 & Im(value) < 0)
  value[flip] <- -value[flip]
  value
}

gh_unwrap_from_anchor <- function(phases, anchor = 0) {
  phases <- as.numeric(phases)
  if (!length(phases)) return(phases)
  output <- phases
  previous <- as.numeric(anchor)
  for (index in seq_along(phases)) {
    output[[index]] <- phases[[index]] + 2 * pi * round(
      (previous - phases[[index]]) / (2 * pi)
    )
    previous <- output[[index]]
  }
  output
}

gh_unwrap_log_cf_path <- function(u, value) {
  u <- as.complex(u)
  value <- as.complex(value)
  ou_gh_assert(length(u) == length(value),
    "Frequency and log-CF vectors must have equal length.")
  if (length(value) < 2L) {
    value[Mod(u) == 0] <- 0 + 0i
    return(value)
  }
  output <- value
  real_axis <- max(abs(Im(u))) <= 1e-14
  if (real_axis) {
    zero <- which(abs(Re(u)) <= 1e-15)
    negative <- which(Re(u) < -1e-15)
    positive <- which(Re(u) > 1e-15)
    if (length(negative)) {
      path <- negative[order(abs(Re(u[negative])), Re(u[negative]))]
      output[path] <- Re(value[path]) + 1i * gh_unwrap_from_anchor(
        Im(value[path]), 0
      )
    }
    if (length(positive)) {
      path <- positive[order(abs(Re(u[positive])), Re(u[positive]))]
      output[path] <- Re(value[path]) + 1i * gh_unwrap_from_anchor(
        Im(value[path]), 0
      )
    }
    if (length(zero)) output[zero] <- 0 + 0i
    return(output)
  }
  same_contour <- max(Im(u)) - min(Im(u)) <=
    1e-12 * max(1, max(abs(Im(u))))
  if (same_contour) {
    ordering <- order(Re(u), Im(u))
  } else {
    ordering <- order(Mod(u), Arg(u), Re(u), Im(u))
  }
  ordered_u <- u[ordering]
  ordered_value <- value[ordering]
  pivot <- which.min(abs(Re(ordered_u)))
  ordered_output <- ordered_value
  if (pivot < length(ordered_value)) {
    right <- seq.int(pivot + 1L, length(ordered_value))
    ordered_output[right] <- Re(ordered_value[right]) + 1i *
      gh_unwrap_from_anchor(Im(ordered_value[right]), Im(ordered_output[[pivot]]))
  }
  if (pivot > 1L) {
    left <- seq.int(pivot - 1L, 1L)
    ordered_output[left] <- Re(ordered_value[left]) + 1i *
      gh_unwrap_from_anchor(Im(ordered_value[left]), Im(ordered_output[[pivot]]))
  }
  output[ordering] <- ordered_output
  output[Mod(u) == 0] <- 0 + 0i
  output
}

gh_log_cf <- function(
    u, lambda, alpha, beta, delta, m = NULL,
    centred = is.null(m), unwrap = TRUE, small_u_threshold = NULL
) {
  if (is.null(m)) m <- gh_centred_location(lambda, alpha, beta, delta)
  gh_validate_direct_parameters(lambda, alpha, beta, delta, m)
  u <- as.complex(u)
  gamma <- sqrt(alpha^2 - beta^2)
  q <- gh_continuous_sqrt(alpha^2 - (beta + 1i * u)^2)
  log_bessel_numerator <- gh_log_bessel_k(lambda, delta * q, unwrap = FALSE)
  log_bessel_denominator <- gh_log_bessel_k(
    lambda, delta * gamma, unwrap = FALSE
  )[[1L]]
  output <- 1i * m * u + lambda * (log(gamma) - log(q)) +
    log_bessel_numerator - log_bessel_denominator
  if (isTRUE(unwrap)) output <- gh_unwrap_log_cf_path(u, output)
  zero <- Mod(u) == 0
  output[zero] <- 0 + 0i
  threshold <- small_u_threshold %||%
    if (exists("OU_GH_NUMERICAL_CONTRACT")) {
      OU_GH_NUMERICAL_CONTRACT$small_u_threshold
    } else 1e-5
  if (isTRUE(centred) && any(Mod(u) > 0 & Mod(u) < threshold)) {
    cumulants <- gh_driver_cumulants_direct(
      lambda, alpha, beta, delta, maximum_order = 4L
    )
    small <- Mod(u) > 0 & Mod(u) < threshold
    us <- u[small]
    output[small] <- cumulants[[2L]] * (1i * us)^2 / factorial(2) +
      cumulants[[3L]] * (1i * us)^3 / factorial(3) +
      cumulants[[4L]] * (1i * us)^4 / factorial(4)
  }
  output
}

gh_driver_log_exponent <- function(u, parameters, unwrap = TRUE) {
  required <- c("lambda", "alpha", "beta", "delta")
  ou_gh_assert(all(required %in% names(parameters)),
    "GH parameters lack direct driver coordinates.")
  gh_log_cf(
    u,
    parameters[["lambda"]], parameters[["alpha"]],
    parameters[["beta"]], parameters[["delta"]],
    m = NULL, centred = TRUE, unwrap = unwrap
  )
}

gh_nig_log_exponent <- function(u, alpha, beta, delta) {
  gamma <- sqrt(alpha^2 - beta^2)
  m0 <- -delta * beta / gamma
  q <- gh_continuous_sqrt(alpha^2 - (beta + 1i * as.complex(u))^2)
  output <- 1i * m0 * u + delta * (gamma - q)
  output[Mod(u) == 0] <- 0 + 0i
  output
}

gh_vg_log_exponent <- function(u, lambda, alpha, beta) {
  ou_gh_assert(lambda > 0 && alpha > abs(beta),
    "VG boundary requires lambda > 0 and alpha > abs(beta).")
  gamma <- sqrt(alpha^2 - beta^2)
  m0 <- -2 * lambda * beta / gamma^2
  q <- gh_continuous_sqrt(alpha^2 - (beta + 1i * as.complex(u))^2)
  output <- 1i * m0 * u + 2 * lambda * (log(gamma) - log(q))
  output <- gh_unwrap_log_cf_path(u, output)
  output[Mod(u) == 0] <- 0 + 0i
  output
}

gh_exponent_identity_diagnostics <- function(
    parameters, frequencies = c(0, 0.1, 0.5, 1, 2, 4),
    derivative_step = 1e-5
) {
  positive <- gh_driver_log_exponent(frequencies, parameters)
  negative <- gh_driver_log_exponent(-frequencies, parameters)
  derivative <- (
    gh_driver_log_exponent(derivative_step, parameters) -
      gh_driver_log_exponent(-derivative_step, parameters)
  ) / (2 * derivative_step)
  data.frame(
    psi_zero_modulus = Mod(gh_driver_log_exponent(0, parameters)),
    derivative_zero_modulus = Mod(derivative),
    max_conjugacy_error = max(Mod(negative - Conj(positive))),
    max_positive_real_part = max(pmax(Re(positive), 0)),
    stringsAsFactors = FALSE
  )
}

ou_gh_reject_direct_gh_as_remainder <- function(method) {
  if (identical(method, "direct_GH_draw")) {
    stop(
      "A direct GH variable is not an OU_GH_DRIVER remainder; use the exact remainder CF.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}
