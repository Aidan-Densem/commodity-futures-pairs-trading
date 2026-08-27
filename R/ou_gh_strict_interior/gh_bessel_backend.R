gh_unwrap_imaginary <- function(value) {
  value <- as.numeric(value)
  if (length(value) < 2L) return(value)
  output <- value
  for (index in 2:length(value)) {
    output[[index]] <- value[[index]] + 2 * pi * round(
      (output[[index - 1L]] - value[[index]]) / (2 * pi)
    )
  }
  output
}

gh_bessel_backend_info <- function() {
  ou_gh_use_vendor_library()
  ou_gh_assert(requireNamespace("Bessel", quietly = TRUE),
    "The pinned Bessel package is unavailable.")
  data.frame(
    backend = "Bessel::BesselK",
    package_version = as.character(utils::packageVersion("Bessel")),
    algorithm = "AMOS/TOMS 644 C translation",
    complex_arguments = TRUE,
    exponential_scaling = "exp(z) * K_nu(z)",
    licence = "GPL-2 | GPL-3",
    stringsAsFactors = FALSE
  )
}

gh_bessel_k <- function(order, z, scaled = FALSE) {
  ou_gh_use_vendor_library()
  ou_gh_assert(requireNamespace("Bessel", quietly = TRUE),
    "Install the pinned Bessel package before GH evaluation.")
  ou_gh_assert(length(order) == 1L && is.finite(order),
    "Bessel order must be one finite scalar.")
  z <- as.complex(z)
  ou_gh_assert(all(is.finite(Re(z))) && all(is.finite(Im(z))),
    "Bessel arguments must be finite.")
  Bessel::BesselK(z, nu = abs(order), expon.scaled = isTRUE(scaled))
}

gh_log_bessel_k <- function(order, z, unwrap = TRUE) {
  z <- as.complex(z)
  scaled <- gh_bessel_k(order, z, scaled = TRUE)
  output <- log(as.complex(scaled)) - z
  if (isTRUE(unwrap) && length(output) > 1L) {
    output <- Re(output) + 1i * gh_unwrap_imaginary(Im(output))
  }
  output
}

gh_log_bessel_ratio <- function(lambda, shift, z) {
  ou_gh_assert(
    length(lambda) == 1L && is.finite(lambda) &&
      length(shift) == 1L && is.finite(shift) &&
      all(is.finite(z)) && all(z > 0),
    "Real positive arguments and finite orders are required for Bessel ratios."
  )
  numerator <- besselK(z, nu = abs(lambda + shift), expon.scaled = TRUE)
  denominator <- besselK(z, nu = abs(lambda), expon.scaled = TRUE)
  log(numerator) - log(denominator)
}

gh_bessel_ratio_terms <- function(lambda, zeta, minimum_relative_gap = 1e-10) {
  ou_gh_assert(
    length(lambda) == 1L && is.finite(lambda) &&
      length(zeta) == 1L && is.finite(zeta) && zeta > 0,
    "lambda and positive zeta must be finite scalars."
  )
  log_R1 <- gh_log_bessel_ratio(lambda, 1, zeta)
  log_R2 <- gh_log_bessel_ratio(lambda, 2, zeta)
  log_fraction <- 2 * log_R1 - log_R2
  if (log_fraction > 0 && log_fraction <= 64 * .Machine$double.eps) {
    log_fraction <- 0
  }
  ou_gh_assert(log_fraction <= 0,
    "Bessel ratios violate the positive GIG variance identity.")
  relative_gap <- -expm1(log_fraction)
  ou_gh_assert(is.finite(relative_gap) && relative_gap > minimum_relative_gap,
    paste0(
      "Bessel-ratio loss of significance: relative GIG variance gap ",
      format(relative_gap, digits = 8), " is below the supported threshold."
    ))
  R1 <- exp(log_R1)
  R2 <- exp(log_R2)
  D <- R2 * relative_gap
  ou_gh_assert(all(is.finite(c(R1, R2, D))) && D > 0,
    "Bessel-ratio moments are nonfinite or nonpositive.")
  c(
    R1 = R1,
    R2 = R2,
    D = D,
    log_R1 = log_R1,
    log_R2 = log_R2,
    relative_gap = relative_gap
  )
}
