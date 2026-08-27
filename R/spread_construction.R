statistical_raw_spread <- function(midpoint_y, midpoint_x, alpha, beta) {
  valid <- is.finite(midpoint_y) & is.finite(midpoint_x) &
    midpoint_y > 0 & midpoint_x > 0
  out <- rep(NA_real_, length(midpoint_y))
  out[valid] <- log(midpoint_y[valid]) - alpha - beta * log(midpoint_x[valid])
  out
}

freeze_formation_spread <- function(formation, alpha, beta) {
  raw <- statistical_raw_spread(
    formation$midpoint_y, formation$midpoint_x, alpha, beta
  )
  centre <- mean(raw, na.rm = TRUE)
  if (!is.finite(centre)) stop("Formation centre is not finite.", call. = FALSE)
  list(alpha = alpha, beta = beta, centre = centre, spread = raw - centre)
}

apply_frozen_spread <- function(data, frozen) {
  raw <- statistical_raw_spread(
    data$midpoint_y, data$midpoint_x, frozen$alpha, frozen$beta
  )
  raw - frozen$centre
}

construct_formation_trading_spread <- function(formation, trading, alpha, beta) {
  frozen <- freeze_formation_spread(formation, alpha, beta)
  list(
    formation_spread = frozen$spread,
    trading_spread = apply_frozen_spread(trading, frozen),
    frozen = frozen
  )
}

