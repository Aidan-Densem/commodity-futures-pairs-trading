quotes <- smoke_quotes()

# A: arithmetic identity.
mid <- accepted_quote_midpoint(c(10, 20), c(12, 24))
smoke_equal(mid, c(11, 22), message = "A: midpoint identity failed")

sync <- synchronise_quote_legs(quotes$y, quotes$x)
alpha <- log(100)
beta <- 1
expected <- log(sync$midpoint_y) - alpha - beta * log(sync$midpoint_x)
observed <- statistical_raw_spread(sync$midpoint_y, sync$midpoint_x, alpha, beta)

# B: spread uses midpoint.
smoke_equal(observed, expected, message = "B: statistical spread is not midpoint-based")

# C: Close invariance.
changed_close <- quotes
changed_close$y$close <- changed_close$y$close + 1e6
changed_close$x$close <- changed_close$x$close * 1e6
sync_close <- synchronise_quote_legs(changed_close$y, changed_close$x)
close_spread <- statistical_raw_spread(
  sync_close$midpoint_y, sync_close$midpoint_x, alpha, beta
)
smoke_equal(close_spread, observed, message = "C: Close perturbation changed the spread")

# D: midpoint sensitivity.
changed_mid <- quotes
changed_mid$y$bid[[2L]] <- changed_mid$y$bid[[2L]] + 2
changed_mid$y$ask[[2L]] <- changed_mid$y$ask[[2L]] + 2
sync_mid <- synchronise_quote_legs(changed_mid$y, changed_mid$x)
mid_spread <- statistical_raw_spread(sync_mid$midpoint_y, sync_mid$midpoint_x, alpha, beta)
expected_change <- log(sync_mid$midpoint_y[[2L]] / sync$midpoint_y[[2L]])
smoke_equal(
  mid_spread[[2L]] - observed[[2L]], expected_change,
  message = "D: midpoint sensitivity is incorrect"
)

# E: invalid quotes are rejected with no Close fallback.
invalid <- quotes
invalid$y$ask[[2L]] <- NA_real_
invalid$y$close[[2L]] <- 999999
sync_invalid <- synchronise_quote_legs(invalid$y, invalid$x)
smoke_expect(!sync_invalid$simultaneous_quote_valid[[2L]], "E: invalid quote was accepted")
smoke_expect(is.na(sync_invalid$midpoint_y[[2L]]), "E: Close was used as a fallback")

# F: formation-only freeze and trading-coordinate consistency.
formation <- sync[1:3, ]
trading <- sync[4, , drop = FALSE]
fit <- estimate_formation_kalman_hedge(formation, q = 1e-4, ve = 1e-3)
frozen_before <- fit$frozen
base_trading <- apply_frozen_spread(trading, frozen_before)
trading_changed <- trading
trading_changed$midpoint_y <- trading_changed$midpoint_y * 1.01
changed_trading <- apply_frozen_spread(trading_changed, frozen_before)
smoke_expect(!isTRUE(all.equal(base_trading, changed_trading)), "F: trading midpoint did not change spread")
smoke_equal(fit$frozen$alpha, frozen_before$alpha, message = "F: frozen alpha changed")
smoke_equal(fit$frozen$beta, frozen_before$beta, message = "F: frozen beta changed")
smoke_equal(fit$frozen$centre, frozen_before$centre, message = "F: frozen centre changed")

cat("MIDPRICE_TESTS_PASS A B C D E F\n")

