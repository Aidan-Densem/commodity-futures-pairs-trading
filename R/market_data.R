# Statistical prices are formed only from accepted synchronous bid/ask quotes.
# The optional close column is retained as a diagnostic reference and is never
# used as a fallback for the statistical midpoint.

accepted_quote_midpoint <- function(bid, ask) {
  valid <- is.finite(bid) & is.finite(ask) & bid > 0 & ask > 0 & ask >= bid
  midpoint <- rep(NA_real_, length(bid))
  midpoint[valid] <- (bid[valid] + ask[valid]) / 2
  midpoint
}

prepare_accepted_quotes <- function(x, timezone = "Europe/London") {
  x <- validate_quote_table(x, timezone)
  x$statistical_quote_valid <- is.finite(x$bid) & is.finite(x$ask) &
    x$bid > 0 & x$ask > 0 & x$ask >= x$bid
  x$midpoint <- accepted_quote_midpoint(x$bid, x$ask)
  x$execution_quote_clean <- NA
  x$execution_quote_reason <- NA_character_
  x
}

synchronise_quote_legs <- function(y_quotes, x_quotes, timezone = "Europe/London") {
  y <- prepare_accepted_quotes(y_quotes, timezone)
  x <- prepare_accepted_quotes(x_quotes, timezone)
  y <- y[!duplicated(y$timestamp), ]
  x <- x[!duplicated(x$timestamp), ]
  out <- merge(y, x, by = "timestamp", suffixes = c("_y", "_x"), sort = TRUE)
  out$opportunity_index <- seq_len(nrow(out))
  out$raw_simultaneous_opportunity <-
    out$statistical_quote_valid_y & out$statistical_quote_valid_x
  out$statistical_quote_valid <- out$raw_simultaneous_opportunity
  # Compatibility field: it now has one unambiguous statistical meaning.
  out$simultaneous_quote_valid <- out$statistical_quote_valid
  out$quote_valid_y <- out$statistical_quote_valid_y
  out$quote_valid_x <- out$statistical_quote_valid_x
  out$execution_quote_clean <- NA
  out$midpoint_y[!out$statistical_quote_valid] <- NA_real_
  out$midpoint_x[!out$statistical_quote_valid] <- NA_real_
  out
}
