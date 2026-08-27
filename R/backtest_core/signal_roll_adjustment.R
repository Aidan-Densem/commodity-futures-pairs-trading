# The submitted method does not define a mechanically offset continuous signal
# coordinate. Exact-contract roll boundaries remain auditable, while the
# formation-fixed raw midpoint spread is passed through unchanged.
mab_adjust_signal_spread <- function(spread_data, tolerance = 1e-12) {
  required <- c("timestamp", "spread", "y_contract", "x_contract")
  mab_assert(is.data.frame(spread_data) && nrow(spread_data) > 0L &&
               all(required %in% names(spread_data)),
             "Signal input lacks the exact-contract spread fields.")
  data <- spread_data[order(spread_data$timestamp, spread_data$global_row_index), , drop = FALSE]
  n <- nrow(data)
  changes <- c(FALSE, data$y_contract[-1L] != data$y_contract[-n] |
                        data$x_contract[-1L] != data$x_contract[-n])
  data$adjusted_signal_spread <- data$spread
  data$roll_offset_delta <- 0
  data$cumulative_roll_offset <- 0
  data$roll_boundary <- changes
  data$signal_roll_adjustment_status <- "NO_ADJUSTMENT_DEFINED"
  data$signal_roll_transition_rule_status <- "NO_SIGNAL_TRANSITION_DEFINED"
  hit <- which(changes)
  audit <- if (!length(hit)) data.frame() else data.frame(
    roll_number = seq_along(hit), outgoing_row_index = hit - 1L,
    incoming_row_index = hit, roll_timestamp = data$timestamp[hit],
    outgoing_y_contract = data$y_contract[hit - 1L], incoming_y_contract = data$y_contract[hit],
    outgoing_x_contract = data$x_contract[hit - 1L], incoming_x_contract = data$x_contract[hit],
    adjustment_source = "none_report_conformant_raw_formation_fixed_spread",
    signal_roll_adjustment_status = "NO_ADJUSTMENT_DEFINED",
    signal_roll_transition_rule_status = "NO_SIGNAL_TRANSITION_DEFINED",
    roll_offset_delta = 0, cumulative_roll_offset = 0, stringsAsFactors = FALSE
  )
  list(
    data = data, roll_audit = audit, pre_existing_adjustment = FALSE,
    signal_roll_adjustment_status = "NO_ADJUSTMENT_DEFINED",
    signal_roll_transition_rule_status = "NO_SIGNAL_TRANSITION_DEFINED"
  )
}
