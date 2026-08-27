percentile_score <- function(x, higher = TRUE) {
  n <- length(x)
  if (!n) return(numeric())
  score <- if (n == 1L) 1 else (rank(x, ties.method = "average") - 1) / (n - 1)
  if (higher) score else 1 - score
}

rank_adf05_top_two <- function(candidate_panel, pair_sleeve_usd = 200000) {
  required <- c(
    "endpoint_id", "endpoint_session_date", "pair_id", "half_life_sessions",
    "cost_adjusted_opportunity", "robust_spread_scale", "v2_cost_primary",
    "technical_object_valid",
    "whole_contract_implementable", "v2_cost_status_valid",
    "final_quote_feasibility_pass", "adf05_pass"
  )
  missing <- setdiff(required, names(candidate_panel))
  if (length(missing)) stop("Candidate panel is missing: ", paste(missing, collapse = ", "))
  flag <- function(x) !is.na(x) & x %in% TRUE
  candidate_panel$primary_eligible <-
    flag(candidate_panel$technical_object_valid) &
    flag(candidate_panel$whole_contract_implementable) &
    flag(candidate_panel$v2_cost_status_valid) &
    flag(candidate_panel$final_quote_feasibility_pass) &
    flag(candidate_panel$adf05_pass) &
    is.finite(candidate_panel$half_life_sessions) &
    candidate_panel$half_life_sessions > 0 &
    is.finite(candidate_panel$cost_adjusted_opportunity) &
    candidate_panel$cost_adjusted_opportunity > 0 &
    is.finite(candidate_panel$robust_spread_scale) &
    candidate_panel$robust_spread_scale > 0 &
    is.finite(candidate_panel$v2_cost_primary) & candidate_panel$v2_cost_primary > 0
  # NECESSARY BUG FIX: initialize full-length columns before grouped writes.
  candidate_panel$primary_score <- NA_real_
  candidate_panel$primary_rank <- NA_integer_
  candidate_panel$selected <- FALSE
  candidate_panel$eligible_count <- 0L
  candidate_panel$selected_count <- 0L
  groups <- split(seq_len(nrow(candidate_panel)), candidate_panel$endpoint_id)
  for (rows in groups) {
    use <- rows[candidate_panel$primary_eligible[rows]]
    if (!length(use)) next
    h <- percentile_score(candidate_panel$half_life_sessions[use], higher = FALSE)
    o <- percentile_score(candidate_panel$cost_adjusted_opportunity[use], higher = TRUE)
    candidate_panel$primary_score[use] <- 0.5 * h + 0.5 * o
    ord <- order(
      -candidate_panel$primary_score[use],
      candidate_panel$half_life_sessions[use],
      -candidate_panel$cost_adjusted_opportunity[use],
      -candidate_panel$robust_spread_scale[use],
      candidate_panel$v2_cost_primary[use],
      candidate_panel$pair_id[use]
    )
    candidate_panel$primary_rank[use[ord]] <- seq_along(ord)
    candidate_panel$selected[use] <- candidate_panel$primary_rank[use] <= min(2L, length(use))
    candidate_panel$eligible_count[rows] <- length(use)
    candidate_panel$selected_count[rows] <- min(2L, length(use))
  }
  candidate_panel$pair_sleeve_usd <- pair_sleeve_usd
  candidate_panel$endpoint_committed_capital_usd <-
    pair_sleeve_usd * candidate_panel$selected_count
  candidate_panel
}
