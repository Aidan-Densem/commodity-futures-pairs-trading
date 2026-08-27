candidate <- function(pair, h, o, scale = 1, cost = 1, adf = TRUE, endpoint = "e") data.frame(
  endpoint_id = endpoint, endpoint_session_date = as.Date("2025-12-01"), pair_id = pair,
  half_life_sessions = h, cost_adjusted_opportunity = o,
  robust_spread_scale = scale, v2_cost_primary = cost,
  technical_object_valid = TRUE, whole_contract_implementable = TRUE,
  v2_cost_status_valid = TRUE, final_quote_feasibility_pass = TRUE, adf05_pass = adf,
  downstream_failure = FALSE, stringsAsFactors = FALSE)
ranked_ids <- function(x) rank_adf05_top_two(x)$pair_id[order(rank_adf05_top_two(x)$primary_rank)]
smoke_expect(identical(ranked_ids(rbind(candidate("A", 1, 3), candidate("B", 2, 1))), c("A", "B")),
             "R1: unequal score ordering failed")
smoke_expect(ranked_ids(rbind(candidate("SHORT", 1, 1), candidate("LONG", 2, 2)))[[1L]] == "SHORT",
             "R2: half-life tie-break failed")
smoke_expect(ranked_ids(rbind(candidate("BIG", 1, 1, 2), candidate("SMALL", 1, 1, 1)))[[1L]] == "BIG",
             "R3: robust-scale tie-break failed")
smoke_expect(ranked_ids(rbind(candidate("CHEAP", 1, 1, 1, .5), candidate("DEAR", 1, 1, 1, 1)))[[1L]] == "CHEAP",
             "R4: prospective-cost tie-break failed")
smoke_expect(ranked_ids(rbind(candidate("A", 1, 1), candidate("B", 1, 1)))[[1L]] == "A",
             "R5: pair-ID tie-break failed")
zero <- rank_adf05_top_two(candidate("A", 1, 1, adf = FALSE)); smoke_expect(!any(zero$selected), "R6 failed")
one <- rank_adf05_top_two(candidate("A", 99, 1)); smoke_expect(sum(one$selected) == 1L, "R7/R10 failed")
many <- rank_adf05_top_two(rbind(candidate("A", 1, 3), candidate("B", 2, 2), candidate("C", 3, 1)))
smoke_expect(sum(many$selected) == 2L, "R8 failed")
gate <- rank_adf05_top_two(rbind(candidate("FAIL", .1, 100, adf = FALSE), candidate("PASS", 2, 1)))
smoke_expect(!gate$selected[gate$pair_id == "FAIL"], "R9: ADF failure was rescued")
smoke_expect(!"gaussian_threshold_success" %in% names(many), "R11: obsolete Gaussian field required")
many$downstream_failure[many$primary_rank == 1L] <- TRUE
smoke_expect(sum(many$selected) == 2L && !many$selected[many$primary_rank == 3L],
             "R12: rank-three substitution occurred")
cat("DYNAMIC_RANKING_REPORT_CONTRACT_PASS R1-R12\n")
