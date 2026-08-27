ou_gh_assert_full_run_authority <- function(
    execution_authority,
    run_full_selected_pair_estimation,
    gate_passed
) {
  ou_gh_assert(
    identical(execution_authority, OU_GH_EXECUTION_CONTRACT$authority),
    "Full estimation requires execution_authority='local_user'."
  )
  ou_gh_assert(
    isTRUE(OU_GH_EXECUTION_CONTRACT$full_selected_pair_estimation_authorised),
    "The production contract does not authorise full estimation."
  )
  ou_gh_assert(isTRUE(run_full_selected_pair_estimation),
    "The full-run switch must be explicitly TRUE.")
  ou_gh_assert(isTRUE(gate_passed),
    "The frozen synthetic and bounded-pilot gate has not passed.")
  invisible(TRUE)
}

ou_gh_assert_threshold_prohibited <- function() {
  ou_gh_assert(
    identical(OU_GH_EXECUTION_CONTRACT$threshold_optimisation_authorised, FALSE) &&
      identical(OU_GH_EXECUTION_CONTRACT$monetary_backtest_authorised, FALSE),
    "Threshold or monetary-backtest authority was unexpectedly enabled."
  )
  invisible(TRUE)
}
