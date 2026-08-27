ou_gh_preflight_fitted_law <- function(
    fit,
    fit_hash,
    probabilities = c(1e-3, 0.01, 0.05, 0.5, 0.95, 0.99, 1 - 1e-3),
    table_n = 4096L,
    reference_n = 8192L
) {
  started <- proc.time()[["elapsed"]]
  result <- tryCatch({
    table <- ou_gh_build_validated_fft_table(
      fit, 1, table_n, 18, 34, 4, 48L
    )
    reference <- ou_gh_build_validated_fft_table(
      fit, 1, reference_n, max(18, table$range_sd + 4),
      max(34, table$range_sd + 12), 4, 64L
    )
    quantile <- ou_gh_table_quantile(probabilities, table)
    reference_quantile <- ou_gh_table_quantile(probabilities, reference)
    maximum_difference <- max(abs(quantile - reference_quantile) / table$sd)
    uniforms <- (seq_len(1000L) - 0.5) / 1000L
    draw_a <- ou_gh_draw_remainder_table(1000L, table, uniforms = uniforms)
    draw_b <- ou_gh_draw_remainder_table(1000L, table, uniforms = uniforms)
    deterministic <- identical(as.numeric(draw_a), as.numeric(draw_b))
    valid <- maximum_difference <= 0.02 && deterministic &&
      isTRUE(table$valid) && isTRUE(reference$valid)
    list(
      status = if (valid) "eligible" else "pending_simulator_validation",
      route = "reference_exact_CF_FFT_inversion",
      failure_reason = if (valid) "" else
        "Reference table parity or deterministic replay failed.",
      maximum_quantile_difference_sd = maximum_difference,
      deterministic_replay = deterministic,
      table_fingerprint = table$fingerprint,
      reference_fingerprint = reference$fingerprint,
      table_metrics = table[c(
        "mass_rectangle", "mass_trapezoid", "boundary_density_ratio",
        "negative_density_relative_floor", "cdf_min_increment"
      )]
    )
  }, error = function(condition) list(
    status = "unavailable", route = "reference_exact_CF_FFT_inversion",
    failure_reason = conditionMessage(condition),
    maximum_quantile_difference_sd = NA_real_,
    deterministic_replay = FALSE,
    table_fingerprint = NA_character_, reference_fingerprint = NA_character_
  ))
  result$runtime_seconds <- proc.time()[["elapsed"]] - started
  result$hash <- ou_gh_hash_object(list(
    fit_hash = fit_hash, status = result$status, route = result$route,
    failure_reason = result$failure_reason,
    maximum_quantile_difference_sd = result$maximum_quantile_difference_sd,
    deterministic_replay = result$deterministic_replay,
    table_fingerprint = result$table_fingerprint,
    reference_fingerprint = result$reference_fingerprint
  ))
  result
}

ou_gh_attach_simulator_preflight <- function(result) {
  if (is.null(result$fit_natural) || !startsWith(result$fit_status, "fit_success")) {
    result$simulator <- list(
      status = "unavailable", route = "",
      failure_reason = "No admissible fitted law is available.",
      hash = NA_character_
    )
    result$threshold_eligibility <- "not_eligible_estimation_failure"
    return(result)
  }
  result$simulator <- ou_gh_preflight_fitted_law(
    result$fit_natural, result$fit_hash
  )
  result$threshold_eligibility <- if (result$simulator$status == "eligible") {
    "reference_sampler_only_production_FGMC_pending"
  } else "not_eligible_simulator_validation_required"
  result
}
