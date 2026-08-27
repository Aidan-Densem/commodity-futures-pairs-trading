# Exact-transition empirical inputs admit every scientifically valid positive
# observed active duration.  Durations are neither rounded into one-minute
# observations nor interpolated/subdivided.

levy_empirical_transition_mask <- function(transition_valid, active_dt_minutes,
                                           remainder,
                                           one_minute_tolerance = 1e-9) {
  transition_valid %in% TRUE &
    is.finite(as.numeric(active_dt_minutes)) & as.numeric(active_dt_minutes) > 0 &
    is.finite(remainder)
}

levy_exact_ou_remainder <- function(spread, mu, kappa_per_active_minute,
                                    active_dt_minutes) {
  spread <- as.numeric(spread)
  active_dt_minutes <- as.numeric(active_dt_minutes)
  if (length(spread) != length(active_dt_minutes)) {
    stop("Spread and active-duration vectors must have equal length.", call. = FALSE)
  }
  previous <- c(NA_real_, head(spread, -1L))
  spread - mu - exp(-kappa_per_active_minute * active_dt_minutes) *
    (previous - mu)
}

levy_identity_hash <- function(object) {
  if (requireNamespace("digest", quietly = TRUE)) {
    return(digest::digest(object, algo = "sha256", serialize = TRUE))
  }
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(object, path, version = 3)
  unname(tools::sha256sum(path))
}

build_exact_transition_likelihood_inputs <- function(
    selected_schedule, prepared_pair_series,
    output_root = repo_path("output", "exact_transition_likelihood_run"),
    one_minute_tolerance = 1e-9) {
  if (!requireNamespace("arrow", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) stop(
    "Packages 'arrow' and 'jsonlite' are required to construct exact-likelihood inputs.",
    call. = FALSE
  )
  selected <- selected_schedule[selected_schedule$selected %in% TRUE, , drop = FALSE]
  families <- c(
    "GAUSSIAN", "NIG", "GHYP_FULL", "VG", "NTS", "BILATERAL_TS",
    "CGMY", "MEIXNER", "SYMMETRIC_ALPHA_STABLE"
  )
  counts <- c(1L, 3L, 4L, 3L, 5L, 6L, 4L, 3L, 2L)
  input_dir <- repo_path("output", "levy_screen_inputs")
  dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)
  rows <- list(); counter <- 0L
  for (i in seq_len(nrow(selected))) {
    row <- selected[i, , drop = FALSE]
    sync <- prepared_pair_series[[as.character(row$pair_id)]]
    if (is.null(sync)) stop("Missing prepared pair path for ", row$pair_id, call. = FALSE)
    use <- sync$timestamp >= as.POSIXct(row$formation_start, tz = "Europe/London") &
      sync$timestamp <= as.POSIXct(row$formation_end, tz = "Europe/London")
    sync <- sync[use, , drop = FALSE]
    if (!all(c("transition_valid", "active_dt_minutes", "structural_segment_id") %in% names(sync))) {
      stop("Levy input lacks segment-safe transition annotations.", call. = FALSE)
    }
    spread <- statistical_raw_spread(
      sync$midpoint_y, sync$midpoint_x, row$alpha, row$beta
    ) - row$formation_centre
    dt <- sync$active_dt_minutes
    remainder <- levy_exact_ou_remainder(
      spread, row$ou_equilibrium, row$kappa_per_active_minute, dt
    )
    accepted <- levy_empirical_transition_mask(
      sync$transition_valid, dt, remainder, one_minute_tolerance
    )
    transitions <- data.frame(
      active_dt_minutes = dt,
      exact_ou_remainder = remainder,
      accepted_segment_id = sync$structural_segment_id,
      transition_valid = sync$transition_valid %in% TRUE,
      transition_order = seq_along(dt),
      previous_opportunity_index = c(NA_integer_, head(sync$opportunity_index, -1L)),
      current_opportunity_index = sync$opportunity_index,
      previous_timestamp = c(as.POSIXct(NA), head(sync$timestamp, -1L)),
      current_timestamp = sync$timestamp,
      one_active_minute_diagnostic = abs(dt - 1) <= one_minute_tolerance,
      accepted_transition_flag = accepted,
      stringsAsFactors = FALSE
    )
    path <- file.path(input_dir, paste0(gsub(
      "[^A-Za-z0-9_.-]", "_", paste(row$pair_id, row$endpoint_session_date, sep = "__")
    ), ".parquet"))
    arrow::write_parquet(transitions, path)
    source_hash <- unname(tools::sha256sum(path))
    identity_hash <- levy_identity_hash(list(
      pair_id = row$pair_id,
      endpoint_session_date = as.character(row$endpoint_session_date),
      accepted_order = transitions$transition_order[accepted],
      previous_opportunity_index = transitions$previous_opportunity_index[accepted],
      current_opportunity_index = transitions$current_opportunity_index[accepted],
      durations = transitions$active_dt_minutes[accepted],
      mu = row$ou_equilibrium,
      kappa = row$kappa_per_active_minute
    ))
    remainder_scale <- stats::mad(remainder[accepted], constant = 1.4826, na.rm = TRUE)
    for (j in seq_along(families)) {
      counter <- counter + 1L
      rows[[counter]] <- data.frame(
        task_id = paste(gsub("[^A-Za-z0-9_.-]", "_", row$pair_id),
                        format(as.Date(row$endpoint_session_date), "%Y%m%d"),
                        families[[j]], sep = "__"),
        pair_endpoint_key = paste(row$pair_id, row$endpoint_session_date, sep = "|"),
        pair_id = row$pair_id,
        endpoint_session_date = as.Date(row$endpoint_session_date),
        family = families[[j]], family_parameter_count = counts[[j]],
        transition_path = normalizePath(path, winslash = "/", mustWork = TRUE),
        source_hash = source_hash, transitions = sum(accepted),
        segments = length(unique(sync$structural_segment_id[accepted])),
        accepted_transition_identity_sha256 = identity_hash,
        unique_duration_count = length(unique(round(dt[accepted], 10))),
        minimum_duration = min(dt[accepted]),
        median_duration = stats::median(dt[accepted]),
        maximum_duration = max(dt[accepted]),
        empirical_transition_duration = "all_valid_positive_observed_active_durations",
        gaussian_mu = row$ou_equilibrium,
        gaussian_kappa_per_active_minute = row$kappa_per_active_minute,
        increment_scale = row$gaussian_diffusion_scale,
        remainder_scale = remainder_scale,
        stringsAsFactors = FALSE
      )
    }
  }
  tasks <- do.call(rbind, rows)
  task_path <- file.path(
    output_root, "exact_transition_likelihood", "task_index.csv"
  )
  dir.create(dirname(task_path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(tasks, task_path, row.names = FALSE, na = "")
  config <- list(
    engine_version = "exact_transition_conditional_likelihood_v4_irregular_duration",
    formula = "phi_eta(u|Delta)=exp(integral_0^Delta psi_L(u exp(-kappa s)) ds)",
    empirical_sample = "all accepted positive-duration structural-segment-safe transitions",
    mathematical_duration_support = "arbitrary positive Delta",
    scaling = "formation_only_remainder_MAD",
    optimisation_grid_n = 4096L, optimisation_quadrature_n = 12L,
    production_grid_n = 8192L, production_quadrature_n = 24L,
    validation_grid_n = 16384L, validation_quadrature_n = 32L,
    production_maxiter = 45L, production_starts = 3L,
    density_floor = 1e-300, mass_tolerance = 0.002,
    negative_mass_tolerance = 1e-5, edge_cf_tolerance = 1e-6,
    grid_loglik_tolerance_per_observation = 5e-4
  )
  dir.create(file.path(output_root, "config"), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    config, file.path(output_root, "config", "exact_likelihood_config.json"),
    auto_unbox = TRUE, pretty = TRUE
  )
  list(tasks = tasks, config = config)
}
