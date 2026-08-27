# Frozen descriptive diagnostic definitions from the validated production
# study. They are excluded from model selection and consume only
# formation-period, segment-safe Gaussian OU remainders.
govc_config <- function() list(
  timezone = "Europe/London", clock_bin_minutes = 30L,
  minimum_bin_reference_observations = 120L, neighbour_bin_radius = 1L,
  scale_floor = 0.1, lags = c(1L, 2L, 5L, 10L, 15L, 30L, 60L),
  lag_weights = c(0.328901856284897, 0.232568732923894,
                  0.147089381715779, 0.104007899251764,
                  0.0849220941285408, 0.0600489886308535,
                  0.0424610470642704),
  huber_cap = 5, minimum_pair_window_observations = 500L,
  minimum_segment_pairs = 100L, meaningful_clustering_score = 0.0025,
  material_acf = 0.05, material_signed_score = 0.001,
  material_signed_acf = 0.05, material_duration_spearman = 0.1,
  material_duration_scale_ratio = 1.25,
  material_intraday_scale_ratio = 1.5, largely_explained_share = 0.5,
  partial_explanation_share = 0.25, persistent_share_threshold = 0.4,
  top_point_one_energy_event = 0.3, top_one_energy_event = 0.6,
  top_session_event_share = 0.35, top_three_session_event_share = 0.65
)

govc_robust_scale <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2L) return(NA_real_)
  1.4826 * stats::median(abs(x - stats::median(x)))
}

govc_safe_cor <- function(x, y, method = "pearson") {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3L || stats::sd(x[ok]) == 0 || stats::sd(y[ok]) == 0) return(NA_real_)
  suppressWarnings(stats::cor(x[ok], y[ok], method = method))
}

govc_segment_pairs <- function(x, segment, lag) {
  pieces <- split(seq_along(x), segment, drop = TRUE)
  pieces <- pieces[lengths(pieces) > lag]
  if (!length(pieces)) return(list(left = numeric(), right = numeric()))
  left <- unlist(lapply(pieces, function(ii) x[ii[seq_len(length(ii) - lag)]]), use.names = FALSE)
  right <- unlist(lapply(pieces, function(ii) x[ii[(lag + 1L):length(ii)]]), use.names = FALSE)
  list(left = left, right = right)
}

govc_lag_metrics <- function(x, segment, lags, version) {
  transforms <- list(signed = identity, absolute = abs, squared = function(z) z^2)
  out <- do.call(rbind, lapply(names(transforms), function(metric) {
    y <- transforms[[metric]](x)
    do.call(rbind, lapply(lags, function(h) {
      p <- govc_segment_pairs(y, segment, h)
      data.frame(version = version, metric = metric, lag = h, pairs = length(p$left),
        acf = govc_safe_cor(p$left, p$right), stringsAsFactors = FALSE)
    }))
  }))
  row.names(out) <- NULL
  out
}

govc_score_from_lags <- function(lag_table, weights, cfg) {
  get_score <- function(metric) {
    x <- lag_table[lag_table$metric == metric, ]
    m <- match(cfg$lags, x$lag)
    rho <- x$acf[m]
    if (any(!is.finite(rho))) return(NA_real_)
    sum(weights * rho^2)
  }
  signed <- get_score("signed")
  absolute <- get_score("absolute")
  squared <- get_score("squared")
  mag <- 0.5 * absolute + 0.5 * squared
  max_signed <- suppressWarnings(max(abs(lag_table$acf[lag_table$metric == "signed"]), na.rm = TRUE))
  max_mag <- suppressWarnings(max(abs(lag_table$acf[lag_table$metric %in% c("absolute", "squared")]), na.rm = TRUE))
  min_pairs <- suppressWarnings(min(lag_table$pairs, na.rm = TRUE))
  q_p <- function(metric) {
    d <- lag_table[lag_table$metric == metric & is.finite(lag_table$acf), ]
    if (!nrow(d)) return(NA_real_)
    n <- max(d$pairs)
    q <- n * (n + 2) * sum(d$acf^2 / pmax(1, n - d$lag))
    stats::pchisq(q, df = nrow(d), lower.tail = FALSE)
  }
  list(clustering_score = mag, signed_score = signed, absolute_score = absolute, squared_score = squared,
    max_signed_acf = max_signed, max_magnitude_acf = max_mag, minimum_lag_pairs = min_pairs,
    signed_ljung_p = q_p("signed"), absolute_ljung_p = q_p("absolute"), squared_ljung_p = q_p("squared"),
    meaningful_magnitude = is.finite(mag) && mag >= cfg$meaningful_clustering_score &&
      is.finite(max_mag) && max_mag >= cfg$material_acf,
    material_signed = is.finite(signed) && signed >= cfg$material_signed_score &&
      is.finite(max_signed) && max_signed >= cfg$material_signed_acf)
}

govc_arch_lm <- function(x, segment) {
  p1 <- govc_segment_pairs(x^2, segment, 1L)
  if (length(p1$left) < 100L) return(c(statistic = NA_real_, df = NA_real_, p = NA_real_))
  fit <- try(stats::lm(p1$right ~ p1$left), silent = TRUE)
  if (inherits(fit, "try-error")) return(c(statistic = NA_real_, df = NA_real_, p = NA_real_))
  r2 <- summary(fit)$r.squared
  c(statistic = length(p1$left) * r2, df = 1, p = stats::pchisq(length(p1$left) * r2, 1, lower.tail = FALSE))
}

govc_loso_intraday <- function(z, session, clock_bin, cfg) {
  session <- as.character(session)
  combos <- unique(data.frame(session = session, clock_bin = clock_bin, stringsAsFactors = FALSE))
  combos <- combos[order(combos$session, combos$clock_bin), ]
  scale <- center <- rep(NA_real_, length(z)); fallback <- rep(NA_integer_, length(z)); reference_n <- rep(NA_integer_, length(z))
  all_scale <- govc_robust_scale(z)
  if (!is.finite(all_scale) || all_scale <= 0) all_scale <- 1
  max_bin <- as.integer(24L * 60L / cfg$clock_bin_minutes - 1L)
  for (k in seq_len(nrow(combos))) {
    d <- combos$session[k]; b <- combos$clock_bin[k]
    target <- session == d & clock_bin == b
    ref <- session != d & clock_bin == b
    level <- 0L
    if (sum(ref) < cfg$minimum_bin_reference_observations) {
      lo <- max(0L, b - cfg$neighbour_bin_radius); hi <- min(max_bin, b + cfg$neighbour_bin_radius)
      ref <- session != d & clock_bin >= lo & clock_bin <= hi
      level <- 1L
    }
    if (sum(ref) < cfg$minimum_bin_reference_observations) {
      ref <- session != d
      level <- 2L
    }
    rr <- z[ref & is.finite(z)]
    med <- if (length(rr)) stats::median(rr) else stats::median(z, na.rm = TRUE)
    sc <- govc_robust_scale(rr)
    if (!is.finite(sc) || sc <= 0) { sc <- all_scale; level <- 3L }
    scale[target] <- max(sc, cfg$scale_floor)
    center[target] <- med
    fallback[target] <- level
    reference_n[target] <- length(rr)
  }
  rms <- sqrt(mean(scale^2, na.rm = TRUE))
  normalised <- scale / rms
  bins <- sort(unique(clock_bin))
  curve <- do.call(rbind, lapply(bins, function(b) {
    zz <- z[clock_bin == b]
    data.frame(clock_bin = b, clock_minutes = b * cfg$clock_bin_minutes,
      bin_label = sprintf("%02d:%02d", (b * cfg$clock_bin_minutes) %/% 60L, (b * cfg$clock_bin_minutes) %% 60L),
      observations = sum(is.finite(zz)), median = stats::median(zz, na.rm = TRUE), raw_scale = govc_robust_scale(zz),
      stringsAsFactors = FALSE)
  }))
  curve$normalisation_rms <- sqrt(stats::weighted.mean(curve$raw_scale^2, curve$observations, na.rm = TRUE))
  curve$normalised_scale <- curve$raw_scale / curve$normalisation_rms
  list(adjusted = z / normalised, scale = scale, normalised_scale = normalised, center = center,
    fallback = fallback, reference_n = reference_n, curve = curve)
}

govc_run_stats <- function(flag, segment) {
  pieces <- split(as.logical(flag), segment, drop = TRUE)
  runs <- unlist(lapply(pieces, function(x) if (any(x)) rle(x)$lengths[rle(x)$values] else integer()), use.names = FALSE)
  pairs <- lapply(pieces[lengths(pieces) > 1L], function(x) cbind(x[-length(x)], x[-1L]))
  pairs <- if (length(pairs)) do.call(rbind, pairs) else matrix(logical(), ncol = 2L)
  cond <- if (nrow(pairs) && any(pairs[, 1L])) mean(pairs[pairs[, 1L], 2L]) else NA_real_
  c(run_count = length(runs), max_run = if (length(runs)) max(runs) else 0, conditional_after = cond)
}

govc_window_diagnostics <- function(inventory_row, save_checkpoint = TRUE) {
  if (!requireNamespace("arrow", quietly = TRUE)) stop("Package arrow is required.", call. = FALSE)
  cfg <- govc_config()
  started <- proc.time()[3L]
  d <- as.data.frame(arrow::read_parquet(inventory_row$residual_path))
  ord <- order(d$next_time, d$transition_index)
  d <- d[ord, ]; row.names(d) <- NULL
  z <- as.numeric(d$gaussian_standardized_z)
  eta <- as.numeric(d$raw_remainder_eta)
  dt <- as.numeric(d$active_dt_minutes)
  london <- as.POSIXlt(d$next_time, tz = cfg$timezone)
  clock_minutes <- london$hour * 60L + london$min
  clock_bin <- as.integer(clock_minutes %/% cfg$clock_bin_minutes)
  session <- as.character(d$session_id)
  segment <- paste(session, d$accepted_segment_id, sep = "::")
  finite <- is.finite(z) & is.finite(eta) & is.finite(dt) & dt > 0 & !is.na(session) & !is.na(segment)
  if (!all(finite)) {
    d <- d[finite, ]; z <- z[finite]; eta <- eta[finite]; dt <- dt[finite]
    clock_minutes <- clock_minutes[finite]; clock_bin <- clock_bin[finite]; session <- session[finite]; segment <- segment[finite]
  }
  seasonal <- govc_loso_intraday(z, session, clock_bin, cfg)
  cap <- function(x) sign(x) * pmin(abs(x), cfg$huber_cap)
  versions <- list(raw = z, seasonal = seasonal$adjusted, huber = cap(z), seasonal_huber = cap(seasonal$adjusted))
  lag_metrics <- do.call(rbind, lapply(names(versions), function(nm) govc_lag_metrics(versions[[nm]], segment, cfg$lags, nm)))
  scores <- lapply(names(versions), function(nm) govc_score_from_lags(lag_metrics[lag_metrics$version == nm, ], cfg$lag_weights, cfg))
  names(scores) <- names(versions)
  arch <- lapply(versions, govc_arch_lm, segment = segment)

  duration_values <- sort(unique(dt))
  duration <- do.call(rbind, lapply(duration_values, function(v) {
    zz <- z[dt == v]
    data.frame(duration_minutes = v, observations = length(zz), variance = stats::var(zz),
      robust_scale = govc_robust_scale(zz), mean_absolute_z = mean(abs(zz)), stringsAsFactors = FALSE)
  }))
  duration_scale_ratio <- if (sum(is.finite(duration$robust_scale) & duration$robust_scale > 0) >= 2L)
    max(duration$robust_scale, na.rm = TRUE) / min(duration$robust_scale[duration$robust_scale > 0], na.rm = TRUE) else NA_real_
  duration_abs_rho <- govc_safe_cor(abs(z), dt, "spearman")
  duration_sq_rho <- govc_safe_cor(z^2, dt, "spearman")
  duration_material <- length(duration_values) >= 2L &&
    (max(abs(c(duration_abs_rho, duration_sq_rho)), na.rm = TRUE) >= cfg$material_duration_spearman ||
       (is.finite(duration_scale_ratio) && duration_scale_ratio >= cfg$material_duration_scale_ratio))
  if (!isTRUE(duration_material)) duration_material <- FALSE

  energy <- z^2; total_energy <- sum(energy)
  top_share <- function(frac) {
    n <- max(1L, ceiling(length(energy) * frac))
    sum(sort(energy, decreasing = TRUE)[seq_len(n)]) / total_energy
  }
  session_energy <- tapply(energy, session, sum)
  session_v <- tapply(energy, session, mean)
  session_n <- tapply(energy, session, length)
  session_order <- order(as.Date(names(session_v)))
  session_v <- session_v[session_order]; session_energy <- session_energy[names(session_v)]; session_n <- session_n[names(session_v)]
  session_names <- names(session_v)
  session_v <- setNames(as.numeric(session_v), session_names)
  session_energy <- setNames(as.numeric(session_energy), session_names)
  session_n <- setNames(as.numeric(session_n), session_names)
  energy_share <- session_energy / sum(session_energy)
  top_sessions <- sort(energy_share, decreasing = TRUE)
  threshold_v <- stats::median(session_v) + stats::mad(session_v, constant = 1, na.rm = TRUE)
  high <- as.logical(session_v > threshold_v)
  high_runs <- if (any(high)) rle(high)$lengths[rle(high)$values] else integer()
  session_lag1 <- if (length(session_v) > 2L) govc_safe_cor(session_v[-length(session_v)], session_v[-1L], "spearman") else NA_real_
  q90 <- stats::quantile(abs(z), .90, names = FALSE); q95 <- stats::quantile(abs(z), .95, names = FALSE)
  runs90 <- govc_run_stats(abs(z) > q90, segment); runs95 <- govc_run_stats(abs(z) > q95, segment)
  event_concentrated <- top_share(.001) >= cfg$top_point_one_energy_event || top_share(.01) >= cfg$top_one_energy_event ||
    top_sessions[1L] >= cfg$top_session_event_share || sum(utils::head(top_sessions, 3L)) >= cfg$top_three_session_event_share

  curve <- seasonal$curve
  positive_scales <- curve$normalised_scale[is.finite(curve$normalised_scale) & curve$normalised_scale > 0]
  intraday_ratio <- if (length(positive_scales)) max(positive_scales) / min(positive_scales) else NA_real_
  active_bins <- sort(unique(clock_bin))
  open_bins <- utils::head(active_bins, min(2L, length(active_bins)))
  close_bins <- utils::tail(active_bins, min(2L, length(active_bins)))
  middle_bins <- setdiff(active_bins, c(open_bins, close_bins))
  middle_var <- if (length(middle_bins)) mean(z[clock_bin %in% middle_bins]^2) else NA_real_
  open_ratio <- mean(z[clock_bin %in% open_bins]^2) / middle_var
  close_ratio <- mean(z[clock_bin %in% close_bins]^2) / middle_var

  c0 <- scores$raw$clustering_score; cs <- scores$seasonal$clustering_score
  ch <- scores$huber$clustering_score; csh <- scores$seasonal_huber$clustering_score
  delta_s <- 0.5 * ((c0 - cs) + (ch - csh))
  delta_h <- 0.5 * ((c0 - ch) + (cs - csh))
  shares <- if (is.finite(c0) && c0 >= cfg$meaningful_clustering_score)
    c(seasonality = delta_s / c0, extreme = delta_h / c0, persistent = csh / c0) else rep(NA_real_, 3L)
  nonnegative <- pmax(c(delta_s, delta_h, csh), 0)
  names(nonnegative) <- c("seasonality", "extreme", "persistent")
  adequate <- length(z) >= cfg$minimum_pair_window_observations && scores$raw$minimum_lag_pairs >= cfg$minimum_segment_pairs &&
    all(is.finite(c(c0, cs, ch, csh)))
  signed_material <- scores$raw$material_signed
  intraday_material <- is.finite(intraday_ratio) && intraday_ratio >= cfg$material_intraday_scale_ratio
  seasonality_led <- is.finite(shares["seasonality"]) && shares["seasonality"] >= cfg$largely_explained_share &&
    shares["persistent"] < cfg$persistent_share_threshold && shares["extreme"] < 0.35
  extreme_led <- is.finite(shares["extreme"]) && (shares["extreme"] >= cfg$largely_explained_share || event_concentrated) &&
    shares["persistent"] < cfg$persistent_share_threshold
  mixed <- is.finite(shares["seasonality"]) && shares["seasonality"] >= cfg$partial_explanation_share &&
    shares["extreme"] >= cfg$partial_explanation_share && shares["persistent"] < 0.50
  persistent <- scores$seasonal_huber$meaningful_magnitude && is.finite(shares["persistent"]) &&
    shares["persistent"] >= cfg$persistent_share_threshold && scores$seasonal_huber$max_magnitude_acf >= cfg$material_acf &&
    !(event_concentrated && shares["extreme"] >= cfg$largely_explained_share)
  classification <- if (!adequate) "insufficient or unstable evidence" else
    if (!scores$raw$meaningful_magnitude && signed_material) "signed residual dependence suggests mean misspecification" else
    if (!scores$raw$meaningful_magnitude) "no economically meaningful magnitude clustering" else
    if (duration_material && !scores$seasonal_huber$meaningful_magnitude) "material duration dependence without persistent scale" else
    if (seasonality_led) "deterministic intraday seasonality largely explains clustering" else
    if (extreme_led) "isolated extremes or event sessions largely explain clustering" else
    if (mixed) "mixed seasonality and extreme-event explanation" else
    if (persistent) "persistent conditional-scale clustering after controls" else
    if (scores$seasonal_huber$meaningful_magnitude) "persistent conditional-scale clustering after controls" else
      "mixed seasonality and extreme-event explanation"

  summary <- data.frame(pair_endpoint_key = inventory_row$pair_endpoint_key, pair_id = inventory_row$pair_id,
    endpoint_id = inventory_row$endpoint_id, endpoint_session_date = as.character(inventory_row$endpoint_session_date),
    observations = length(z), sessions = length(unique(session)),
    segments = length(unique(segment)), finite_residuals = all(is.finite(z)), positive_durations = all(dt > 0),
    distinct_durations = length(duration_values), eta_z_distinct = !isTRUE(all.equal(eta, z)),
    C0 = c0, CS = cs, CH = ch, CSH = csh,
    C0_meaningful = scores$raw$meaningful_magnitude, CS_meaningful = scores$seasonal$meaningful_magnitude,
    CH_meaningful = scores$huber$meaningful_magnitude, CSH_meaningful = scores$seasonal_huber$meaningful_magnitude,
    baseline_max_magnitude_acf = scores$raw$max_magnitude_acf, adjusted_max_magnitude_acf = scores$seasonal_huber$max_magnitude_acf,
    baseline_signed_score = scores$raw$signed_score, adjusted_signed_score = scores$seasonal_huber$signed_score,
    baseline_max_signed_acf = scores$raw$max_signed_acf, material_signed_dependence = signed_material,
    delta_seasonality_signed = delta_s, delta_extreme_signed = delta_h,
    delta_seasonality_nonnegative = nonnegative["seasonality"], delta_extreme_nonnegative = nonnegative["extreme"],
    persistent_nonnegative = nonnegative["persistent"], seasonality_share = shares["seasonality"],
    extreme_share = shares["extreme"], persistent_share = shares["persistent"],
    intraday_scale_ratio = intraday_ratio, material_intraday_seasonality = intraday_material,
    open_region_variance_ratio = open_ratio, close_region_variance_ratio = close_ratio,
    loso_same_bin_rate = mean(seasonal$fallback == 0L), loso_neighbour_rate = mean(seasonal$fallback == 1L),
    loso_other_session_global_rate = mean(seasonal$fallback == 2L), loso_global_fallback_rate = mean(seasonal$fallback == 3L),
    minimum_loso_reference_n = min(seasonal$reference_n, na.rm = TRUE), duration_abs_spearman = duration_abs_rho,
    duration_squared_spearman = duration_sq_rho, duration_scale_ratio = duration_scale_ratio,
    material_duration_effect = duration_material, max_abs_z = max(abs(z)), q99_abs_z = unname(stats::quantile(abs(z), .99)),
    q995_abs_z = unname(stats::quantile(abs(z), .995)), q999_abs_z = unname(stats::quantile(abs(z), .999)),
    top_point_one_percent_energy_share = top_share(.001), top_one_percent_energy_share = top_share(.01),
    top_session_energy_share = top_sessions[1L], top_three_session_energy_share = sum(utils::head(top_sessions, 3L)),
    session_energy_hhi = sum(energy_share^2), session_V_dispersion = stats::mad(session_v, constant = 1, na.rm = TRUE),
    session_V_lag1_spearman = session_lag1, high_volatility_sessions = sum(high),
    max_high_volatility_session_run = if (length(high_runs)) max(high_runs) else 0L,
    exceedance90_runs = runs90["run_count"], exceedance90_max_run = runs90["max_run"],
    exceedance90_conditional = runs90["conditional_after"], exceedance95_runs = runs95["run_count"],
    exceedance95_max_run = runs95["max_run"], exceedance95_conditional = runs95["conditional_after"],
    event_concentrated = event_concentrated, raw_arch_lm_p = arch$raw["p"], adjusted_arch_lm_p = arch$seasonal_huber["p"],
    primary_classification = classification, warning_signed_mean = signed_material,
    warning_duration = duration_material, warning_event_concentration = event_concentrated,
    warning_sparse_intraday_fallback = mean(seasonal$fallback >= 2L) > 0.10,
    runtime_seconds = proc.time()[3L] - started, stringsAsFactors = FALSE)
  curve$pair_endpoint_key <- inventory_row$pair_endpoint_key
  curve$pair_id <- inventory_row$pair_id
  curve$endpoint_id <- inventory_row$endpoint_id
  duration$pair_endpoint_key <- inventory_row$pair_endpoint_key
  lag_metrics$pair_endpoint_key <- inventory_row$pair_endpoint_key
  session_table <- data.frame(pair_endpoint_key = inventory_row$pair_endpoint_key, session_id = names(session_v),
    observations = as.integer(session_n), session_V = as.numeric(session_v), energy_share = as.numeric(energy_share),
    high_volatility = as.logical(high), stringsAsFactors = FALSE)
  out <- list(summary = summary, intraday_curve = curve, duration = duration, lag_metrics = lag_metrics,
    sessions = session_table, config_hash = if (requireNamespace("digest", quietly = TRUE)) digest::digest(cfg, algo = "sha256") else NA_character_,
    residual_sha256 = inventory_row$residual_sha256)
  if (save_checkpoint) govc_atomic_save_rds(out, govc_checkpoint_path(inventory_row$pair_endpoint_key), compress = "xz")
  rm(d, versions); gc(verbose = FALSE)
  out
}

build_conditional_gaussian_remainder_fixture <- function(selected_row, pair_series,
                                                          tolerance = 1e-9) {
  use <- pair_series$timestamp >= as.POSIXct(selected_row$formation_start, tz = "Europe/London") &
    pair_series$timestamp <= as.POSIXct(selected_row$formation_end, tz = "Europe/London")
  x <- pair_series[use, , drop = FALSE]
  spread <- statistical_raw_spread(
    x$midpoint_y, x$midpoint_x, selected_row$alpha, selected_row$beta
  ) - selected_row$formation_centre
  dt <- x$active_dt_minutes
  previous <- c(NA_real_, head(spread, -1L))
  phi <- exp(-selected_row$kappa_per_active_minute * dt)
  eta <- spread - selected_row$ou_equilibrium -
    phi * (previous - selected_row$ou_equilibrium)
  variance_factor <- -expm1(-2 * selected_row$kappa_per_active_minute * dt) /
    (2 * selected_row$kappa_per_active_minute)
  sd_eta <- selected_row$gaussian_diffusion_scale * sqrt(variance_factor)
  accepted <- x$transition_valid %in% TRUE & is.finite(dt) & dt > 0 &
    is.finite(eta) & is.finite(sd_eta) & sd_eta > 0
  data.frame(
    next_time = x$timestamp[accepted], transition_index = which(accepted),
    gaussian_standardized_z = eta[accepted] / sd_eta[accepted],
    raw_remainder_eta = eta[accepted], active_dt_minutes = dt[accepted],
    session_id = x$calendar_session_date[accepted],
    accepted_segment_id = x$structural_segment_id[accepted],
    stringsAsFactors = FALSE
  )
}

conditional_dynamics_diagnostic_one <- function(selected_row, pair_series) {
  if (!requireNamespace("arrow", quietly = TRUE)) stop(
    "Package 'arrow' is required for the descriptive diagnostic.", call. = FALSE
  )
  residual <- build_conditional_gaussian_remainder_fixture(selected_row, pair_series)
  if (nrow(residual) < 3L) stop("Too few accepted positive-duration remainders.", call. = FALSE)
  path <- tempfile(fileext = ".parquet")
  on.exit(unlink(path), add = TRUE)
  arrow::write_parquet(residual, path)
  inventory <- data.frame(
    pair_endpoint_key = paste(selected_row$pair_id, selected_row$endpoint_session_date, sep = "|"),
    pair_id = selected_row$pair_id, endpoint_id = selected_row$endpoint_id,
    endpoint_session_date = as.Date(selected_row$endpoint_session_date),
    residual_path = path, residual_sha256 = unname(tools::sha256sum(path)),
    stringsAsFactors = FALSE
  )
  govc_window_diagnostics(inventory, save_checkpoint = FALSE)
}

build_conditional_dynamics_diagnostics <- function(selected_schedule,
                                                prepared_pair_series) {
  selected <- selected_schedule[selected_schedule$selected %in% TRUE, , drop = FALSE]
  objects <- lapply(seq_len(nrow(selected)), function(i) tryCatch(
    conditional_dynamics_diagnostic_one(
      selected[i, , drop = FALSE],
      prepared_pair_series[[as.character(selected$pair_id[[i]])]]
    ),
    error = function(e) list(summary = data.frame(
      pair_endpoint_key = paste(selected$pair_id[[i]], selected$endpoint_session_date[[i]], sep = "|"),
      pair_id = selected$pair_id[[i]], endpoint_id = selected$endpoint_id[[i]],
      observations = NA_integer_, primary_classification = "diagnostic_unavailable",
      warning_signed_mean = NA, warning_duration = NA,
      warning_event_concentration = NA, diagnostic_failure_reason = conditionMessage(e),
      stringsAsFactors = FALSE
    ))
  ))
  detail <- do.call(rbind, lapply(objects, `[[`, "summary"))
  summary <- data.frame(
    diagnostic = c(
      "persistent_conditional_scale_clustering", "signed_dependence",
      "event_concentration", "material_duration_dependence"
    ),
    pair_windows = c(
      sum(grepl("persistent conditional-scale clustering", detail$primary_classification), na.rm = TRUE),
      sum(detail$warning_signed_mean %in% TRUE, na.rm = TRUE),
      sum(detail$warning_event_concentration %in% TRUE, na.rm = TRUE),
      sum(detail$warning_duration %in% TRUE, na.rm = TRUE)
    ),
    denominator = nrow(detail), stringsAsFactors = FALSE
  )
  summary$percentage <- 100 * summary$pair_windows / summary$denominator
  list(window_diagnostics = detail, descriptive_summary = summary,
       role = "descriptive_only_excluded_from_family_selection")
}
