newey_west_mean <- function(x, lag = 10L) {
  x <- as.numeric(x[is.finite(x)]); n <- length(x)
  if (n < 2L) return(c(mean = if (n) mean(x) else NA_real_, se = NA_real_,
                        t = NA_real_, p = NA_real_, ci_low = NA_real_, ci_high = NA_real_))
  centred <- x - mean(x); l <- min(as.integer(lag), n - 1L)
  long_run <- sum(centred^2) / n
  if (l > 0L) for (j in seq_len(l)) {
    gamma <- sum(centred[(j + 1L):n] * centred[seq_len(n - j)]) / n
    long_run <- long_run + 2 * (1 - j / (l + 1)) * gamma
  }
  se <- sqrt(max(long_run, 0) / n); statistic <- if (se > 0) mean(x) / se else NA_real_
  c(mean = mean(x), se = se, t = statistic,
    p = if (is.finite(statistic)) 2 * stats::pnorm(-abs(statistic)) else NA_real_,
    ci_low = mean(x) - 1.96 * se, ci_high = mean(x) + 1.96 * se)
}

adjusted_skewness <- function(x) {
  x <- x[is.finite(x)]; n <- length(x); s <- stats::sd(x)
  if (n < 3L || !is.finite(s) || s == 0) return(NA_real_)
  n / ((n - 1) * (n - 2)) * sum(((x - mean(x)) / s)^3)
}

unbiased_excess_kurtosis <- function(x) {
  x <- x[is.finite(x)]; n <- length(x); s <- stats::sd(x)
  if (n < 4L || !is.finite(s) || s == 0) return(NA_real_)
  n * (n + 1) / ((n - 1) * (n - 2) * (n - 3)) * sum(((x - mean(x)) / s)^4) -
    3 * (n - 1)^2 / ((n - 2) * (n - 3))
}

maximum_time_underwater <- function(wealth) {
  underwater <- wealth < cummax(c(1, wealth))[-1L] - 1e-15
  if (!any(underwater)) return(0L)
  max(rle(underwater)$lengths[rle(underwater)$values])
}

hac_sharpe <- function(x, annualisation = 252L, lag = 10L) {
  x <- x[is.finite(x)]; n <- length(x)
  if (n < 2L) return(NA_real_)
  centred <- x - mean(x); l <- min(as.integer(lag), n - 1L)
  lrv <- sum(centred^2) / n
  if (l > 0L) for (j in seq_len(l)) {
    lrv <- lrv + 2 * (1 - j / (l + 1)) *
      sum(centred[(j + 1L):n] * centred[seq_len(n - j)]) / n
  }
  if (lrv <= 0) NA_real_ else sqrt(annualisation) * mean(x) / sqrt(lrv)
}

daily_performance_metrics <- function(returns, annualisation = 252L, hac_lag = 10L) {
  x <- as.numeric(returns[is.finite(returns)])
  if (!length(x)) stop("A performance series has no finite return.", call. = FALSE)
  wealth <- cumprod(1 + x); peak <- cummax(c(1, wealth))[-1L]
  drawdown <- wealth / peak - 1; sd_x <- stats::sd(x)
  downside <- sqrt(mean(pmin(x, 0)^2))
  q05 <- as.numeric(stats::quantile(x, .05, type = 8, names = FALSE))
  nw <- newey_west_mean(x, hac_lag)
  data.frame(
    observations = length(x), minimum = min(x),
    q25 = as.numeric(stats::quantile(x, .25, type = 8)), median = stats::median(x),
    q75 = as.numeric(stats::quantile(x, .75, type = 8)), maximum = max(x),
    daily_mean = mean(x), daily_sd = sd_x,
    adjusted_skewness = adjusted_skewness(x), excess_kurtosis = unbiased_excess_kurtosis(x),
    positive_return_share = mean(x > 0), zero_return_share = mean(x == 0),
    historical_var_05 = q05, historical_es_05 = mean(x[x <= q05]),
    maximum_drawdown = min(drawdown), maximum_time_underwater_sessions = maximum_time_underwater(wealth),
    annualised_arithmetic_mean = annualisation * mean(x),
    annualised_volatility = sqrt(annualisation) * sd_x,
    annualised_downside_deviation = sqrt(annualisation) * downside,
    iid_sharpe = if (sd_x > 0) sqrt(annualisation) * mean(x) / sd_x else NA_real_,
    hac_sharpe = hac_sharpe(x, annualisation, hac_lag),
    sortino = if (downside > 0) sqrt(annualisation) * mean(x) / downside else NA_real_,
    compounded_return = prod(1 + x) - 1,
    nw_mean = nw[["mean"]], nw_se = nw[["se"]], nw_t = nw[["t"]],
    nw_p_two_sided = nw[["p"]], nw_ci_low = nw[["ci_low"]], nw_ci_high = nw[["ci_high"]],
    stringsAsFactors = FALSE
  )
}

stationary_bootstrap_indices <- function(n, replications = 5000L,
                                         mean_block_length = 10,
                                         seed = 20260818L) {
  if (n < 1L) stop("Bootstrap length must be positive.", call. = FALSE)
  set.seed(as.integer(seed)); p <- 1 / mean_block_length
  replicate(as.integer(replications), {
    index <- integer(n); index[[1L]] <- sample.int(n, 1L)
    if (n > 1L) for (i in 2:n) index[[i]] <- if (stats::runif(1) < p) {
      sample.int(n, 1L)
    } else index[[i - 1L]] %% n + 1L
    index
  }, simplify = "matrix")
}

stationary_bootstrap_model_inference <- function(return_matrix,
                                                  replications = 5000L,
                                                  mean_block_length = 10,
                                                  seed = 20260818L,
                                                  annualisation = 252L) {
  x <- as.matrix(return_matrix)
  if (any(!is.finite(x))) stop("Aligned model-return matrix must be finite.", call. = FALSE)
  labels <- colnames(x); indices <- stationary_bootstrap_indices(nrow(x), replications,
                                                                 mean_block_length, seed)
  statistic <- function(v) {
    s <- stats::sd(v); if (s > 0) sqrt(annualisation) * mean(v) / s else NA_real_
  }
  sharpe <- sapply(seq_along(labels), function(j) apply(indices, 2L, function(ii) statistic(x[ii, j])))
  colnames(sharpe) <- labels
  model <- data.frame(
    model_label = labels,
    sharpe_ci_low = apply(sharpe, 2L, stats::quantile, .025, na.rm = TRUE),
    sharpe_ci_high = apply(sharpe, 2L, stats::quantile, .975, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  pairs <- utils::combn(seq_along(labels), 2L)
  contrast <- do.call(rbind, lapply(seq_len(ncol(pairs)), function(k) {
    a <- pairs[1L, k]; b <- pairs[2L, k]; d <- sharpe[, a] - sharpe[, b]
    p <- 2 * min(mean(d <= 0, na.rm = TRUE), mean(d >= 0, na.rm = TRUE))
    data.frame(model_left = labels[[a]], model_right = labels[[b]],
      sharpe_difference = statistic(x[, a]) - statistic(x[, b]),
      bootstrap_ci_low = stats::quantile(d, .025, na.rm = TRUE),
      bootstrap_ci_high = stats::quantile(d, .975, na.rm = TRUE),
      bootstrap_p_two_sided = min(1, p), stringsAsFactors = FALSE)
  }))
  # Kept as a separately labelled bootstrap family; the primary mean-return
  # Holm correction is computed on aligned HAC contrasts below.
  contrast$holm_p <- stats::p.adjust(contrast$bootstrap_p_two_sided, method = "holm")
  list(model_intervals = model, pairwise_sharpe_contrasts = contrast,
       common_indices = indices)
}

pairwise_hac_return_differences <- function(daily, lag = 10L) {
  labels <- sort(unique(as.character(daily$model_label)))
  if (length(labels) < 2L) return(data.frame())
  pairs <- utils::combn(labels, 2L, simplify = FALSE)
  out <- do.call(rbind, lapply(pairs, function(pair) {
    left <- daily[daily$model_label == pair[[1L]],
                  c("session_date", "return_committed"), drop = FALSE]
    right <- daily[daily$model_label == pair[[2L]],
                   c("session_date", "return_committed"), drop = FALSE]
    names(left)[[2L]] <- "left_return"; names(right)[[2L]] <- "right_return"
    aligned <- merge(left, right, by = "session_date", all = FALSE, sort = TRUE)
    aligned <- aligned[is.finite(aligned$left_return) &
                         is.finite(aligned$right_return), , drop = FALSE]
    z <- newey_west_mean(aligned$left_return - aligned$right_return, lag)
    data.frame(
      model_left = pair[[1L]], model_right = pair[[2L]],
      aligned_sessions = nrow(aligned), hac_lag = as.integer(lag),
      mean_return_difference = z[["mean"]], hac_se = z[["se"]],
      hac_t = z[["t"]], hac_p_two_sided = z[["p"]],
      hac_ci_low = z[["ci_low"]], hac_ci_high = z[["ci_high"]],
      stringsAsFactors = FALSE
    )
  }))
  out$holm_p_all_pairwise_mean_return_family <- stats::p.adjust(
    out$hac_p_two_sided, method = "holm"
  )
  out
}

performance_analysis <- function(daily, config) {
  daily$session_date <- as.Date(daily$session_date)
  split_returns <- split(daily$return_committed, daily$model_label)
  summary <- do.call(rbind, lapply(split_returns, daily_performance_metrics,
                                  annualisation = config$annualisation,
                                  hac_lag = config$newey_west_lag))
  summary$model_label <- rownames(summary); rownames(summary) <- NULL
  date_range <- do.call(rbind, lapply(split(daily$session_date, daily$model_label), function(x) {
    data.frame(first_session = min(x), last_session = max(x))
  }))
  date_range$model_label <- rownames(date_range); rownames(date_range) <- NULL
  summary <- merge(date_range, summary, by = "model_label", sort = FALSE)
  lags <- unique(c(5L, 9L, config$newey_west_lag, 20L))
  nw <- do.call(rbind, lapply(names(split_returns), function(label) do.call(rbind, lapply(lags, function(lag) {
    z <- newey_west_mean(split_returns[[label]], lag)
    data.frame(model_label = label, lag = lag, mean = z[["mean"]], se = z[["se"]],
               t = z[["t"]], p_two_sided = z[["p"]], ci_low = z[["ci_low"]],
               ci_high = z[["ci_high"]], stringsAsFactors = FALSE)
  }))))
  wide <- reshape(daily[c("session_date", "model_label", "return_committed")],
                  idvar = "session_date", timevar = "model_label", direction = "wide")
  wide <- wide[stats::complete.cases(wide), , drop = FALSE]
  matrix <- as.matrix(wide[setdiff(names(wide), "session_date")])
  colnames(matrix) <- sub("^return_committed\\.", "", colnames(matrix))
  bootstrap <- if (ncol(matrix) >= 2L && nrow(matrix) >= 2L) {
    stationary_bootstrap_model_inference(
      matrix, config$stationary_bootstrap_replications,
      config$stationary_bootstrap_mean_block,
      config$stationary_bootstrap_seed, config$annualisation
    )
  } else list(model_intervals = data.frame(), pairwise_sharpe_contrasts = data.frame(),
              common_indices = matrix(integer(), 0, 0))
  pairwise_hac <- pairwise_hac_return_differences(
    daily, config$newey_west_lag
  )
  list(summary = summary, newey_west_sensitivity = nw,
       pairwise_hac_return_differences = pairwise_hac,
       bootstrap_model_intervals = bootstrap$model_intervals,
       pairwise_sharpe_contrasts = bootstrap$pairwise_sharpe_contrasts)
}
