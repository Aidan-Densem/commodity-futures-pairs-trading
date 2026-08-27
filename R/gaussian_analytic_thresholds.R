gaussian_threshold_extract_fit_table <- function(fit_object) {
  if (is.data.frame(fit_object)) return(as.data.frame(fit_object, stringsAsFactors = FALSE))
  candidates <- list(
    fit_object$summary,
    fit_object$parameters_by_window,
    fit_object$summary_table
  )
  for (x in candidates) {
    if (is.data.frame(x)) return(as.data.frame(x, stringsAsFactors = FALSE))
  }
  stop("Could not resolve a Gaussian OU fit table from fit_object.", call. = FALSE)
}

gaussian_threshold_extract_parameters <- function(
    fit_object,
    window_index = NULL,
    window_name = NULL,
    require_clean_fit = FALSE,
    stationary_sd_tolerance = 1e-6
) {
  tab <- gaussian_threshold_extract_fit_table(fit_object)
  if (!is.null(window_name) && "Window_Name" %in% names(tab)) {
    hit <- which(as.character(tab$Window_Name) == as.character(window_name)[1L])
  } else if (!is.null(window_index) && "Window_Index" %in% names(tab)) {
    hit <- which(as.integer(tab$Window_Index) == as.integer(window_index)[1L])
  } else if (!is.null(window_index) && "Window" %in% names(tab)) {
    hit <- which(as.integer(tab$Window) == as.integer(window_index)[1L])
  } else {
    stop("Supply window_index or window_name and a matching fit table column.", call. = FALSE)
  }
  if (length(hit) != 1L) {
    stop("Expected exactly one Gaussian fit row; found ", length(hit), ".", call. = FALSE)
  }
  row <- tab[hit, , drop = FALSE]
  if ("Success" %in% names(row) && !isTRUE(as.logical(row$Success[1L]))) {
    stop("Input Gaussian fit was not successful for ", row$Window_Name[1L] %||% hit, ".", call. = FALSE)
  }
  if (isTRUE(require_clean_fit) &&
      "Review_Suspicious_First_Pass" %in% names(row) &&
      isTRUE(as.logical(row$Review_Suspicious_First_Pass[1L]))) {
    stop("Input Gaussian fit is marked suspicious and require_clean_fit = TRUE.", call. = FALSE)
  }
  source <- list(mu = NA_character_, sigma = NA_character_, lambda = NA_character_)
  if (all(c("Mu", "Sigma_Per_Sqrt_Minute", "Lambda_Per_Minute") %in% names(row))) {
    mu <- as.numeric(row$Mu[1L])
    sigma <- as.numeric(row$Sigma_Per_Sqrt_Minute[1L])
    lambda <- as.numeric(row$Lambda_Per_Minute[1L])
    source <- list(mu = "Mu", sigma = "Sigma_Per_Sqrt_Minute", lambda = "Lambda_Per_Minute")
  } else if (all(c("Mu_Model_Scale", "Sigma_Per_Sqrt_Minute_Model_Scale", "Spread_Scale", "Lambda_Per_Minute") %in% names(row))) {
    scale <- as.numeric(row$Spread_Scale[1L])
    mu <- as.numeric(row$Mu_Model_Scale[1L]) * scale
    sigma <- as.numeric(row$Sigma_Per_Sqrt_Minute_Model_Scale[1L]) * scale
    lambda <- as.numeric(row$Lambda_Per_Minute[1L])
    source <- list(mu = "Mu_Model_Scale*Spread_Scale", sigma = "Sigma_Model*Spread_Scale", lambda = "Lambda_Per_Minute")
  } else {
    stop("Gaussian fit row does not contain usable original- or model-scale parameter columns.", call. = FALSE)
  }
  if (!all(is.finite(c(mu, sigma, lambda))) || sigma <= 0 || lambda <= 0) {
    stop("Gaussian fit parameters are not finite/admissible.", call. = FALSE)
  }
  stationary_sd <- sigma / sqrt(2 * lambda)
  if ("Stationary_SD" %in% names(row) && is.finite(as.numeric(row$Stationary_SD[1L]))) {
    stored <- as.numeric(row$Stationary_SD[1L])
    if (abs(stored - stationary_sd) > stationary_sd_tolerance * (1 + abs(stationary_sd))) {
      warning("Stored Stationary_SD differs from sigma/sqrt(2*lambda). Recalculated value is used.", call. = FALSE)
    }
  }
  list(
    row = row,
    window_index = as.integer(row$Window_Index[1L] %||% row$Window[1L] %||% window_index),
    window_name = as.character(row$Window_Name[1L] %||% window_name %||% paste0("window_", window_index)),
    mu = mu,
    lambda = lambda,
    sigma = sigma,
    stationary_sd = stationary_sd,
    source = source
  )
}

gou_threshold_bidask_time_key <- function(x, label = "Dates") {
  as.numeric(as.POSIXct(x, tz = "Europe/London"))
}

gou_threshold_resolve_bidask_columns <- function(
    quote_source,
    co_bid_col = NULL,
    co_ask_col = NULL,
    cl_bid_col = NULL,
    cl_ask_col = NULL
) {
  nms <- names(quote_source)
  pick <- function(user, candidates, label) {
    if (!is.null(user)) {
      if (!user %in% nms) stop(label, " column not found: ", user, call. = FALSE)
      return(user)
    }
    hit <- candidates[candidates %in% nms]
    if (length(hit) == 0L) stop("Could not resolve ", label, " column.", call. = FALSE)
    hit[1L]
  }
  list(
    co_bid = pick(co_bid_col, c("CO1_Bid", "CO_Bid"), "CO bid"),
    co_ask = pick(co_ask_col, c("CO1_Ask", "CO_Ask"), "CO ask"),
    cl_bid = pick(cl_bid_col, c("CL1_Bid", "CL_Bid"), "CL bid"),
    cl_ask = pick(cl_ask_col, c("CL1_Ask", "CL_Ask"), "CL ask")
  )
}

estimate_gaussian_round_trip_cost_from_formation_bidask <- function(
    windows_object,
    spread_spec,
    window_index = NULL,
    window_name = NULL,
    bidask_data = NULL,
    windows_date_col = "Dates",
    bidask_date_col = "Dates",
    co_bid_col = NULL,
    co_ask_col = NULL,
    cl_bid_col = NULL,
    cl_ask_col = NULL,
    statistic = c("median", "mean", "trimmed_mean"),
    trim = 0.05,
    min_valid_quotes = 50L,
    continue_on_error = FALSE
) {
  statistic <- match.arg(statistic)
  tryCatch({
    labels <- gou_window_labels(windows_object$windows)
    if (!is.null(window_name)) {
      index <- match(as.character(window_name)[1L], labels)
      if (is.na(index)) stop("Unknown window_name: ", window_name, call. = FALSE)
    } else {
      index <- as.integer(window_index)[1L]
    }
    if (!is.finite(index) || index < 1L || index > length(labels)) {
      stop("window_index is outside windows_object$windows.", call. = FALSE)
    }
    label <- labels[index]
    win <- windows_object$windows[[index]]
    formation_rows <- gou_window_rows(win, sample = "estimation")
    pm <- gou_match_spread_parameters(spread_spec, win, label, index)
    beta <- as.numeric(pm$row$Beta[1L])
    alpha <- as.numeric(pm$row$Alpha[1L])
    quote_source <- bidask_data %||% windows_object$data
    cols <- gou_threshold_resolve_bidask_columns(quote_source, co_bid_col, co_ask_col, cl_bid_col, cl_ask_col)
    if (!windows_date_col %in% names(windows_object$data)) {
      stop("windows_object$data is missing date column: ", windows_date_col, call. = FALSE)
    }
    if (!bidask_date_col %in% names(quote_source)) {
      stop("bidask_data is missing date column: ", bidask_date_col, call. = FALSE)
    }
    qkey <- gou_threshold_bidask_time_key(quote_source[[bidask_date_col]], "Bid/ask dates")
    if (anyDuplicated(qkey[is.finite(qkey)])) {
      stop("Bid/ask source has duplicate timestamps.", call. = FALSE)
    }
    fkey <- gou_threshold_bidask_time_key(windows_object$data[[windows_date_col]][formation_rows], "Formation dates")
    matched <- match(fkey, qkey)
    quotes <- data.frame(
      CO_Bid = as.numeric(quote_source[[cols$co_bid]][matched]),
      CO_Ask = as.numeric(quote_source[[cols$co_ask]][matched]),
      CL_Bid = as.numeric(quote_source[[cols$cl_bid]][matched]),
      CL_Ask = as.numeric(quote_source[[cols$cl_ask]][matched]),
      stringsAsFactors = FALSE
    )
    valid <- with(
      quotes,
      is.finite(CO_Bid) & is.finite(CO_Ask) & CO_Bid > 0 & CO_Ask > 0 & CO_Ask >= CO_Bid &
        is.finite(CL_Bid) & is.finite(CL_Ask) & CL_Bid > 0 & CL_Ask > 0 & CL_Ask >= CL_Bid
    )
    n_valid <- sum(valid)
    n_form <- length(formation_rows)
    if (n_valid < min_valid_quotes) {
      stop("Only ", n_valid, " valid formation bid/ask rows; minimum is ", min_valid_quotes, ".", call. = FALSE)
    }
    quotes <- quotes[valid, , drop = FALSE]
    co_cost <- log(quotes$CO_Ask / quotes$CO_Bid)
    cl_cost <- log(quotes$CL_Ask / quotes$CL_Bid)
    total_cost <- co_cost + abs(beta) * cl_cost
    agg <- function(x) switch(statistic, median = stats::median(x), mean = mean(x), trimmed_mean = mean(x, trim = trim))
    co_agg <- agg(co_cost)
    cl_agg <- agg(cl_cost)
    total_agg <- agg(total_cost)
    data.frame(
      Window_Index = index,
      Window_Name = label,
      Hedge_Alpha = alpha,
      Hedge_Beta = beta,
      Cost_CO_RoundTrip = co_agg,
      Cost_CL_RoundTrip = cl_agg,
      Cost_CL_Hedge_Weighted_RoundTrip = abs(beta) * cl_agg,
      Cost_Total_RoundTrip = total_agg,
      Cost_Statistic = statistic,
      Cost_Trim = trim,
      N_Formation_Rows = n_form,
      N_Valid_BidAsk_Rows = n_valid,
      Share_Valid_BidAsk_Rows = n_valid / n_form,
      Min_CO_BidAsk_Cost = min(co_cost),
      Median_CO_BidAsk_Cost = stats::median(co_cost),
      Max_CO_BidAsk_Cost = max(co_cost),
      Min_CL_BidAsk_Cost = min(cl_cost),
      Median_CL_BidAsk_Cost = stats::median(cl_cost),
      Max_CL_BidAsk_Cost = max(cl_cost),
      CO_Bid_Column = cols$co_bid,
      CO_Ask_Column = cols$co_ask,
      CL_Bid_Column = cols$cl_bid,
      CL_Ask_Column = cols$cl_ask,
      Testing_BidAsk_Used_For_Costs = FALSE,
      Success = TRUE,
      Warning = "",
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    if (!isTRUE(continue_on_error)) stop(e)
    data.frame(
      Window_Index = as.integer(window_index %||% NA_integer_),
      Window_Name = as.character(window_name %||% NA_character_),
      Hedge_Alpha = NA_real_,
      Hedge_Beta = NA_real_,
      Cost_CO_RoundTrip = NA_real_,
      Cost_CL_RoundTrip = NA_real_,
      Cost_CL_Hedge_Weighted_RoundTrip = NA_real_,
      Cost_Total_RoundTrip = NA_real_,
      Cost_Statistic = statistic,
      Cost_Trim = trim,
      N_Formation_Rows = NA_integer_,
      N_Valid_BidAsk_Rows = 0L,
      Share_Valid_BidAsk_Rows = NA_real_,
      Testing_BidAsk_Used_For_Costs = FALSE,
      Success = FALSE,
      Warning = conditionMessage(e),
      stringsAsFactors = FALSE
    )
  })
}

gou_threshold_resolve_costs <- function(
    cost_mode = c("frictionless", "fixed_spread_units", "formation_bidask_estimate"),
    round_trip_cost = 0,
    ...
) {
  cost_mode <- match.arg(cost_mode)
  if (identical(cost_mode, "frictionless")) {
    return(list(cost = 0, diagnostics = NULL, source = "frictionless"))
  }
  if (identical(cost_mode, "fixed_spread_units")) {
    cost <- gou_scalar(round_trip_cost, "round_trip_cost", lower = 0)
    return(list(cost = cost, diagnostics = NULL, source = "fixed_spread_units"))
  }
  diag <- estimate_gaussian_round_trip_cost_from_formation_bidask(...)
  if (!isTRUE(diag$Success[1L])) stop(diag$Warning[1L], call. = FALSE)
  list(cost = as.numeric(diag$Cost_Total_RoundTrip[1L]), diagnostics = diag, source = "formation_bidask_estimate")
}

gou_zl_D_and_Dprime <- function(a, series_tol = 1e-12, max_terms = 10000L) {
  if (!is.finite(a) || a < 0) stop("a must be finite and non-negative.", call. = FALSE)
  term_D <- sqrt(2 * pi) * a
  sum_D <- term_D
  term_Dprime <- sqrt(pi)
  sum_Dprime <- term_Dprime
  converged <- isTRUE(a == 0)
  n_used <- 0L
  if (!converged) {
    for (n in 0:(as.integer(max_terms) - 1L)) {
      next_D <- term_D * (a^2 * (2 * n + 1)) / ((2 * n + 2) * (2 * n + 3))
      next_Dp <- term_Dprime * (a^2 / (2 * n + 2))
      if (!all(is.finite(c(next_D, next_Dp)))) break
      sum_D <- sum_D + next_D
      sum_Dprime <- sum_Dprime + next_Dp
      n_used <- n + 1L
      if (abs(next_D) < series_tol * (1 + abs(sum_D)) &&
          abs(next_Dp) < series_tol * (1 + abs(sum_Dprime))) {
        converged <- TRUE
        break
      }
      term_D <- next_D
      term_Dprime <- next_Dp
    }
  }
  list(
    D = 0.5 * sum_D,
    Dprime = sqrt(2) / 2 * sum_Dprime,
    Series_Converged = converged,
    Series_Terms_Used = as.integer(n_used)
  )
}

gou_zl_residual <- function(a, effective_cost_dimensionless, series_tol = 1e-12, max_terms = 10000L) {
  vals <- gou_zl_D_and_Dprime(a, series_tol = series_tol, max_terms = max_terms)
  vals$D - (a - effective_cost_dimensionless) * vals$Dprime
}

gou_zl_objective_dimensionless <- function(
    a,
    actual_cost_dimensionless,
    criterion = c("half_cycle_mean_exit", "endres_band_to_band"),
    series_tol = 1e-12,
    max_terms = 10000L
) {
  criterion <- match.arg(criterion)
  vals <- gou_zl_D_and_Dprime(a, series_tol = series_tol, max_terms = max_terms)
  if (!isTRUE(vals$Series_Converged) || !is.finite(vals$D) || vals$D <= 0) return(NA_real_)
  if (identical(criterion, "half_cycle_mean_exit")) {
    profit <- a - actual_cost_dimensionless
    time <- vals$D
  } else {
    profit <- 2 * a - actual_cost_dimensionless
    time <- 2 * vals$D
  }
  if (!is.finite(profit) || profit <= 0) return(NA_real_)
  profit / time
}

gou_solve_zl_root <- function(
    cost_dimensionless,
    criterion = c("half_cycle_mean_exit", "endres_band_to_band"),
    initial_max_threshold_z = 8,
    max_threshold_z = 32,
    bracket_expansion_factor = 1.5,
    root_tol = 1e-10,
    series_tol = 1e-12,
    max_series_terms = 10000L,
    solver = c("root_then_optimize", "root", "optimize")
) {
  criterion <- match.arg(criterion)
  solver <- match.arg(solver)
  eff_cost <- if (identical(criterion, "endres_band_to_band")) cost_dimensionless / 2 else cost_dimensionless
  if (eff_cost <= 0) {
    limit_obj <- sqrt(2 / pi)
    return(list(
      success = TRUE,
      a = 0,
      effective_cost = eff_cost,
      solver = "zero_cost_limit",
      root_converged = TRUE,
      root_residual = 0,
      bracket_expansions = 0L,
      final_upper = initial_max_threshold_z,
      evaluations = 0L,
      fallback_used = FALSE,
      boundary_optimum = TRUE,
      objective_dimensionless_limit = limit_obj,
      series = gou_zl_D_and_Dprime(0, series_tol, max_series_terms)
    ))
  }
  lower <- eff_cost + max(1e-12, eff_cost * 1e-10)
  upper <- max(initial_max_threshold_z, lower * 1.25)
  hard <- max(max_threshold_z, upper)
  evals <- 0L
  f <- function(a) {
    evals <<- evals + 1L
    gou_zl_residual(a, eff_cost, series_tol = series_tol, max_terms = max_series_terms)
  }
  fl <- f(lower)
  fu <- f(upper)
  expansions <- 0L
  while (is.finite(fl) && is.finite(fu) && fl * fu > 0 && upper < hard) {
    upper <- min(hard, upper * bracket_expansion_factor)
    fu <- f(upper)
    expansions <- expansions + 1L
  }
  root_ok <- is.finite(fl) && is.finite(fu) && fl * fu <= 0
  fallback <- FALSE
  if (root_ok && solver %in% c("root_then_optimize", "root")) {
    ur <- stats::uniroot(f, lower = lower, upper = upper, tol = root_tol)
    a <- ur$root
    res <- f(a)
    sol <- "uniroot"
  } else if (solver %in% c("root_then_optimize", "optimize")) {
    fallback <- TRUE
    obj <- function(a) {
      -gou_zl_objective_dimensionless(
        a,
        actual_cost_dimensionless = cost_dimensionless,
        criterion = criterion,
        series_tol = series_tol,
        max_terms = max_series_terms
      )
    }
    opt <- stats::optimize(obj, interval = c(lower, hard), tol = root_tol)
    a <- opt$minimum
    res <- f(a)
    sol <- "optimize"
  } else {
    return(list(success = FALSE, message = "Could not bracket Zeng-Lee root."))
  }
  series <- gou_zl_D_and_Dprime(a, series_tol, max_series_terms)
  list(
    success = TRUE,
    a = a,
    effective_cost = eff_cost,
    solver = sol,
    root_converged = root_ok || fallback,
    root_residual = res,
    bracket_expansions = as.integer(expansions),
    final_upper = upper,
    evaluations = as.integer(evals),
    fallback_used = fallback,
    boundary_optimum = FALSE,
    series = series
  )
}

gou_erfi_series <- function(x, tol = 1e-12, max_terms = 10000L) {
  sapply(as.numeric(x), function(xx) {
    term <- xx
    sumv <- term
    converged <- xx == 0
    if (!converged) {
      for (n in 0:(max_terms - 1L)) {
        next_term <- term * xx^2 * (2 * n + 1) / ((n + 1) * (2 * n + 3))
        if (!is.finite(next_term)) return(sign(xx) * Inf)
        sumv <- sumv + next_term
        if (abs(next_term) < tol * (1 + abs(sumv))) {
          converged <- TRUE
          break
        }
        term <- next_term
      }
    }
    2 / sqrt(pi) * sumv
  })
}

gou_bertram_expected_time <- function(d, lambda, sigma) {
  z <- d * sqrt(lambda) / sigma
  (pi / lambda) * (gou_erfi_series(z) - gou_erfi_series(-z))
}

gou_bertram_objective <- function(d, lambda, sigma, cost) {
  profit <- 2 * d - cost
  if (!is.finite(profit) || profit <= 0) return(NA_real_)
  time <- gou_bertram_expected_time(d, lambda, sigma)
  if (!is.finite(time) || time <= 0) return(NA_real_)
  profit / time
}

gou_make_threshold_profile <- function(
    window_index,
    window_name,
    criterion,
    analytic_method,
    mu,
    stationary_sd,
    lambda,
    cost,
    a_star,
    max_z,
    profile_points,
    series_tol,
    max_series_terms
) {
  zseq <- seq(0, max_z, length.out = max(10L, as.integer(profile_points)))
  zseq <- sort(unique(c(zseq, a_star)))
  rows <- lapply(seq_along(zseq), function(i) {
    z <- zseq[i]
    d <- z * stationary_sd
    if (criterion == "bertram_expected_return") {
      obj <- gou_bertram_objective(d, lambda, stationary_sd * sqrt(2 * lambda), cost)
      et <- gou_bertram_expected_time(d, lambda, stationary_sd * sqrt(2 * lambda))
      profit <- 2 * d - cost
    } else {
      vals <- gou_zl_D_and_Dprime(z, series_tol, max_series_terms)
      if (criterion == "half_cycle_mean_exit") {
        profit <- d - cost
        et <- vals$D / lambda
      } else {
        profit <- 2 * d - cost
        et <- 2 * vals$D / lambda
      }
      obj <- if (is.finite(profit) && profit > 0 && is.finite(et) && et > 0) profit / et else NA_real_
    }
    data.frame(
      Window_Index = window_index,
      Window_Name = window_name,
      Criterion = criterion,
      Analytic_Method = analytic_method,
      Candidate_ID = i,
      Search_Stage = "analytic_profile",
      Threshold_Z = z,
      d_plus = d,
      d_minus = d,
      c_plus = 0,
      c_minus = 0,
      Upper_Entry = mu + d,
      Lower_Entry = mu - d,
      Upper_Exit = mu,
      Lower_Exit = mu,
      Valid_Candidate = is.finite(obj),
      Invalid_Reason = if (is.finite(obj)) NA_character_ else "nonpositive_profit_or_invalid_time",
      Objective = obj,
      Expected_Profit = profit,
      Expected_Time = et,
      Selected_Final = abs(z - a_star) < 1e-10,
      stringsAsFactors = FALSE
    )
  })
  gou_bind_rows_fill(rows)
}

optimise_gaussian_ou_thresholds_analytic <- function(
    fit_object,
    windows_object,
    spread_spec,
    prepared_pair = NULL,
    windows = NULL,
    window_names = NULL,
    criterion = c("half_cycle_mean_exit", "endres_band_to_band", "bertram_expected_return"),
    threshold_mode = "symmetric",
    objective_mode = "expected_profit_per_expected_time",
    mean_level_source = c("fitted_mu", "zero", "formation_mean", "user"),
    mean_level = NULL,
    cost_mode = c("frictionless", "fixed_spread_units", "formation_bidask_estimate"),
    round_trip_cost = 0,
    bidask_data = NULL,
    bidask_date_col = "Dates",
    co_bid_col = NULL,
    co_ask_col = NULL,
    cl_bid_col = NULL,
    cl_ask_col = NULL,
    bidask_cost_stat = c("median", "mean", "trimmed_mean"),
    bidask_cost_trim = 0.05,
    min_valid_bidask_quotes = 50L,
    solver = c("root_then_optimize", "root", "optimize"),
    initial_max_threshold_z = 8,
    max_threshold_z = 32,
    bracket_expansion_factor = 1.5,
    root_tol = 1e-10,
    objective_tol = 1e-10,
    series_tol = 1e-12,
    max_series_terms = 10000L,
    keep_profile = TRUE,
    profile_points = 200L,
    require_clean_fit = FALSE,
    continue_on_error = TRUE,
    verbose = TRUE
) {
  criterion <- match.arg(criterion)
  mean_level_source <- match.arg(mean_level_source)
  cost_mode <- match.arg(cost_mode)
  bidask_cost_stat <- match.arg(bidask_cost_stat)
  solver <- match.arg(solver)
  if (!identical(threshold_mode, "symmetric")) stop("Only threshold_mode = 'symmetric' is currently implemented.", call. = FALSE)
  labels <- gou_window_labels(windows_object$windows)
  selected <- if (!is.null(window_names)) {
    hit <- match(window_names, labels)
    if (anyNA(hit)) stop("Unknown window name(s): ", paste(window_names[is.na(hit)], collapse = ", "), call. = FALSE)
    hit
  } else if (!is.null(windows)) {
    as.integer(windows)
  } else {
    seq_along(labels)
  }
  one <- function(idx) {
    start <- proc.time()[["elapsed"]]
    label <- labels[idx]
    tryCatch({
      pars <- gaussian_threshold_extract_parameters(
        fit_object,
        window_index = idx,
        window_name = label,
        require_clean_fit = require_clean_fit
      )
      mu <- switch(
        mean_level_source,
        fitted_mu = pars$mu,
        zero = 0,
        user = gou_scalar(mean_level, "mean_level"),
        formation_mean = {
          sp <- gou_reconstruct_window_spread(windows_object, spread_spec, idx, sample = "estimation")
          mean(sp$Spread, na.rm = TRUE)
        }
      )
      cost_res <- gou_threshold_resolve_costs(
        cost_mode = cost_mode,
        round_trip_cost = round_trip_cost,
        windows_object = windows_object,
        spread_spec = spread_spec,
        window_index = idx,
        window_name = label,
        bidask_data = bidask_data,
        bidask_date_col = bidask_date_col,
        co_bid_col = co_bid_col,
        co_ask_col = co_ask_col,
        cl_bid_col = cl_bid_col,
        cl_ask_col = cl_ask_col,
        statistic = bidask_cost_stat,
        trim = bidask_cost_trim,
        min_valid_quotes = min_valid_bidask_quotes
      )
      cost <- cost_res$cost
      lambda <- pars$lambda
      sigma <- pars$sigma
      sd_stat <- pars$stationary_sd
      cdim <- cost / sd_stat
      method <- switch(
        criterion,
        half_cycle_mean_exit = "zeng_lee_conventional",
        endres_band_to_band = "zeng_lee_new_optimal_rule",
        bertram_expected_return = "bertram_2010_expected_return"
      )
      exit_mode <- switch(criterion, half_cycle_mean_exit = "mean", endres_band_to_band = "opposite_band", bertram_expected_return = "opposite_symmetric_band")
      time_mode <- switch(criterion, half_cycle_mean_exit = "wait_plus_holding", endres_band_to_band = "band_passage", bertram_expected_return = "bertram_cycle")
      warnings <- character()
      if (criterion %in% c("half_cycle_mean_exit", "endres_band_to_band")) {
        sol <- gou_solve_zl_root(
          cost_dimensionless = cdim,
          criterion = criterion,
          initial_max_threshold_z = initial_max_threshold_z,
          max_threshold_z = max_threshold_z,
          bracket_expansion_factor = bracket_expansion_factor,
          root_tol = root_tol,
          series_tol = series_tol,
          max_series_terms = max_series_terms,
          solver = solver
        )
        if (!isTRUE(sol$success)) stop(sol$message %||% "Zeng-Lee solver failed.", call. = FALSE)
        z <- sol$a
        d <- z * sd_stat
        vals <- sol$series
        if (criterion == "half_cycle_mean_exit") {
          expected_profit <- d - cost
          expected_time <- vals$D / lambda
          upper_exit <- mu
          lower_exit <- mu
          upper_band <- mu + d
          lower_band <- mu - d
          long_entry <- lower_band
          long_exit <- mu
          short_entry <- upper_band
          short_exit <- mu
          immediate <- FALSE
        } else {
          expected_profit <- 2 * d - cost
          expected_time <- 2 * vals$D / lambda
          upper_exit <- mu - d
          lower_exit <- mu + d
          upper_band <- mu + d
          lower_band <- mu - d
          long_entry <- lower_band
          long_exit <- upper_band
          short_entry <- upper_band
          short_exit <- lower_band
          immediate <- TRUE
        }
        root_resid <- sol$root_residual
        series_conv <- vals$Series_Converged
        series_terms <- vals$Series_Terms_Used
        solver_used <- sol$solver
        bracket_exp <- sol$bracket_expansions
        final_upper <- sol$final_upper
        evals <- sol$evaluations
        fallback <- sol$fallback_used
        boundary <- sol$boundary_optimum
      } else {
        lower <- max(cost / 2 + 1e-12, 1e-12)
        upper <- max_threshold_z * sd_stat
        obj <- function(d) -gou_bertram_objective(d, lambda, sigma, cost)
        opt <- stats::optimize(obj, c(lower, upper), tol = root_tol)
        d <- opt$minimum
        z <- d / sd_stat
        expected_profit <- 2 * d - cost
        expected_time <- gou_bertram_expected_time(d, lambda, sigma)
        upper_band <- mu + d
        lower_band <- mu - d
        upper_exit <- lower_band
        lower_exit <- upper_band
        long_entry <- lower_band
        long_exit <- upper_band
        short_entry <- upper_band
        short_exit <- lower_band
        immediate <- FALSE
        root_resid <- NA_real_
        series_conv <- NA
        series_terms <- NA_integer_
        solver_used <- "optimize"
        bracket_exp <- 0L
        final_upper <- max_threshold_z
        evals <- NA_integer_
        fallback <- FALSE
        boundary <- FALSE
      }
      objective <- expected_profit / expected_time
      if (!is.finite(objective) || objective <= 0) warnings <- c(warnings, "nonpositive_or_invalid_objective")
      left_z <- max(0, z * 0.99)
      right_z <- z * 1.01 + 1e-12
      profile <- if (isTRUE(keep_profile)) {
        gou_make_threshold_profile(
          window_index = idx,
          window_name = label,
          criterion = criterion,
          analytic_method = method,
          mu = mu,
          stationary_sd = sd_stat,
          lambda = lambda,
          cost = cost,
          a_star = z,
          max_z = max(max(initial_max_threshold_z, z * 1.5), 1),
          profile_points = profile_points,
          series_tol = series_tol,
          max_series_terms = max_series_terms
        )
      } else {
        NULL
      }
      check_obj <- function(zz) {
        dd <- zz * sd_stat
        if (criterion == "bertram_expected_return") {
          gou_bertram_objective(dd, lambda, sigma, cost)
        } else {
          gou_zl_objective_dimensionless(zz, cdim, criterion, series_tol, max_series_terms) * sd_stat * lambda
        }
      }
      left_obj <- check_obj(left_z)
      right_obj <- check_obj(right_z)
      local_max <- isTRUE(is.finite(objective) &&
        (!is.finite(left_obj) || objective >= left_obj - objective_tol) &&
        (!is.finite(right_obj) || objective >= right_obj - objective_tol))
      if (!local_max) warnings <- c(warnings, "local_maximum_check_failed")
      cost_diag <- cost_res$diagnostics
      if (is.null(cost_diag)) {
        cost_diag <- data.frame(
          Hedge_Beta = NA_real_,
          Cost_CO_RoundTrip = NA_real_,
          Cost_CL_RoundTrip = NA_real_,
          Cost_CL_Hedge_Weighted_RoundTrip = NA_real_,
          Cost_Total_RoundTrip = cost,
          Cost_Statistic = NA_character_,
          Cost_Trim = NA_real_,
          N_Formation_Rows = NA_integer_,
          N_Valid_BidAsk_Rows = NA_integer_,
          Share_Valid_BidAsk_Rows = NA_real_,
          Testing_BidAsk_Used_For_Costs = FALSE,
          stringsAsFactors = FALSE
        )
      }
      runtime <- proc.time()[["elapsed"]] - start
      summary <- data.frame(
        Window_Index = idx,
        Window_ID = as.character(idx),
        Window = idx,
        Window_Name = label,
        Criterion = criterion,
        Analytic_Method = method,
        Threshold_Mode = threshold_mode,
        Exit_Mode = exit_mode,
        Time_Mode = time_mode,
        Objective_Mode = objective_mode,
        Success = TRUE,
        Warnings = gou_collapse_warnings(warnings),
        Runtime_Seconds = runtime,
        Mean_Level = mu,
        Mean_Level_Source = mean_level_source,
        Gaussian_Mu = pars$mu,
        OU_Lambda = lambda,
        Gaussian_Sigma = sigma,
        OU_Half_Life_Active_Minutes = log(2) / lambda,
        Fitted_Stationary_SD = sd_stat,
        Used_Original_Scale_Parameters = TRUE,
        Fit_Parameter_Source = paste(unlist(pars$source), collapse = ";"),
        Input_Fit_Success = TRUE,
        Input_Fit_Review_Status = pars$row$Review_First_Pass_Reasons[1L] %||% "",
        Optimal_d_plus = d,
        Optimal_d_minus = d,
        Optimal_c_plus = 0,
        Optimal_c_minus = 0,
        Upper_Entry = mu + d,
        Lower_Entry = mu - d,
        Upper_Exit = upper_exit,
        Lower_Exit = lower_exit,
        Upper_Band = upper_band,
        Lower_Band = lower_band,
        Long_Entry = long_entry,
        Long_Exit = long_exit,
        Short_Entry = short_entry,
        Short_Exit = short_exit,
        Immediate_Reversal_At_Opposite_Band = immediate,
        Optimal_Threshold_Z = z,
        Transaction_Cost_Dimensionless = cdim,
        Effective_Solver_Cost_Dimensionless = if (criterion == "endres_band_to_band") cdim / 2 else cdim,
        Objective = objective,
        Expected_Profit = expected_profit,
        Expected_Time = expected_time,
        Expected_Cycle_Time_Minutes = expected_time,
        Expected_Return_Per_Minute_One_Sided = objective,
        Expected_Return_Per_Minute_Two_Sided_Idealised = NA_real_,
        No_Trade_Optimal = FALSE,
        Nonpositive_Optimal_Objective = !is.finite(objective) || objective <= 0,
        Analytic_Solver = solver_used,
        Analytic_Root_Converged = TRUE,
        Analytic_Root_Residual = root_resid,
        Analytic_Equation_Label = if (criterion == "endres_band_to_band") "Zeng-Lee Eq23 / Eq20 with c/2" else if (criterion == "half_cycle_mean_exit") "Zeng-Lee Eq20" else "Bertram expected-return optimize",
        Analytic_Initial_Upper_Bound = initial_max_threshold_z,
        Analytic_Final_Upper_Bound = final_upper,
        Analytic_Bracket_Expansions = bracket_exp,
        Analytic_Function_Evaluations = evals,
        Analytic_Fallback_Used = fallback,
        Series_Converged = series_conv,
        Series_Terms_Used = series_terms,
        Objective_Left_Check = left_obj,
        Objective_At_Optimum = objective,
        Objective_Right_Check = right_obj,
        Local_Maximum_Verified = local_max,
        Boundary_Optimum = boundary,
        Cost_Mode = cost_mode,
        Cost_Source = cost_res$source,
        BidAsk_Cost_Statistic = bidask_cost_stat,
        BidAsk_Cost_Trim = bidask_cost_trim,
        Hedge_Beta_For_Cost = cost_diag$Hedge_Beta[1L],
        Cost_CO_RoundTrip = cost_diag$Cost_CO_RoundTrip[1L],
        Cost_CL_RoundTrip = cost_diag$Cost_CL_RoundTrip[1L],
        Cost_CL_Hedge_Weighted_RoundTrip = cost_diag$Cost_CL_Hedge_Weighted_RoundTrip[1L],
        Cost_Total_RoundTrip = cost,
        N_Formation_BidAsk_Rows = cost_diag$N_Formation_Rows[1L],
        N_Valid_BidAsk_Rows = cost_diag$N_Valid_BidAsk_Rows[1L],
        Share_Valid_BidAsk_Rows = cost_diag$Share_Valid_BidAsk_Rows[1L],
        Testing_BidAsk_Used_For_Costs = FALSE,
        Round_Trip_Cost_Plus = cost,
        Round_Trip_Cost_Minus = cost,
        Round_Trip_Cost_Lower_To_Upper = cost,
        Round_Trip_Cost_Upper_To_Lower = cost,
        N_Paths = NA_integer_,
        Random_Seed = NA_integer_,
        Common_Random_Numbers = FALSE,
        Simulation_Backend = "not_used_analytic",
        Threshold_Evaluator_Actual = "analytic_first_passage",
        Censoring_Probability = NA_real_,
        Forced_Exit_Probability = NA_real_,
        Entry_Probability = NA_real_,
        Exit_Probability = NA_real_,
        Objective_SE = NA_real_,
        Expected_Profit_SE = NA_real_,
        Expected_Time_SE = NA_real_,
        Monte_Carlo_SE_Not_Applicable = TRUE,
        stringsAsFactors = FALSE
      )
      list(summary = summary, candidate_results = profile, diagnostics = list(cost = cost_diag))
    }, error = function(e) {
      if (!isTRUE(continue_on_error)) stop(e)
      list(summary = data.frame(
        Window_Index = idx,
        Window_ID = as.character(idx),
        Window = idx,
        Window_Name = label,
        Criterion = criterion,
        Analytic_Method = NA_character_,
        Success = FALSE,
        Warnings = conditionMessage(e),
        Objective = NA_real_,
        Cost_Mode = cost_mode,
        Testing_BidAsk_Used_For_Costs = FALSE,
        stringsAsFactors = FALSE
      ), candidate_results = NULL, diagnostics = list(error = conditionMessage(e)))
    })
  }
  results <- lapply(selected, one)
  out <- list(
    summary = gou_bind_rows_fill(lapply(results, `[[`, "summary")),
    candidate_results = gou_bind_rows_fill(lapply(results, `[[`, "candidate_results")),
    diagnostics = lapply(results, `[[`, "diagnostics"),
    settings = list(
      criterion = criterion,
      threshold_mode = threshold_mode,
      objective_mode = objective_mode,
      mean_level_source = mean_level_source,
      cost_mode = cost_mode,
      Formation_Only_Calibration = TRUE,
      Testing_Prices_Used_For_Thresholds = FALSE,
      Testing_BidAsk_Used_For_Costs = FALSE
    ),
    metadata = list(created_at = Sys.time())
  )
  class(out) <- c("gaussian_ou_threshold_analytic", "list")
  out
}

as_gaussian_threshold_rule_spec <- function(threshold_result, criterion = NULL) {
  tab <- threshold_result$summary %||% threshold_result
  if (!is.null(criterion) && "Criterion" %in% names(tab)) {
    tab <- tab[tab$Criterion %in% criterion, , drop = FALSE]
  }
  data.frame(
    Window = tab$Window %||% tab$Window_Index,
    Window_Name = tab$Window_Name,
    Rule_Name = paste(tab$Criterion, tab$Analytic_Method, sep = "_"),
    Long_Entry = tab$Long_Entry,
    Long_Exit = tab$Long_Exit,
    Short_Entry = tab$Short_Entry,
    Short_Exit = tab$Short_Exit,
    Long_Stop = NA_real_,
    Short_Stop = NA_real_,
    Transaction_Cost = tab$Cost_Total_RoundTrip,
    Criterion = tab$Criterion,
    Immediate_Reversal_At_Opposite_Band = tab$Immediate_Reversal_At_Opposite_Band,
    stringsAsFactors = FALSE
  )
}

gou_predictive_band <- function(test_grid, lambda, mu, sigma, x0, band_probs = c(0.05, 0.5, 0.95), active_time_col = "Active_Time_Minutes") {
  active <- as.numeric(test_grid[[active_time_col]])
  cutoff <- active[1L]
  tau <- active - cutoff
  tau[!is.finite(tau) | tau < 0] <- NA_real_
  meanv <- mu + exp(-lambda * tau) * (x0 - mu)
  varv <- sigma^2 * (-expm1(-2 * lambda * tau)) / (2 * lambda)
  varv[tau == 0] <- 0
  data.frame(
    Step = seq_along(active) - 1L,
    Dates = as.POSIXct(test_grid$Dates, tz = "Europe/London"),
    Active_Time_Minutes = active,
    Relative_Active_Time_Minutes = tau,
    q_lower = meanv + stats::qnorm(min(band_probs)) * sqrt(varv),
    q_mid = meanv,
    q_upper = meanv + stats::qnorm(max(band_probs)) * sqrt(varv),
    stringsAsFactors = FALSE
  )
}

gou_predictive_paths <- function(test_grid, lambda, mu, sigma, x0, n_paths = 50L, seed = NULL, active_time_col = "Active_Time_Minutes") {
  if (n_paths <= 0L) return(data.frame())
  if (!is.null(seed)) set.seed(seed)
  active <- as.numeric(test_grid[[active_time_col]])
  dt <- diff(active)
  if (any(!is.finite(dt) | dt <= 0)) stop("Testing active-time grid must have positive finite increments.", call. = FALSE)
  rows <- vector("list", n_paths)
  for (j in seq_len(n_paths)) {
    x <- numeric(length(active))
    x[1L] <- x0
    for (i in seq_along(dt)) {
      tq <- gou_transition_quantities(lambda, dt[i])
      x[i + 1L] <- mu + tq$phi * (x[i] - mu) + sigma * sqrt(tq$q) * stats::rnorm(1L)
    }
    rows[[j]] <- data.frame(
      Simulation_ID = j,
      Step = seq_along(active) - 1L,
      Dates = as.POSIXct(test_grid$Dates, tz = "Europe/London"),
      Active_Time_Minutes = active,
      Relative_Active_Time_Minutes = active - active[1L],
      Simulated_Spread = x,
      stringsAsFactors = FALSE
    )
  }
  gou_bind_rows_fill(rows)
}

