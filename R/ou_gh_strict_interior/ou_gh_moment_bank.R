ou_gh_candidate_moment_groups <- function() {
  candidates <- expand.grid(
    horizon = c(1, 2, 5, 10, 15, 30, 60, 120, 240, 630),
    frequency = c(0.05, 0.10, 0.20, 0.35, 0.50, 0.75, 1, 1.5, 2.5, 4),
    instrument = c(0, 0.5),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  candidates$group_id <- sprintf(
    "h%03d_u%04.2f_a%+04.2f",
    candidates$horizon, candidates$frequency, candidates$instrument
  )
  candidates$weight <- 1 / sqrt(candidates$horizon)
  candidates <- candidates[order(candidates$group_id),
    c("group_id", "horizon", "frequency", "instrument", "weight")]
  row.names(candidates) <- NULL
  candidates
}

ou_gh_population_truth_bank <- function() {
  specification <- data.frame(
    case_id = c(
      "symmetric", "nig_positive", "nig_negative", "hyperbolic",
      "vg_near", "concentrated", "strong_positive", "strong_negative"
    ),
    lambda = c(-2, -0.5, -0.5, 1, 1.2, 5, 0.5, -1),
    zeta = c(1, 1.5, 1.5, 2.5, 0.02, 100, 0.5, 0.2),
    rho = c(0, 0.55, -0.55, 0.2, 0.35, -0.5, 0.85, -0.85),
    sigma_eta_1 = c(0.02, 0.03, 0.03, 0.02, 0.05, 0.015, 0.04, 0.02),
    half_life = c(300, 1500, 1500, 8000, 100, 30000, 30, 100000),
    stringsAsFactors = FALSE
  )
  fits <- lapply(seq_len(nrow(specification)), function(index) {
    one <- specification[index, ]
    gh_shape_scale_to_direct(
      one$lambda, one$zeta, one$rho, one$sigma_eta_1,
      log(2) / one$half_life, mu = 0.1
    )
  })
  names(fits) <- specification$case_id
  list(specification = specification, fits = fits)
}

ou_gh_population_jacobians <- function(
    candidates = ou_gh_candidate_moment_groups(),
    truth_bank = ou_gh_population_truth_bank(),
    step = 1e-4,
    quadrature_nodes = 24L
) {
  candidates <- candidates[order(candidates$group_id), , drop = FALSE]
  fits <- truth_bank$fits[sort(names(truth_bank$fits))]
  output <- lapply(fits, function(truth_fit) {
    raw <- ou_gh_fit_to_raw(truth_fit)
    jacobian <- matrix(NA_real_, nrow = 2L * nrow(candidates), ncol = 6L,
      dimnames = list(NULL, ou_gh_raw_parameter_names()))
    for (column in seq_len(6L)) {
      plus <- minus <- raw
      plus[[column]] <- plus[[column]] + step
      minus[[column]] <- minus[[column]] - step
      jacobian[, column] <- (
        ou_gh_population_moments(plus, truth_fit, candidates, quadrature_nodes) -
          ou_gh_population_moments(minus, truth_fit, candidates, quadrature_nodes)
      ) / (2 * step)
    }
    column_scale <- sqrt(colSums(jacobian^2))
    jacobian <- sweep(jacobian, 2L, pmax(column_scale, 1e-12), "/")
    row.names(jacobian) <- as.vector(rbind(
      paste0(candidates$group_id, "::real"),
      paste0(candidates$group_id, "::imag")
    ))
    jacobian
  })
  output
}

ou_gh_select_moment_bank <- function(
    candidates = ou_gh_candidate_moment_groups(),
    jacobians = ou_gh_population_jacobians(candidates),
    target_groups = 36L,
    ridge = 1e-8,
    mandatory_group_ids = c(
      "h001_u0.05_a+0.00", "h001_u0.10_a+0.00",
      "h001_u0.20_a+0.00", "h001_u0.35_a+0.00",
      "h001_u0.50_a+0.00", "h001_u0.75_a+0.00",
      "h001_u1.00_a+0.00", "h001_u1.50_a+0.00",
      "h001_u2.50_a+0.00", "h001_u4.00_a+0.00",
      "h001_u0.10_a+0.50", "h001_u0.50_a+0.50",
      "h001_u1.00_a+0.50", "h001_u2.50_a+0.50",
      "h002_u0.20_a+0.00", "h002_u0.35_a+0.00",
      "h002_u0.50_a+0.00", "h002_u0.75_a+0.00",
      "h002_u1.00_a+0.00", "h002_u1.50_a+0.00",
      "h002_u2.50_a+0.00", "h002_u0.20_a+0.50",
      "h002_u0.75_a+0.50", "h005_u0.20_a+0.00",
      "h005_u0.50_a+0.00", "h005_u1.00_a+0.00",
      "h005_u2.50_a+0.00", "h005_u0.50_a+0.50",
      "h010_u0.20_a+0.00", "h015_u0.20_a+0.00",
      "h030_u0.35_a+0.00", "h060_u0.35_a+0.00",
      "h120_u0.20_a+0.00", "h240_u0.20_a+0.00",
      "h630_u0.10_a+0.00"
    )
) {
  candidates <- candidates[order(candidates$group_id), , drop = FALSE]
  jacobians <- jacobians[sort(names(jacobians))]
  group_rows <- lapply(candidates$group_id, function(group_id) {
    expected <- paste0(group_id, c("::real", "::imag"))
    rows <- match(expected, row.names(jacobians[[1L]]))
    ou_gh_assert(!anyNA(rows), paste(
      "Population Jacobian lacks rows for candidate group", group_id
    ))
    rows
  })
  information <- lapply(jacobians, function(value) diag(ridge, 6L))
  selected <- match(mandatory_group_ids, candidates$group_id)
  ou_gh_assert(!anyNA(selected), "A mandatory CCF group is absent from candidates.")
  ou_gh_assert(target_groups >= length(selected),
    "Target moment-bank size is smaller than the mandatory design.")
  trace <- list()
  for (index in seq_along(selected)) {
    rows <- group_rows[[selected[[index]]]]
    information <- Map(function(info, jacobian) {
      info + crossprod(jacobian[rows, , drop = FALSE])
    }, information, jacobians)
    trace[[index]] <- data.frame(
      selection_step = index,
      group_id = candidates$group_id[[selected[[index]]]],
      maximin_log_determinant = NA_real_,
      stringsAsFactors = FALSE
    )
  }
  for (step_index in seq.int(length(selected) + 1L, target_groups)) {
    available <- setdiff(seq_len(nrow(candidates)), selected)
    scores <- vapply(available, function(candidate_index) {
      rows <- group_rows[[candidate_index]]
      case_scores <- mapply(function(info, jacobian) {
        proposal <- info + crossprod(jacobian[rows, , drop = FALSE])
        determinant <- determinant(proposal, logarithm = TRUE)
        as.numeric(determinant$modulus)
      }, information, jacobians)
      min(case_scores)
    }, numeric(1L))
    best_score <- max(scores)
    tied <- available[abs(scores - best_score) <= 1e-12]
    best <- tied[order(candidates$group_id[tied])[[1L]]]
    selected <- c(selected, best)
    rows <- group_rows[[best]]
    information <- Map(function(info, jacobian) {
      info + crossprod(jacobian[rows, , drop = FALSE])
    }, information, jacobians)
    trace[[step_index]] <- data.frame(
      selection_step = step_index,
      group_id = candidates$group_id[[best]],
      maximin_log_determinant = best_score,
      stringsAsFactors = FALSE
    )
  }
  bank <- candidates[selected, , drop = FALSE]
  bank <- bank[order(bank$group_id), , drop = FALSE]
  diagnostics <- do.call(rbind, lapply(names(jacobians), function(case_id) {
    rows <- unlist(group_rows[selected], use.names = FALSE)
    singular <- svd(jacobians[[case_id]][rows, , drop = FALSE])$d
    data.frame(
      case_id = case_id, rank = sum(singular > 1e-8),
      minimum_singular_value = min(singular),
      condition_number = max(singular) / min(singular),
      stringsAsFactors = FALSE
    )
  }))
  bank_hash <- ou_gh_hash_object(bank)
  attr(bank, "bank_hash") <- bank_hash
  list(
    bank = bank, bank_hash = bank_hash,
    selection_trace = do.call(rbind, trace), diagnostics = diagnostics,
    candidate_count = nrow(candidates), target_groups = target_groups
  )
}

ou_gh_load_frozen_moment_bank <- function(root = ou_gh_project_root()) {
  path <- file.path(root, "config", "strict_interior_gh_frozen_moment_bank.csv")
  ou_gh_assert(file.exists(path), "Frozen GH moment bank is unavailable.")
  bank <- utils::read.csv(path, stringsAsFactors = FALSE)
  attr(bank, "bank_hash") <- ou_gh_hash_object(bank)
  bank
}
