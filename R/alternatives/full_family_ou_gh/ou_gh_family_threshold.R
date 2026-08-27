# Branch-local exact-transition path generator. Threshold search and event
# evaluation use the repository's common V2 engine.
ou_gh_family_path_matrix <- function(fit, active_time, x0, n_paths, seed,
                                      table = NULL, supplied_uniforms = NULL) {
  active_time <- as.numeric(active_time)
  stopifnot(length(active_time) >= 2L, active_time[[1L]] == 0,
    all(diff(active_time) > 0), n_paths >= 1L)
  n_paths <- as.integer(n_paths)
  n_steps <- length(active_time) - 1L
  if (!is.null(supplied_uniforms)) {
    supplied_uniforms <- as.matrix(supplied_uniforms)
    stopifnot(identical(dim(supplied_uniforms), c(n_steps, n_paths)))
  } else set.seed(as.integer(seed)[[1L]])
  paths <- matrix(NA_real_, nrow = n_steps + 1L, ncol = n_paths)
  paths[1L, ] <- x0
  for (index in seq_len(n_steps)) {
    Delta <- active_time[[index + 1L]] - active_time[[index]]
    one_table <- table
    if (!is.null(table) && !isTRUE(all.equal(table$Delta, Delta))) one_table <- NULL
    uniforms <- if (is.null(supplied_uniforms)) NULL else supplied_uniforms[index, ]
    remainder <- ou_gh_family_remainder_draw(
      n_paths, fit, Delta, uniforms = uniforms, table = one_table
    )
    rho <- exp(-fit$kappa * Delta)
    paths[index + 1L, ] <- fit$mu + rho * (paths[index, ] - fit$mu) + remainder
  }
  paths
}
