strict_interior_gh_fit_task <- function(task_row, checkpoint_path = NULL, ...) {
  result <- ou_gh_fit_task_row(task_row, checkpoint_path = checkpoint_path, ...)
  result$gh_mode <- "STRICT_INTERIOR"
  result$model_id <- "OU_GH_DRIVER_INTERIOR"
  result$model_label <- "GHI"
  result$family_router_used <- FALSE
  result$fallback_family_used <- FALSE
  if (!is.null(checkpoint_path)) ou_gh_atomic_save_rds(result, checkpoint_path)
  result
}

validate_strict_interior_snapshot <- function(snapshot) {
  stopifnot(is.data.frame(snapshot))
  required <- c("fit_status", "lambda", "zeta", "rho", "gh_mode")
  missing <- setdiff(required, names(snapshot))
  if (length(missing)) stop(
    "Strict-interior snapshot is missing: ", paste(missing, collapse = ", "),
    call. = FALSE
  )
  successful <- grepl("^fit_success", snapshot$fit_status)
  admissible <- is.finite(snapshot$lambda) & is.finite(snapshot$zeta) &
    is.finite(snapshot$rho) & snapshot$zeta > 0 & abs(snapshot$rho) < 1
  if (any(successful & !admissible)) stop(
    "A successful GHI fit is not in the strict GH parameter interior.",
    call. = FALSE
  )
  if (any(snapshot$gh_mode != "STRICT_INTERIOR")) stop(
    "Strict-interior snapshot contains an incompatible gh_mode.", call. = FALSE
  )
  invisible(TRUE)
}
