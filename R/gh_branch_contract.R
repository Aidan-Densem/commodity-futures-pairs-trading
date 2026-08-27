GH_MODES <- c("STRICT_INTERIOR", "FULL_FAMILY")

gh_object_mode <- function(object) {
  mode <- if (is.data.frame(object) && "gh_mode" %in% names(object)) {
    unique(as.character(object$gh_mode))
  } else as.character(if (is.null(object$gh_mode)) NA_character_ else object$gh_mode)
  unique(mode[!is.na(mode) & nzchar(mode)])
}

validate_gh_mode <- function(object, expected) {
  if (!expected %in% GH_MODES) stop("Unknown expected GH mode.", call. = FALSE)
  observed <- gh_object_mode(object)
  if (length(observed) != 1L || !identical(observed, expected)) stop(
    "GH object-mode mismatch: expected ", expected, "; observed ",
    if (length(observed)) paste(observed, collapse = ",") else "UNLABELLED",
    ". Cross-branch checkpoint reuse is prohibited.", call. = FALSE
  )
  invisible(object)
}

gh_checkpoint_fingerprint <- function(task, gh_mode, source_hashes = NULL) {
  if (!gh_mode %in% GH_MODES) stop("Invalid GH checkpoint mode.", call. = FALSE)
  payload <- list(gh_mode = gh_mode, task = task, source_hashes = source_hashes)
  if (requireNamespace("digest", quietly = TRUE)) {
    digest::digest(payload, algo = "sha256", serialize = TRUE)
  } else {
    path <- tempfile(fileext = ".rds"); on.exit(unlink(path), add = TRUE)
    saveRDS(payload, path, version = 3); unname(tools::sha256sum(path))
  }
}
