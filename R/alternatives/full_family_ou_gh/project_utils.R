ou_gh_assert <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
  invisible(TRUE)
}

`%||%` <- function(left, right) if (is.null(left)) right else left

ou_gh_project_root <- function() normalizePath(getwd(), winslash = "/", mustWork = TRUE)

ou_gh_sha256 <- function(path) {
  ou_gh_assert(length(path) == 1L && file.exists(path), paste("Missing file:", path))
  unname(tools::sha256sum(path))
}

ou_gh_hash_object <- function(object) {
  if (requireNamespace("digest", quietly = TRUE)) {
    return(digest::digest(object, algo = "sha256", serialize = TRUE))
  }
  path <- tempfile(fileext = ".rds"); on.exit(unlink(path), add = TRUE)
  saveRDS(object, path, version = 3); ou_gh_sha256(path)
}

ou_gh_atomic_save_rds <- function(object, path, version = 3) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(paste0(".", basename(path), "."), dirname(path))
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  saveRDS(object, temporary, version = version)
  ou_gh_assert(file.rename(temporary, path), paste("Atomic rename failed:", path))
  invisible(path)
}

ou_gh_time <- function(x) as.POSIXct(x, origin = "1970-01-01", tz = "Europe/London")
