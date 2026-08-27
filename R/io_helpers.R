repo_root <- function(start = getwd()) {
  path <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(path, "config", "production_config.R"))) return(path)
    parent <- dirname(path)
    if (identical(parent, path)) break
    path <- parent
  }
  stop("Could not locate the repository root.", call. = FALSE)
}

repo_path <- function(...) file.path(repo_root(), ...)

repo_external_data_root <- function(must_work = FALSE) {
  value <- Sys.getenv("DISSERTATION_DATA_ROOT", unset = "")
  if (!nzchar(value)) {
    if (must_work) stop(
      "Set DISSERTATION_DATA_ROOT to the local proprietary-data directory.",
      call. = FALSE
    )
    return(NA_character_)
  }
  normalizePath(value, winslash = "/", mustWork = must_work)
}

repo_assert <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
  invisible(TRUE)
}

repo_sha256 <- function(path) {
  repo_assert(file.exists(path), paste("Missing file:", path))
  unname(tools::sha256sum(path))
}

repo_atomic_rds <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(paste0(".", basename(path), "."), dirname(path))
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  saveRDS(value, temporary, version = 3)
  repo_assert(file.rename(temporary, path), paste("Atomic rename failed:", path))
  invisible(path)
}

repo_atomic_csv <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(paste0(".", basename(path), "."), dirname(path))
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  utils::write.csv(value, temporary, row.names = FALSE, na = "")
  repo_assert(file.rename(temporary, path), paste("Atomic rename failed:", path))
  invisible(path)
}

repo_source <- function(relative, envir = parent.frame()) {
  sys.source(repo_path(relative), envir = envir)
}

