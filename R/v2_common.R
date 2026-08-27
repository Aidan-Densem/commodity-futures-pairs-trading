options(stringsAsFactors = FALSE)

`%v2||%` <- function(x, y) if (is.null(x)) y else x

v2_assert <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
  invisible(TRUE)
}

v2_sha256 <- function(path) {
  v2_assert(file.exists(path), paste("Cannot hash missing path:", path))
  unname(tools::sha256sum(path))
}

v2_hash_object <- function(x) {
  v2_assert(requireNamespace("digest", quietly = TRUE), "Package 'digest' is required.")
  digest::digest(x, algo = "sha256", serialize = TRUE)
}

v2_atomic_rds <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(pattern = paste0(basename(path), "."), tmpdir = dirname(path))
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  saveRDS(x, temporary, compress = "gzip")
  v2_assert(file.rename(temporary, path), paste("Atomic RDS write failed:", path))
  invisible(path)
}

v2_write_csv <- function(x, path) {
  v2_assert(is.data.frame(x), "CSV output must be a data.frame.")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(pattern = paste0(basename(path), "."), tmpdir = dirname(path))
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  utils::write.csv(x, temporary, row.names = FALSE, na = "")
  v2_assert(file.rename(temporary, path), paste("Atomic CSV write failed:", path))
  invisible(path)
}

v2_bind_rows <- function(rows) {
  rows <- rows[vapply(rows, function(x) is.data.frame(x) && nrow(x), logical(1L))]
  if (!length(rows)) return(data.frame())
  fields <- unique(unlist(lapply(rows, names), use.names = FALSE))
  rows <- lapply(rows, function(x) {
    for (field in setdiff(fields, names(x))) x[[field]] <- NA
    x[fields]
  })
  output <- do.call(rbind, rows)
  rownames(output) <- NULL
  output
}

v2_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "Europe/London")

v2_configuration_fingerprint <- function(contract) {
  v2_hash_object(contract[c(
    "schema_version", "quote_quality", "cost_proxy", "terminal", "capital", "production"
  )])
}

v2_simulate_paths <- function(simulator, active_time, x0, n_paths, seed) {
  value <- simulator$simulate_paths(
    active_time = active_time, x0 = x0,
    n_paths = as.integer(n_paths), seed = as.integer(seed)
  )
  v2_assert(is.list(value) && is.matrix(value$paths) && all(is.finite(value$paths)),
    "Simulator returned invalid paths.")
  value
}
