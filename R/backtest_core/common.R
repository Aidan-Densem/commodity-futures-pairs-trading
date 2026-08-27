options(stringsAsFactors = FALSE)
.mab_version <- "model_agnostic_exact_contract_midpoint_v2"
if (!exists("%||%", mode = "function")) `%||%` <- function(x, y) if (is.null(x)) y else x

mab_assert <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
  invisible(TRUE)
}

mab_time <- function(x, tz = "Europe/London") {
  if (inherits(x, "POSIXt")) return(as.POSIXct(x, tz = tz))
  as.POSIXct(x, origin = "1970-01-01", tz = tz)
}

mab_iso_time <- function(x = Sys.time()) format(
  as.POSIXct(x), "%Y-%m-%dT%H:%M:%S%z", tz = "Europe/London"
)

mab_sha256 <- function(path) {
  mab_assert(file.exists(path), paste0("Cannot hash missing file: ", path))
  unname(tools::sha256sum(path))
}

mab_hash_object <- function(object) {
  if (requireNamespace("digest", quietly = TRUE)) {
    return(digest::digest(object, algo = "sha256", serialize = TRUE))
  }
  path <- tempfile(fileext = ".rds"); on.exit(unlink(path), add = TRUE)
  saveRDS(object, path, version = 3); mab_sha256(path)
}

mab_bind_rows <- function(rows) {
  rows <- rows[!vapply(rows, is.null, logical(1L))]
  rows <- rows[vapply(rows, NROW, integer(1L)) > 0L]
  if (!length(rows)) return(data.frame())
  fields <- unique(unlist(lapply(rows, names), use.names = FALSE))
  rows <- lapply(rows, function(row) {
    for (field in setdiff(fields, names(row))) row[[field]] <- rep(NA, nrow(row))
    row[fields]
  })
  out <- do.call(rbind, rows); rownames(out) <- NULL; out
}
