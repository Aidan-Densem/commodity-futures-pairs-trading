ou_gh_assert <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
  invisible(TRUE)
}

`%||%` <- function(left, right) {
  if (is.null(left)) right else left
}

ou_gh_project_root <- function() {
  value <- Sys.getenv("OU_GH_PROJECT_ROOT", unset = "")
  if (nzchar(value)) return(normalizePath(value, mustWork = TRUE))
  normalizePath(getwd(), mustWork = TRUE)
}

ou_gh_vendor_r_library <- function(root = ou_gh_project_root()) {
  file.path(root, "vendor", "R", "library")
}

ou_gh_use_vendor_library <- function(root = ou_gh_project_root()) {
  library <- ou_gh_vendor_r_library(root)
  if (dir.exists(library)) .libPaths(unique(c(library, .libPaths())))
  invisible(.libPaths())
}

ou_gh_sha256 <- function(path) {
  ou_gh_assert(length(path) == 1L && file.exists(path),
    paste("Missing file:", path))
  unname(tools::sha256sum(path))
}

ou_gh_hash_object <- function(object) {
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(object, path, version = 3)
  ou_gh_sha256(path)
}

ou_gh_atomic_write_csv <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(paste0(".", basename(path), "."), dirname(path))
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  utils::write.csv(object, temporary, row.names = FALSE, na = "")
  ou_gh_assert(file.rename(temporary, path), paste("Atomic rename failed:", path))
  invisible(path)
}

ou_gh_atomic_write_lines <- function(lines, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(paste0(".", basename(path), "."), dirname(path))
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  writeLines(lines, temporary, useBytes = TRUE)
  ou_gh_assert(file.rename(temporary, path), paste("Atomic rename failed:", path))
  invisible(path)
}

ou_gh_atomic_save_rds <- function(object, path, version = 3) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(paste0(".", basename(path), "."), dirname(path))
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  saveRDS(object, temporary, version = version)
  ou_gh_assert(file.rename(temporary, path), paste("Atomic rename failed:", path))
  invisible(path)
}

ou_gh_time <- function(x) {
  if (inherits(x, "POSIXt")) return(as.POSIXct(x, tz = "Europe/London"))
  if (is.character(x)) {
    output <- as.POSIXct(
      x, format = "%Y-%m-%d %H:%M:%S", tz = "Europe/London"
    )
    date_only <- is.na(output) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x)
    output[date_only] <- as.POSIXct(
      x[date_only], format = "%Y-%m-%d", tz = "Europe/London"
    )
    return(output)
  }
  as.POSIXct(x, origin = "1970-01-01", tz = "Europe/London")
}

ou_gh_source_all <- function(root = ou_gh_project_root(), envir = .GlobalEnv) {
  # Load the branch-local numerical foundation in dependency order.
  files <- c(
    "config/strict_interior_gh_project_contract.R",
    "R/ou_gh_strict_interior/project_utils.R",
    "R/ou_gh_strict_interior/gh_bessel_backend.R",
    "R/ou_gh_strict_interior/gig_distribution.R",
    "R/ou_gh_strict_interior/gh_parameterization.R",
    "R/ou_gh_strict_interior/gh_distribution.R",
    "R/ou_gh_strict_interior/gh_driver.R",
    "R/ou_gh_strict_interior/ou_gh_transition.R"
  )
  for (relative in files) sys.source(file.path(root, relative), envir = envir)
  ou_gh_use_vendor_library(root)
  invisible(TRUE)
}

ou_gh_source_production <- function(root = ou_gh_project_root(), envir = .GlobalEnv) {
  ou_gh_source_all(root, envir)
  files <- c(
    "config/strict_interior_gh_production_contract.R",
    "config/strict_interior_gh_full_run_authority.R",
    "R/ou_gh_strict_interior/ou_gh_task_adapter.R"
  )
  production_modules <- c(
    "R/ou_gh_strict_interior/ou_gh_foundation_hardening.R",
    "R/ou_gh_strict_interior/ou_gh_direct_inversion.R",
    "R/ou_gh_strict_interior/ou_gh_preliminary_profile.R",
    "R/ou_gh_strict_interior/ou_gh_ccf_objective.R",
    "R/ou_gh_strict_interior/ou_gh_moment_bank.R",
    "R/ou_gh_strict_interior/ou_gh_estimation.R",
    "R/ou_gh_strict_interior/ou_gh_exact_likelihood.R",
    "R/ou_gh_strict_interior/ou_gh_synthetic.R",
    "R/ou_gh_strict_interior/ou_gh_simulator_preflight.R",
    "R/ou_gh_strict_interior/ou_gh_parameter_runner.R"
  )
  files <- c(files, production_modules)
  for (relative in files) sys.source(file.path(root, relative), envir = envir)
  invisible(TRUE)
}
