#!/usr/bin/env Rscript
source(file.path("R", "io_helpers.R"))
source(file.path("R", "public_input_paths.R"))
source(file.path("config", "package_requirements.R"))

validate_inputs_main <- function(require_empirical_data = TRUE) {
  missing_packages <- required_r_packages[!vapply(required_r_packages, requireNamespace,
                                                   logical(1L), quietly = TRUE)]
  data_root <- repo_external_data_root(require_empirical_data)
  expected <- c(
    "market_quotes.csv", "contract_lifecycle.csv",
    "session_intervals.csv", "bfix.xlsx",
    "contract_specs.csv"
  )
  missing_inputs <- if (is.na(data_root)) expected else expected[
    !file.exists(file.path(data_root, expected))]
  if (require_empirical_data && length(missing_packages)) stop(
    "Missing required R packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)
  if (require_empirical_data && length(missing_inputs)) stop(
    "Missing external inputs: ", paste(missing_inputs, collapse = ", "), call. = FALSE)
  fee_schedule <- repo_fee_schedule_path(data_root)
  session_metadata <- repo_market_session_metadata_path()
  pair_configuration <- repo_candidate_pairs_path(data_root)
  exact_contract_manifest <- repo_exact_contract_manifest_path()
  exact_contract_manifest_validation <- repo_validate_exact_contract_manifest(
    exact_contract_manifest
  )
  result <- list(repository_root = repo_root(), data_root = data_root,
                 missing_r_packages = missing_packages,
                 missing_external_inputs = missing_inputs,
                 fee_schedule = fee_schedule,
                 candidate_pair_configuration = pair_configuration,
                 public_exact_contract_manifest = exact_contract_manifest,
                 public_exact_contract_manifest_validation =
                   exact_contract_manifest_validation,
                 public_session_metadata = session_metadata)
  print(result); invisible(result)
}
if (sys.nframe() == 0L) validate_inputs_main(TRUE)
