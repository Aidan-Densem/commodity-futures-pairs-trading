# Public ancillary inputs ship with the repository. An authorised researcher
# may supply an identically schemed local override beside the licensed inputs;
# otherwise the included public calibration is used.

repo_fee_schedule_path <- function(data_root = NA_character_) {
  external <- if (
    length(data_root) == 1L && !is.na(data_root) && nzchar(data_root)
  ) file.path(data_root, "fees.csv") else NA_character_
  if (!is.na(external) && file.exists(external)) return(external)
  included <- repo_path("data", "ancillary", "fees.csv")
  repo_assert(file.exists(included), "Included public fee schedule is missing.")
  included
}

repo_market_session_metadata_path <- function() {
  included <- repo_path(
    "data", "ancillary", "market_sessions_13_products.json"
  )
  repo_assert(file.exists(included), "Included market-session metadata is missing.")
  included
}

repo_candidate_pairs_path <- function(data_root = NA_character_) {
  external <- if (
    length(data_root) == 1L && !is.na(data_root) && nzchar(data_root)
  ) file.path(data_root, "candidate_pairs.csv") else NA_character_
  if (!is.na(external) && file.exists(external)) return(external)
  included <- repo_path("data", "ancillary", "candidate_pairs_78.csv")
  repo_assert(file.exists(included), "Included 78-pair configuration is missing.")
  included
}

repo_exact_contract_manifest_path <- function() {
  included <- repo_path(
    "data", "ancillary", "exact_contract_manifest.csv"
  )
  repo_assert(file.exists(included), "Included exact-contract manifest is missing.")
  included
}

repo_validate_exact_contract_manifest <- function(path = repo_exact_contract_manifest_path()) {
  manifest <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c(
    "generic_root", "generic", "exact_contract_identifier",
    "source_alias_identifier", "delivery_month", "delivery_year",
    "canonical_product_name", "venue"
  )
  repo_assert(all(required %in% names(manifest)), paste(
    "Exact-contract manifest lacks:",
    paste(setdiff(required, names(manifest)), collapse = ", ")
  ))
  repo_assert(nrow(manifest) == 208L,
              "Exact-contract manifest must contain exactly 208 securities.")
  repo_assert(!anyDuplicated(manifest$exact_contract_identifier) &&
                !anyDuplicated(manifest$source_alias_identifier),
              "Exact-contract identifiers and source aliases must each be globally unique.")
  repo_assert(!anyDuplicated(manifest[c("generic", "delivery_month")]),
              "Generic plus delivery month must be unique in the exact-contract manifest.")
  counts <- table(manifest$generic)
  repo_assert(length(counts) == 13L && all(counts == 16L),
              "Exact-contract manifest must contain 13 generics with 16 months each.")
  repo_assert(identical(range(manifest$delivery_month), c("2025-07", "2026-10")),
              "Exact-contract manifest month range must be 2025-07 through 2026-10.")
  repo_assert(all(as.integer(substr(manifest$delivery_month, 1L, 4L)) ==
                    as.integer(manifest$delivery_year)),
              "Exact-contract manifest delivery year is inconsistent with delivery month.")
  list(
    path = normalizePath(path, winslash = "/", mustWork = TRUE),
    sha256 = repo_sha256(path),
    rows = nrow(manifest),
    generics = length(counts),
    months_per_generic = unique(as.integer(counts)),
    month_range = range(manifest$delivery_month)
  )
}
