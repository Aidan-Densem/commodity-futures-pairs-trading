source(repo_path("R", "public_input_paths.R"), local = TRUE)

validation <- repo_validate_exact_contract_manifest()
smoke_equal(validation$rows, 208L, message = "Exact-contract manifest row count changed.")
smoke_equal(validation$generics, 13L, message = "Exact-contract manifest root count changed.")
smoke_equal(validation$months_per_generic, 16L,
            message = "Exact-contract manifest month breadth changed.")

exact <- utils::read.csv(
  repo_exact_contract_manifest_path(), stringsAsFactors = FALSE, check.names = FALSE
)
universe <- utils::read.csv(
  repo_path("data", "ancillary", "product_universe_13.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
smoke_expect(
  setequal(unique(exact$generic_root), universe$root) &&
    setequal(unique(exact$generic), universe$generic),
  "Exact-contract manifest does not match the frozen 13-product universe."
)

fees <- mab_read_fee_table(repo_fee_schedule_path())
smoke_expect(nrow(fees) > 0L && all(fees$verified),
             "The public fee table does not pass the fail-closed verifier.")
mcx <- fees$root %in% c("U6", "ZS")
smoke_expect(
  sum(mcx) == 2L && all(fees$verified[mcx]) &&
    all(abs(fees$fee_rate[mcx] - 0.000021) < 1e-15),
  "The source-verified MCX fee rows are missing or numerically altered."
)

smoke_expect(requireNamespace("jsonlite", quietly = TRUE),
             "jsonlite is required to validate public session metadata.")
session <- jsonlite::fromJSON(repo_market_session_metadata_path(), simplifyVector = FALSE)
smoke_expect(
  length(session$instruments) == 13L &&
    any(grepl("not a date-expanded production session-interval table", session$notes, fixed = TRUE)),
  "Session reference metadata is unreadable or its reconstruction scope is ambiguous."
)

input_manifest <- utils::read.csv(
  repo_path("data", "INPUT_MANIFEST.csv"), stringsAsFactors = FALSE,
  check.names = FALSE
)
included <- toupper(as.character(input_manifest$redistributed_in_repo)) == "TRUE"
included_paths <- input_manifest$expected_path[included]
smoke_expect(
  all(vapply(included_paths, function(path) file.exists(repo_path(path)), logical(1L))),
  "A redistributed input-manifest path does not resolve."
)
cat("PUBLIC_ANCILLARY_CONTRACTS_PASS\n")
