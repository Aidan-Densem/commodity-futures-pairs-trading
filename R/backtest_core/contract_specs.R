mab_contract_spec_required_fields <- function() c(
  "Generic", "Root", "ContractMonth", "BloombergTickerOriginal",
  "BloombergSecurityResolved", "BloombergProductName", "Product",
  "ExchangeCode", "ProductCode", "ProductCodeSource", "QuotationCurrency",
  "PnLCurrency", "ContractQuantity", "ContractUnit", "PriceQuotation",
  "PriceScaleToPnLCurrencyPerUnit", "PointValueNativePerDisplayedPoint",
  "MinimumPriceIncrementDisplayed", "TickValueNative", "MinimumLotContracts",
  "LotIncrementContracts", "SpecificationClass", "CoreSpecificationSource",
  "RetrievalDate", "CoreMonetarySpecificationVerified", "VerificationScope",
  "VerificationNotes", "BloombergGlobalID"
)

mab_normalize_security_id <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x <- sub("[[:space:]]+(COMDTY|CURNCY)$", "", x)
  gsub("[^A-Z0-9]", "", x)
}

mab_read_contract_specs <- function(path, strict_expected_structure = TRUE,
                                    tolerance = 1e-10) {
  mab_assert(file.exists(path), paste0("Missing contract-specification CSV: ", path))
  specs <- utils::read.csv(
    path, check.names = FALSE, stringsAsFactors = FALSE,
    fileEncoding = "UTF-8-BOM", na.strings = c("", "NA")
  )
  required <- mab_contract_spec_required_fields()
  mab_assert(all(required %in% names(specs)), paste0(
    "Contract-specification schema lacks: ",
    paste(setdiff(required, names(specs)), collapse = ", ")
  ))
  numeric_fields <- c(
    "ContractQuantity", "PriceScaleToPnLCurrencyPerUnit",
    "PointValueNativePerDisplayedPoint", "MinimumPriceIncrementDisplayed",
    "TickValueNative", "MinimumLotContracts", "LotIncrementContracts"
  )
  for (field in numeric_fields) specs[[field]] <- suppressWarnings(as.numeric(specs[[field]]))
  specs$CoreMonetarySpecificationVerified <- tolower(trimws(
    as.character(specs$CoreMonetarySpecificationVerified)
  )) %in% c("true", "t", "1", "yes")
  specs$Generic <- toupper(trimws(specs$Generic))
  specs$Root <- toupper(trimws(specs$Root))
  specs$PnLCurrency <- toupper(trimws(specs$PnLCurrency))
  specs$QuotationCurrency <- toupper(trimws(specs$QuotationCurrency))
  specs$ContractMonth <- trimws(as.character(specs$ContractMonth))
  specs$original_key <- mab_normalize_security_id(specs$BloombergTickerOriginal)
  specs$resolved_key <- mab_normalize_security_id(specs$BloombergSecurityResolved)
  specs$exact_contract_key <- paste(specs$Generic, specs$ContractMonth, specs$resolved_key, sep = "|")

  finite_positive <- function(field) all(is.finite(specs[[field]]) & specs[[field]] > 0)
  for (field in numeric_fields) {
    mab_assert(finite_positive(field), paste0("Contract field must be finite and positive: ", field))
  }
  mab_assert(all(specs$CoreMonetarySpecificationVerified),
             "One or more exact contracts are not marked core-monetary-specification verified.")
  mab_assert(!anyDuplicated(specs$resolved_key),
             "BloombergSecurityResolved must be globally unique after normalisation.")
  mab_assert(!anyDuplicated(specs[c("Generic", "original_key")]),
             "Generic plus BloombergTickerOriginal must be unique after normalisation.")
  mab_assert(!anyDuplicated(specs[c("Generic", "ContractMonth")]),
             "Generic plus ContractMonth must be unique.")
  identity_error <- abs(
    specs$TickValueNative -
      specs$MinimumPriceIncrementDisplayed * specs$PointValueNativePerDisplayedPoint
  )
  identity_scale <- pmax(1, abs(specs$TickValueNative))
  mab_assert(all(identity_error <= tolerance * identity_scale),
             "TickValueNative does not equal tick size times point value.")

  expected_currency <- c(
    CO1 = "USD", CL1 = "USD", HO1 = "USD", XB1 = "USD", NG1 = "USD",
    QS1 = "USD", EN1 = "USD", OQA1 = "USD", FN1 = "GBP", TZT1 = "EUR",
    BIT1 = "CNY", U61 = "INR", ZS1 = "INR"
  )
  observed_currency <- tapply(specs$PnLCurrency, specs$Generic, function(x) unique(x))
  mab_assert(setequal(names(observed_currency), names(expected_currency)),
             "Contract metadata generic universe does not match the fixed 13-futures universe.")
  mab_assert(all(vapply(names(expected_currency), function(generic) {
    identical(as.character(observed_currency[[generic]]), expected_currency[[generic]])
  }, logical(1L))), "Contract metadata P&L currency mapping is invalid.")
  scaled_roots <- sort(unique(specs$Root[abs(specs$PriceScaleToPnLCurrencyPerUnit - 0.01) <= tolerance]))
  unit_roots <- sort(unique(specs$Root[abs(specs$PriceScaleToPnLCurrencyPerUnit - 1) <= tolerance]))
  mab_assert(identical(scaled_roots, sort(c("FN", "HO", "XB"))),
             "Only FN, HO, and XB may use a displayed-price scale of 0.01.")
  mab_assert(length(unit_roots) == 10L &&
               all(abs(specs$PriceScaleToPnLCurrencyPerUnit - 0.01) <= tolerance |
                     abs(specs$PriceScaleToPnLCurrencyPerUnit - 1) <= tolerance),
             "Unexpected displayed-price scale in contract metadata.")

  if (isTRUE(strict_expected_structure)) {
    mab_assert(nrow(specs) == 208L, "Expected exactly 208 exact-contract rows.")
    counts <- table(specs$Generic)
    mab_assert(length(counts) == 13L && all(counts == 16L),
               "Expected 13 generics with 16 exact contract months each.")
    mab_assert(identical(range(specs$ContractMonth), c("2025-07", "2026-10")),
               "Expected contract-month coverage from 2025-07 through 2026-10.")
  }
  fn_quantities <- sort(unique(specs$ContractQuantity[specs$Generic == "FN1"]))
  tzt_quantities <- sort(unique(specs$ContractQuantity[specs$Generic == "TZT1"]))
  mab_assert(length(fn_quantities) > 1L && length(unique(
    specs$PointValueNativePerDisplayedPoint[specs$Generic == "FN1"]
  )) > 1L, "FN exact-month quantity/point-value variation is missing.")
  mab_assert(length(tzt_quantities) > 1L && length(unique(
    specs$PointValueNativePerDisplayedPoint[specs$Generic == "TZT1"]
  )) > 1L, "TZT exact-month quantity/point-value variation is missing.")

  attr(specs, "source_path") <- normalizePath(path, winslash = "/", mustWork = TRUE)
  attr(specs, "source_sha256") <- mab_sha256(path)
  attr(specs, "validation") <- data.frame(
    check = c(
      "row_count", "generic_count", "months_per_generic", "resolved_unique",
      "generic_month_unique", "tick_value_identity", "currency_mapping",
      "displayed_price_scale", "fn_month_specific", "tzt_month_specific",
      "verified_flags"
    ),
    status = "pass",
    observed = c(
      nrow(specs), length(unique(specs$Generic)), paste(sort(unique(as.integer(counts))), collapse = ";"),
      !anyDuplicated(specs$resolved_key), !anyDuplicated(specs[c("Generic", "ContractMonth")]),
      max(identity_error), paste(names(expected_currency), expected_currency, sep = "=", collapse = ";"),
      paste0("0.01:", paste(scaled_roots, collapse = ","), ";1.0:", paste(unit_roots, collapse = ",")),
      paste(fn_quantities, collapse = ";"), paste(tzt_quantities, collapse = ";"),
      sum(specs$CoreMonetarySpecificationVerified)
    ),
    stringsAsFactors = FALSE
  )
  specs
}

mab_match_contract_spec_one <- function(execution_contract, generic, specs) {
  raw <- as.character(execution_contract)
  generic <- toupper(trimws(as.character(generic)))
  key <- mab_normalize_security_id(raw)
  resolved_hit <- which(specs$resolved_key == key & specs$Generic == generic)
  original_hit <- which(specs$original_key == key & specs$Generic == generic)
  hit <- if (length(resolved_hit)) resolved_hit else original_hit
  method <- if (length(resolved_hit)) {
    "exact_resolved_security"
  } else if (length(original_hit)) {
    "exact_original_ticker_with_generic"
  } else {
    "unmatched"
  }
  status <- if (length(hit) == 1L) "matched" else if (!length(hit)) "unmatched" else "ambiguous"
  audit <- data.frame(
    raw_execution_contract = raw,
    normalised_identifier = key,
    execution_generic = generic,
    matched_bloomberg_exact_contract = if (length(hit) == 1L) specs$BloombergSecurityResolved[hit] else NA_character_,
    root = if (length(hit) == 1L) specs$Root[hit] else NA_character_,
    contract_month = if (length(hit) == 1L) specs$ContractMonth[hit] else NA_character_,
    matching_method = method,
    match_count = length(hit),
    unique = length(hit) == 1L,
    status = status,
    failure_reason = if (status == "matched") NA_character_ else paste0(status, " exact-contract specification"),
    stringsAsFactors = FALSE
  )
  if (length(hit) != 1L) {
    return(list(spec = NULL, audit = audit))
  }
  list(spec = specs[hit, , drop = FALSE], audit = audit)
}

mab_match_contract_specs <- function(execution_contract, generic, specs, stop_on_failure = TRUE) {
  mab_assert(length(execution_contract) == length(generic),
             "execution_contract and generic must have equal length.")
  matches <- Map(mab_match_contract_spec_one, execution_contract, generic, MoreArgs = list(specs = specs))
  audit <- mab_bind_rows(lapply(matches, `[[`, "audit"))
  if (isTRUE(stop_on_failure) && any(audit$status != "matched")) {
    bad <- unique(paste(audit$execution_generic[audit$status != "matched"],
                        audit$raw_execution_contract[audit$status != "matched"], sep = ":"))
    stop("Exact-contract specification join failed: ", paste(bad, collapse = ", "), call. = FALSE)
  }
  list(specs = lapply(matches, `[[`, "spec"), audit = audit)
}
