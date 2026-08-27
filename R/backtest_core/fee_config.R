# Primary explicit-fee contract: USD 1 brokerage per contract-side plus
# verified product exchange and clearing charges. Regulatory levies and taxes
# are outside the primary specification and cannot be enabled here.

mab_scenario_definitions <- function(scenarios = c(
    "monetary_baseline_seamless", "monetary_realistic_seamless_roll",
    "monetary_realistic_explicit_roll")) {
  definitions <- data.frame(
    scenario_id = c("monetary_baseline_seamless", "monetary_realistic_seamless_roll",
                    "monetary_realistic_explicit_roll"),
    roll_policy = c("seamless_continuation", "seamless_continuation", "explicit_close_reopen"),
    apply_explicit_fees = c(FALSE, TRUE, TRUE),
    price_mode = "realised_bidask", tradeable_time_rule = "side_specific",
    execution_timing = "same_observation", end_of_window_policy = "force_close",
    stringsAsFactors = FALSE
  )
  unknown <- setdiff(scenarios, definitions$scenario_id)
  mab_assert(!length(unknown), paste0("Unknown monetary scenario(s): ", paste(unknown, collapse = ", ")))
  definitions[match(scenarios, definitions$scenario_id), , drop = FALSE]
}

mab_fee_table_required_fields <- function() c(
  "fee_key_type", "exact_contract", "root", "exchange", "fee_component",
  "fee_basis", "fee_rate", "fee_currency", "fee_unit", "charge_side",
  "effective_from", "effective_until", "source", "verified"
)

mab_read_fee_table <- function(path = NULL) {
  if (is.null(path) || !nzchar(path)) return(NULL)
  mab_assert(file.exists(path), paste0("Missing fee table: ", path))
  z <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                       na.strings = c("", "NA"))
  missing <- setdiff(mab_fee_table_required_fields(), names(z))
  mab_assert(!length(missing), paste0("Fee table schema lacks: ", paste(missing, collapse = ", ")))
  z$fee_key_type <- tolower(trimws(z$fee_key_type))
  z$fee_component <- tolower(trimws(z$fee_component))
  z$fee_basis <- tolower(trimws(z$fee_basis))
  z$charge_side <- tolower(trimws(z$charge_side))
  z$root <- toupper(trimws(z$root)); z$exchange <- toupper(trimws(z$exchange))
  z$fee_currency <- toupper(trimws(z$fee_currency))
  z$exact_contract_key <- mab_normalize_security_id(z$exact_contract)
  mab_assert(all(z$fee_key_type %in% c("exact_contract", "root", "exchange")),
             "Unknown fee key type.")
  mab_assert(all(z$fee_component %in% c("exchange", "clearing", "combined_exchange_clearing")),
             "Only exchange/clearing venue components belong in the primary fee table.")
  mab_assert(all(z$fee_basis %in% c("per_contract", "per_physical_unit", "transaction_notional")),
             "Unknown venue-fee basis.")
  mab_assert(all(z$charge_side %in% c("both", "buy", "sell")), "Unsupported charge side.")
  z$fee_rate <- suppressWarnings(as.numeric(z$fee_rate))
  mab_assert(all(is.finite(z$fee_rate) & z$fee_rate >= 0), "Invalid fee rate.")
  z$effective_from <- as.Date(z$effective_from); z$effective_until <- as.Date(z$effective_until)
  z$effective_from[is.na(z$effective_from)] <- as.Date("1900-01-01")
  z$effective_until[is.na(z$effective_until)] <- as.Date("2999-12-31")
  z$verified <- tolower(trimws(as.character(z$verified))) %in% c("true", "t", "1", "yes")
  mab_assert(all(z$verified) && all(nzchar(trimws(z$source))),
             "Every enabled venue-fee row must be verified and sourced.")
  mab_assert(all(z$effective_until > z$effective_from), "Invalid fee effective interval.")
  attr(z, "source_path") <- normalizePath(path, winslash = "/", mustWork = TRUE)
  attr(z, "source_sha256") <- mab_sha256(path)
  z
}

mab_fee_configuration <- function(
    realistic_brokerage_usd_per_contract_side = 1,
    fee_table = NULL,
    apply_exchange_fees = TRUE,
    apply_clearing_fees = TRUE,
    apply_regulatory_fees = FALSE) {
  brokerage <- as.numeric(realistic_brokerage_usd_per_contract_side)
  mab_assert(length(brokerage) == 1L && is.finite(brokerage) && brokerage == 1,
             "The primary benchmark requires USD 1 brokerage per contract-side.")
  mab_assert(!isTRUE(apply_regulatory_fees),
             "Regulatory fees and transaction taxes are excluded from the primary convention.")
  structure(list(
    realistic_brokerage_usd_per_contract_side = brokerage,
    fee_table = fee_table,
    fee_table_path = if (is.null(fee_table)) NA_character_ else attr(fee_table, "source_path"),
    fee_table_sha256 = if (is.null(fee_table)) NA_character_ else attr(fee_table, "source_sha256"),
    apply_exchange_fees = isTRUE(apply_exchange_fees),
    apply_clearing_fees = isTRUE(apply_clearing_fees),
    apply_regulatory_fees = FALSE,
    apply_transaction_taxes = FALSE,
    fee_basis = "USD 1 brokerage plus verified venue charges per contract-side"
  ), class = "mab_fee_configuration")
}

mab_resolve_fee_rows <- function(spec, timestamp, fee_config, fill_side) {
  z <- fee_config$fee_table
  if (is.null(z) || !nrow(z)) return(data.frame())
  date <- as.Date(mab_time(timestamp), tz = "Europe/London")
  active <- z$effective_from <= date & date < z$effective_until &
    z$charge_side %in% c("both", fill_side)
  keys <- list(
    exact_contract = c(spec$resolved_key[[1L]], spec$original_key[[1L]]),
    root = toupper(spec$Root[[1L]]), exchange = toupper(spec$ExchangeCode[[1L]])
  )
  rows <- list()
  for (component in unique(z$fee_component)) {
    chosen <- integer()
    for (key_type in c("exact_contract", "root", "exchange")) {
      value <- if (key_type == "exact_contract") z$exact_contract_key else z[[key_type]]
      hit <- which(active & z$fee_component == component & z$fee_key_type == key_type &
                     value %in% keys[[key_type]])
      mab_assert(length(hit) <= 1L, paste0("Ambiguous ", component, " fee match."))
      if (length(hit)) { chosen <- hit; break }
    }
    if (length(chosen)) rows[[length(rows) + 1L]] <- z[chosen, , drop = FALSE]
  }
  mab_bind_rows(rows)
}

mab_calculate_fill_fees <- function(quantity_change, spec, timestamp, fee_config,
                                    bfix, max_fx_age_days = 7,
                                    apply_explicit_fees = TRUE,
                                    fill_price_displayed = NA_real_,
                                    fee_timestamp = timestamp,
                                    fx_rate_override = NULL) {
  quantity <- abs(as.numeric(quantity_change))
  mab_assert(length(quantity) == 1L && is.finite(quantity) && quantity >= 0,
             "Fill quantity change must be finite.")
  zero <- !isTRUE(apply_explicit_fees) || quantity == 0
  brokerage_usd <- if (zero) 0 else quantity * fee_config$realistic_brokerage_usd_per_contract_side
  side <- if (as.numeric(quantity_change) > 0) "buy" else "sell"
  rows <- if (zero) NULL else mab_resolve_fee_rows(spec, fee_timestamp, fee_config, side)
  exchange_usd <- clearing_usd <- 0
  total_native <- 0
  currencies <- character()
  if (!is.null(rows) && nrow(rows)) for (i in seq_len(nrow(rows))) {
    enabled <- (rows$fee_component[[i]] == "exchange" && fee_config$apply_exchange_fees) ||
      (rows$fee_component[[i]] == "clearing" && fee_config$apply_clearing_fees) ||
      (rows$fee_component[[i]] == "combined_exchange_clearing" &&
         fee_config$apply_exchange_fees && fee_config$apply_clearing_fees)
    basis_quantity <- switch(
      rows$fee_basis[[i]],
      per_contract = quantity,
      per_physical_unit = quantity * as.numeric(spec$ContractQuantity[[1L]]),
      transaction_notional = {
        mab_assert(is.finite(fill_price_displayed) && fill_price_displayed > 0,
                   "Transaction-notional fees require the causal fill price.")
        quantity * fill_price_displayed *
          as.numeric(spec$PointValueNativePerDisplayedPoint[[1L]])
      }
    )
    native <- if (enabled) basis_quantity * rows$fee_rate[[i]] else 0
    currency <- rows$fee_currency[[i]]
    fx <- if (!is.null(fx_rate_override) && currency %in% names(fx_rate_override)) {
      as.numeric(fx_rate_override[[currency]])
    } else {
      mab_align_fx(fee_timestamp, currency, bfix, max_fx_age_days)$fx_rate_usd_per_native[[1L]]
    }
    mab_assert(is.finite(fx) && fx > 0, "Fee FX override must be finite and positive.")
    usd <- native * fx
    if (rows$fee_component[[i]] == "exchange") exchange_usd <- exchange_usd + usd
    else if (rows$fee_component[[i]] == "clearing") clearing_usd <- clearing_usd + usd
    else { exchange_usd <- exchange_usd + usd / 2; clearing_usd <- clearing_usd + usd / 2 }
    total_native <- total_native + native; currencies <- c(currencies, rows$fee_currency[[i]])
  }
  total <- brokerage_usd + exchange_usd + clearing_usd
  data.frame(
    fee_matching_method = if (is.null(rows) || !nrow(rows)) "brokerage_only" else "brokerage_plus_venue",
    fee_currency = if (length(unique(currencies)) == 1L) unique(currencies) else "USD",
    brokerage_native = 0, exchange_fee_native = NA_real_, clearing_fee_native = NA_real_,
    regulatory_fee_native = 0, total_fee_native = if (length(unique(currencies)) == 1L) total_native else NA_real_,
    fee_fx_rate = NA_real_, brokerage_usd = brokerage_usd,
    exchange_fee_usd = exchange_usd, clearing_fee_usd = clearing_usd,
    regulatory_fee_usd = 0, total_fee_usd = total,
    stringsAsFactors = FALSE
  )
}
