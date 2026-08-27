mab_read_bfix <- function(path, strict_expected_structure = TRUE,
                          timezone = "Europe/London", tolerance = 5e-7) {
  mab_assert(file.exists(path), paste0("Missing Bloomberg BFIX workbook: ", path))
  mab_assert(requireNamespace("readxl", quietly = TRUE),
             "The 'readxl' package is required to ingest Bloomberg BFIX data.")
  sheets <- readxl::excel_sheets(path)
  mab_assert(length(sheets) >= 1L, "BFIX workbook contains no worksheet.")
  header <- suppressMessages(readxl::read_excel(
    path, sheet = sheets[[1L]], range = "A1:M6", col_names = FALSE
  ))
  tickers <- toupper(trimws(as.character(unlist(header[4L, c(2L, 5L, 8L, 11L)]))))
  tickers <- gsub("[[:space:]]+", " ", tickers)
  expected_tickers <- c("EUR L160 CURNCY", "GBP L160 CURNCY", "CNY L160 CURNCY", "INR L160 CURNCY")
  mab_assert(identical(tickers, expected_tickers),
             "BFIX workbook ticker headers do not match EUR/GBP/CNY/INR L160.")

  columns <- c(
    "fixing_date", "eur_last", "eur_bid", "eur_ask", "gbp_last", "gbp_bid", "gbp_ask",
    "cny_last", "cny_bid", "cny_ask", "inr_last", "inr_bid", "inr_ask"
  )
  raw <- suppressWarnings(suppressMessages(readxl::read_excel(
    path, sheet = sheets[[1L]], skip = 6L, col_names = columns,
    col_types = c("date", rep("numeric", 12L))
  )))
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  raw$fixing_date <- as.Date(raw$fixing_date)
  mab_assert(!anyNA(raw$fixing_date) && !anyDuplicated(raw$fixing_date),
             "BFIX dated rows must have unique, valid dates.")

  definitions <- data.frame(
    native_currency = c("EUR", "GBP", "CNY", "INR"),
    raw_ticker = c("EUR L160 Curncy", "GBP L160 Curncy", "CNY L160 Curncy", "INR L160 Curncy"),
    raw_quote_direction = c("USD per EUR", "USD per GBP", "CNY per USD", "INR per USD"),
    inverted = c(FALSE, FALSE, TRUE, TRUE),
    prefix = c("eur", "gbp", "cny", "inr"),
    stringsAsFactors = FALSE
  )
  rows <- lapply(seq_len(nrow(definitions)), function(i) {
    definition <- definitions[i, , drop = FALSE]
    prefix <- definition$prefix
    last <- as.numeric(raw[[paste0(prefix, "_last")]])
    bid <- as.numeric(raw[[paste0(prefix, "_bid")]])
    ask <- as.numeric(raw[[paste0(prefix, "_ask")]])
    inverted <- isTRUE(definition$inverted)
    standard_last <- if (inverted) 1 / last else last
    standard_bid <- if (inverted) 1 / ask else bid
    standard_ask <- if (inverted) 1 / bid else ask
    publication <- as.POSIXct(
      paste(raw$fixing_date, "16:00:00"), tz = timezone, format = "%Y-%m-%d %H:%M:%S"
    )
    data.frame(
      native_currency = definition$native_currency,
      raw_ticker = definition$raw_ticker,
      raw_quote_direction = definition$raw_quote_direction,
      raw_px_last = last,
      raw_px_bid = bid,
      raw_px_ask = ask,
      standardised_last = standard_last,
      standardised_bid = standard_bid,
      standardised_ask = standard_ask,
      fixing_date = raw$fixing_date,
      fixing_timestamp = publication,
      timezone = timezone,
      inversion_flag = inverted,
      populated_fixing = is.finite(last) & is.finite(bid) & is.finite(ask),
      source_path = normalizePath(path, winslash = "/", mustWork = TRUE),
      source_sha256 = mab_sha256(path),
      stringsAsFactors = FALSE
    )
  })
  bfix <- mab_bind_rows(rows)
  populated <- bfix$populated_fixing
  mab_assert(all(is.finite(bfix$standardised_last[populated]) &
                   is.finite(bfix$standardised_bid[populated]) &
                   is.finite(bfix$standardised_ask[populated]) &
                   bfix$standardised_last[populated] > 0 &
                   bfix$standardised_bid[populated] > 0 &
                   bfix$standardised_ask[populated] > 0),
             "Populated BFIX rates must be finite and positive.")
  mab_assert(all(
    bfix$standardised_bid[populated] <= bfix$standardised_last[populated] + tolerance &
      bfix$standardised_last[populated] <= bfix$standardised_ask[populated] + tolerance
  ), "Standardised BFIX bid/last/ask ordering is invalid.")
  partial <- with(bfix, xor(populated_fixing,
                            is.finite(raw_px_last) & is.finite(raw_px_bid) & is.finite(raw_px_ask)))
  mab_assert(!any(partial), "BFIX rows contain partial or inconsistent quote triplets.")

  nonfix <- sort(unique(bfix$fixing_date[!bfix$populated_fixing]))
  if (isTRUE(strict_expected_structure)) {
    mab_assert(nrow(raw) == 232L, "Expected 232 dated BFIX rows.")
    mab_assert(identical(range(raw$fixing_date), as.Date(c("2025-09-01", "2026-07-21"))),
               "Unexpected BFIX date coverage.")
    mab_assert(all(vapply(split(bfix, bfix$native_currency), function(x) {
      sum(x$populated_fixing) == 229L
    }, logical(1L))), "Expected 229 populated fixing rows per currency.")
    mab_assert(identical(nonfix, as.Date(c("2025-12-25", "2026-01-01", "2026-04-03"))),
               "Unexpected common non-fixing dates.")
  }

  attr(bfix, "source_path") <- normalizePath(path, winslash = "/", mustWork = TRUE)
  attr(bfix, "source_sha256") <- mab_sha256(path)
  attr(bfix, "validation") <- data.frame(
    check = c(
      "dated_rows", "currency_count", "populated_rows_per_currency", "first_date",
      "last_date", "common_non_fixing_dates", "reciprocal_bid_ask_reversal",
      "standardised_quote_order"
    ),
    status = "pass",
    observed = c(
      nrow(raw), length(unique(bfix$native_currency)),
      paste(vapply(split(bfix, bfix$native_currency), function(x) sum(x$populated_fixing), integer(1L)), collapse = ";"),
      as.character(min(raw$fixing_date)), as.character(max(raw$fixing_date)),
      paste(nonfix, collapse = ";"), "CNY/INR bid=1/raw ask; ask=1/raw bid", "bid<=last<=ask"
    ),
    stringsAsFactors = FALSE
  )
  bfix
}

mab_align_fx <- function(event_timestamp, native_currency, bfix,
                         max_fx_age_days = 7, rate_field = "standardised_last") {
  event_timestamp <- mab_time(event_timestamp, tz = "Europe/London")
  currency <- toupper(trimws(as.character(native_currency)))
  mab_assert(length(event_timestamp) == 1L && !is.na(event_timestamp),
             "FX alignment requires one valid event timestamp.")
  mab_assert(length(currency) == 1L && nzchar(currency),
             "FX alignment requires one native currency.")
  mab_assert(length(max_fx_age_days) == 1L && is.finite(max_fx_age_days) && max_fx_age_days >= 0,
             "max_fx_age_days must be one finite non-negative number.")
  if (identical(currency, "USD")) {
    return(data.frame(
      event_timestamp = event_timestamp,
      native_currency = "USD",
      bfix_date = as.Date(event_timestamp, tz = "Europe/London"),
      publication_timestamp = event_timestamp,
      fx_rate_usd_per_native = 1,
      rate_source_field = "USD identity",
      fx_age_calendar_days = 0L,
      carry_forward_flag = FALSE,
      raw_ticker = "USD identity",
      inversion_flag = FALSE,
      standardised_bid = 1,
      standardised_last = 1,
      standardised_ask = 1,
      stringsAsFactors = FALSE
    ))
  }
  mab_assert(currency %in% unique(bfix$native_currency),
             paste0("No BFIX series exists for fee/P&L currency: ", currency))
  eligible <- which(
    bfix$native_currency == currency & bfix$populated_fixing &
      !is.na(bfix$fixing_timestamp) & bfix$fixing_timestamp <= event_timestamp
  )
  mab_assert(length(eligible) > 0L, paste0(
    "No causally available BFIX exists for ", currency, " at ", event_timestamp, "."
  ))
  i <- eligible[which.max(bfix$fixing_timestamp[eligible])]
  age <- as.integer(
    as.Date(event_timestamp, tz = "Europe/London") - bfix$fixing_date[i]
  )
  mab_assert(age >= 0L, "FX alignment attempted to use a future fixing.")
  mab_assert(age <= max_fx_age_days, paste0(
    "Latest causal ", currency, " BFIX is ", age,
    " calendar days old, exceeding MAX_FX_AGE_DAYS = ", max_fx_age_days, "."
  ))
  rate <- as.numeric(bfix[[rate_field]][i])
  mab_assert(is.finite(rate) && rate > 0, "Selected causal BFIX rate is invalid.")
  event_date <- as.Date(event_timestamp, tz = "Europe/London")
  data.frame(
    event_timestamp = event_timestamp,
    native_currency = currency,
    bfix_date = bfix$fixing_date[i],
    publication_timestamp = bfix$fixing_timestamp[i],
    fx_rate_usd_per_native = rate,
    rate_source_field = rate_field,
    fx_age_calendar_days = age,
    carry_forward_flag = event_date != bfix$fixing_date[i],
    raw_ticker = bfix$raw_ticker[i],
    inversion_flag = bfix$inversion_flag[i],
    standardised_bid = bfix$standardised_bid[i],
    standardised_last = bfix$standardised_last[i],
    standardised_ask = bfix$standardised_ask[i],
    stringsAsFactors = FALSE
  )
}

mab_align_fx_many <- function(event_timestamp, native_currency, bfix,
                              max_fx_age_days = 7) {
  mab_assert(length(event_timestamp) == length(native_currency),
             "FX vector inputs must have equal length.")
  mab_bind_rows(Map(
    mab_align_fx, event_timestamp, native_currency,
    MoreArgs = list(bfix = bfix, max_fx_age_days = max_fx_age_days)
  ))
}
