smoke_expect <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
  invisible(TRUE)
}

smoke_equal <- function(x, y, tolerance = 1e-10, message = "Values differ") {
  ok <- length(x) == length(y) && all(is.na(x) == is.na(y)) &&
    all(abs(as.numeric(x[!is.na(x)]) - as.numeric(y[!is.na(y)])) <= tolerance)
  smoke_expect(ok, message)
}

smoke_quotes <- function() {
  raw <- utils::read.csv(repo_path("data", "sample", "tiny_quotes.csv"), stringsAsFactors = FALSE)
  raw$timestamp <- as.POSIXct(raw$timestamp, tz = "Europe/London")
  fields <- c(
    "timestamp", "generic", "contract", "bid", "ask", "close"
  )
  list(
    y = raw[raw$leg == "y", fields],
    x = raw[raw$leg == "x", fields]
  )
}
