python <- Sys.getenv("PYTHON", unset = Sys.which("python3"))
if (!nzchar(python)) stop("Python is required for quote-cleaner tests.", call. = FALSE)
status <- system2(
  python, repo_path("tools", "quote_cleaning", "tests", "test_quote_cleaning.py"),
  env = "PYTHONDONTWRITEBYTECODE=1"
)
smoke_expect(identical(status, 0L), "Standalone quote-cleaner unit tests failed")
cat("QUOTE_CLEANING_PYTHON_CONTRACT_PASS\n")
