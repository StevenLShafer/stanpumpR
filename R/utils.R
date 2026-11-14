#' Check if a value is a valid single number within bounds
#'
#' @param x Value to check
#' @param min Minimum allowable value (optional)
#' @param max Maximum allowable value (optional)
#' @return Logical value indicating whether the value is valid
#' @noRd
is_valid_number <- function(x, min = -Inf, max = Inf) {
  is.numeric(x) && length(x) == 1L && is.finite(x) && x >= min && x <= max
}

# One number from a value a browser or a link sent, or NA: anything but a
# single number or string -- NULL, a vector, a list -- is NA.  The
# suppressWarnings(as.numeric(x)) it replaces gives NA for most of those, but
# stops with an error on a list holding a vector ({"a":[1,2]} in a link),
# which in app_ui() served the page as HTTP 500 and in an observer ended the
# session (Carbon's UI audit, 2026-11-14, F5).
asScalarNumeric <- function(x) {
  if (!is.atomic(x) || length(x) != 1L || is.factor(x)) return(NA_real_)
  suppressWarnings(as.numeric(x))
}

# One string from a value a browser sent, or `default`: anything but a
# single non-missing string is `default`
asScalarString <- function(x, default = "") {
  if (is.character(x) && length(x) == 1L && !is.na(x)) x else default
}

# Check if the contents of two dataframes are identical, ignoring the row names
identicalTable <- function(x, y) {
  rownames(x) <- NULL
  rownames(y) <- NULL
  identical(x, y)
}

# Check if an email address look valid
isEmailValid <- function(email) {
  # One string, or FALSE: NULL, NA or a vector gave logical(0), NA or a
  # vector, which the Email Slide observers stopped on, ending the session
  # (Carbon's UI audit, 2026-11-14, F6D)
  if (!is.character(email) || length(email) != 1 || is.na(email)) return(FALSE)
  regex_email <- "^\\w+([-+.']\\w+)*@\\w+([-.]\\w+)*\\.\\w{2,}([-.]\\w+)*$"
  nchar(email) == attr(regexpr(regex_email, email, perl = FALSE), "match.length")
}

# Get a snapshot of the R version and packages, to help with troubleshooting in production
getInstalledPackagesInfo <- function() {
  pkgs <- utils::installed.packages()[, c("Package", "Version"), drop = FALSE]
  pkgs <- pkgs[order(tolower(pkgs[, "Package"])), , drop = FALSE]
  paste0(
    R.version.string, "\n", nrow(pkgs), " packages:\n",
    paste0("  ", pkgs[, "Package"], " ", pkgs[, "Version"], collapse = "\n")
  )
}
