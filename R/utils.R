# Check if a number is between two numbers
`%btwn%` <- function(x, range) {
  x >= range[1] & x <= range[2]
}

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

# Check if the contents of two dataframes are identical, ignoring the row names
identicalTable <- function(x, y) {
  rownames(x) <- NULL
  rownames(y) <- NULL
  identical(x, y)
}

# Check if an email address look valid
isEmailValid <- function(email) {
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
