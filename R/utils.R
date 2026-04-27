# Check if a number is between two numbers
`%btwn%` <- function(x, range) {
  x >= range[1] & x <= range[2]
}

identicalTable <- function(x, y) {
  rownames(x) <- NULL
  rownames(y) <- NULL
  identical(x, y)
}

isEmailValid <- function(email) {
  regex_email <- "^\\w+([-+.']\\w+)*@\\w+([-.]\\w+)*\\.\\w{2,}([-.]\\w+)*$"
  nchar(email) == attr(regexpr(regex_email, email, perl = FALSE), "match.length")
}

drugHasNonZeroDoses <- function(dt, drug) {
  drugDoses <- dt[dt$Drug == drug & dt$Dose != "", ]
  any(suppressWarnings(as.numeric(drugDoses$Dose)) != 0, na.rm = TRUE)
}
