# Check if a number is between two numbers
`%btwn%` <- function(x, range) {
  x >= range[1] & x <= range[2]
}

identicalTable <- function(x, y) {
  rownames(x) <- NULL
  rownames(y) <- NULL
  identical(x, y)
}
