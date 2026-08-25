#' @keywords internal
#' @import shiny
#' @importFrom magrittr %>%
#' @importFrom memoise memoise
"_PACKAGE"

# Names that R CMD check can't see a binding for, because they are either
# column names used non-standardly inside dplyr/ggplot2 calls, or created as a
# side effect at run time
utils::globalVariables(c(
  "Drug", "Label", "MEAC", "Recovery", "Site", "Time", "Y",
  "endCe", "new", "outputString", "xmax", "xmin", "y", "ymax", "ymin"
))

## usethis namespace: start
## usethis namespace: end
NULL
