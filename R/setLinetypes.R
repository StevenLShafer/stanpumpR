# Set line types for different types of plots

lineType <- function(plasmaLinetype, effectsiteLinetype) {
  list(plasmaLinetype = plasmaLinetype, effectsiteLinetype = effectsiteLinetype)
}

setLinetypes <- function(normalization = "none")
{
  result <- switch(
    normalization,
    "Peak plasma" = lineType("solid","blank"),
    "Peak effect site" = lineType("blank","solid"),
    "MEAC" = lineType("blank","solid"),
    "none" = lineType("blank","solid")
  )

  if (is.null(result)) {
    result <- lineType("blank", "solid")
  }

  return(result)
}
