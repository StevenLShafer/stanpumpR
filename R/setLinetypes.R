# Set line types for different types of plots

setLinetypes.lineType <- function(plasmaLinetype, effectsiteLinetype) {
  list(plasmaLinetype = plasmaLinetype, effectsiteLinetype = effectsiteLinetype)
}

setLinetypes <- function(normalization = "none")
{
  result <- switch(
    normalization,
    "Peak plasma" = setLinetypes.lineType("solid","blank"),
    "Peak effect site" = setLinetypes.lineType("blank","solid"),
    "MEAC" = setLinetypes.lineType("blank","solid"),
    "none" = setLinetypes.lineType("blank","solid")
  )

  if (is.null(result)) {
    result <- setLinetypes.lineType("blank", "solid")
  }

  return(result)
}
