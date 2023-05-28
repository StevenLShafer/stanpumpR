# Set line types for different types of plots

setLinetypes.lineType <- function(plasmaLinetype, effectsiteLinetype) {
  list(plasmaLinetype = plasmaLinetype, effectsiteLinetype = effectsiteLinetype)
}

setLinetypes <- function(normalization="none",dplasmaLinetype,deffectsiteLinetype)
{
  result <- switch(
    normalization,
    "Peak plasma" = setLinetypes.lineType(dplasmaLinetype,"blank"),
    "Peak effect site" = setLinetypes.lineType("blank",deffectsiteLinetype),
    "MEAC" = setLinetypes.lineType("blank",deffectsiteLinetype),
    "none" = setLinetypes.lineType(dplasmaLinetype,deffectsiteLinetype)
  )

  if (is.null(result)) {
    result <- setLinetypes.lineType("blank", "solid")
  }

  return(result)
}
