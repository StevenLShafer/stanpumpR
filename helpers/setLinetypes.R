# Set line types for different types of plots
setLinetypes <- function(normalization = "none")
{
  plasmaLinetype     <- "blank"
  effectsiteLinetype <- "solid"
  switch(
    normalization,
    "Peak plasma" = {
      plasmaLinetype     <- "solid"
      effectsiteLinetype <- "blank"
    },
    "Peak effect site" = {
      plasmaLinetype     <- "blank"
      effectsiteLinetype <- "solid"
    },
    "MEAC" = {
      plasmaLinetype     <- "blank"
      effectsiteLinetype <- "solid"
    },
    "none" = {
      plasmaLinetype     <- "blank"
      effectsiteLinetype <- "solid"
    },
    "none" = {
      plasmaLinetype     <- "blank"
      effectsiteLinetype <- "solid"
    }
  )
  return(list(plasmaLinetype = plasmaLinetype, effectsiteLinetype = effectsiteLinetype))
}
