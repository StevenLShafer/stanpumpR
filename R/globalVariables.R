UNIT_YEAR <- 1
UNIT_MONTH <- 0.08333333 # 1/12
UNIT_KG <- 1
UNIT_LB <- 0.453592
UNIT_CM <- 1
UNIT_INCH <- 2.54

MIN_AGE <- 0
MAX_AGE <- 110
MIN_WEIGHT <- 0.1
MAX_WEIGHT <- 500
MIN_HEIGHT <- 10
MAX_HEIGHT <- 200

# default variables
defaultAge <- 50
defaultAgeUnit <- UNIT_YEAR
defaultWeight <- 60
defaultWeightUnit <- UNIT_KG
defaultHeight <- 66
defaultHeightUnit <- UNIT_INCH
defaultSex <- "female"

# Resolution for linear interpolation
RESOLUTION <- 100

# Be sure there are more items below then potential facets on the simulation plot
#                     1     2     3     4     5     6     7     8     9    10    11    12    13    14   15
bolusUnits <- c("g","mg","mcg", "ng","g/kg","mg/kg","mcg/kg","ng/kg")
infusionUnits <- c("mg/min","mg/hr","mg/kg/min","mg/kg/hr","mcg/min","mcg/hr","mcg/kg/min","mcg/kg/hr")
poUnits <- c("g PO", "g/kg PO", "mg PO", "mg/kg PO", "mcg PO", "mcg/kg PO")
inUnits <- c("g IN", "g/kg IN", "mg IN", "mg/kg IN", "mcg IN", "mcg/kg IN")
imUnits <- c("g IM", "g/kg IM", "mg IM", "mg/kg IM", "mcg IM", "mcg/kg IM")

allUnits <- c(bolusUnits, infusionUnits, poUnits, inUnits, imUnits)


maxtimes <- data.frame(
  times = c(10, 30, 60, 90, 120, 180, 240, 300, 360, 480, 600, 720, 1440, 1680, 1920, 2880, 4320, 5760,7200, 1000000 ),
  steps = c( 1,  5, 10, 15,  15,  30,  30,  60,  60,  60, 120, 120, 240,  240, 240,  480,   480,  720, 720, 1440)
)

PLOT_NAME_EVENTS      <- "Events"
PLOT_NAME_MEAC        <- "MEAC"
PLOT_NAME_INTERACTION <- "Interaction"

DEBUG_LEVEL_OFF <- 0
DEBUG_LEVEL_NORMAL <- 1
DEBUG_LEVEL_VERBOSE <- 2

DEFAULT_CONFIG <- list(
  title = "stanpumpR",
  help_link = "https://steveshafer.shinyapps.io/stanpumpR_HelpPage",
  debug = DEBUG_LEVEL_OFF,
  long_title = FALSE
)
