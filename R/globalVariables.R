
facetFont <-      c(  20,   18,   18,   16,   14,   14,   14,   13,   13,   12,   11,   10,    9,    9,   8,  8)
labelFont <-      c(  16,   15,   14,   13,   12,   11,   9,    8,    8,    7,    6,    6,    5,    5,   5,  5)
facetSeparator <- c( " ",  "\n", "\n", "\n", "\n", "\n", "\n", "\n", "\n", "\n", "\n", "\n", "\n", "\n", "\n", "\n")
facetAngle <-     c( 270,  270,  180,  180,  180,  180,  180,  180,  180,  180,  180,  180,  180,  180,  180, 180)
facetSpacing <-   c(2.25,    2,  1.75,  1.5,    1,    1,  0.8,  0.7,  0.6,  0.6,  0.5,  0.5,  0.4,  0.3,  0.3, 0.2)

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

# defaultAge <- NULL
# defaultAgeUnit <- character(0)
# defaultWeight <- NULL
# defaultWeightUnit <- character(0)
# defaultHeight <- NULL
# defaultHeightUnit <- character(0)
# defaultSex <- character(0)

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
