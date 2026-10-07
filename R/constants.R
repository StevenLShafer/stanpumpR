UNIT_YEAR <- 1
UNIT_MONTH <- 0.08333333 # 1/12
UNIT_KG <- 1
UNIT_LB <- 0.453592
UNIT_CM <- 1
UNIT_INCH <- 2.54

SEX_MALE <- "male"
SEX_FEMALE <- "female"
SEX_VALUES <- c(SEX_MALE, SEX_FEMALE)

# Which plasma curve a drug's tPeak was observed against.  getDrugPK() solves
# ke0 so the effect site peaks at tPeak, and the answer depends on whether the
# observation followed an intravenous bolus or an oral dose.
ROUTE_IV <- "IV"
ROUTE_PO <- "PO"
ROUTE_IM <- "IM"
ROUTE_IN <- "IN"
TPEAK_ROUTES <- c(ROUTE_IV, ROUTE_PO)

# Every route a dose can take, in the order the units dropdowns list them.  A
# dose's route is carried by its Units string; see doseRoute() in R/routes.R.
DOSE_ROUTES <- c(ROUTE_IV, ROUTE_PO, ROUTE_IM, ROUTE_IN)

# CYP2D6 metaboliser phenotype.  Four categories, using the CPIC terms the
# genotyping laboratories report, rather than the three the UI carried before
# any drug used them.  "normal" is the reference: a drug's formation parameters
# are published relative to it.
CYP2D6_POOR         <- "poor"
CYP2D6_INTERMEDIATE <- "intermediate"
CYP2D6_NORMAL       <- "normal"
CYP2D6_ULTRARAPID   <- "ultrarapid"
CYP2D6_VALUES  <- c(CYP2D6_POOR, CYP2D6_INTERMEDIATE, CYP2D6_NORMAL,
                    CYP2D6_ULTRARAPID)
CYP2D6_DEFAULT <- CYP2D6_NORMAL

# Baseline serum osmolality, mOsm/kg, before any osmotic agent is given.  Read
# only by models that declare an `osmolality` argument (mannitol), which add
# their own contribution on top of it.  The default, 280, is within the normal
# adult range (about 275 to 295); the user enters the patient's measured value.
OSMOLALITY_DEFAULT <- 280
MIN_OSMOLALITY <- 200
MAX_OSMOLALITY <- 400

# Serum creatinine, mg/dL, for the renally cleared models.  Optional: when the
# patient's is not entered (NULL, or NA from an empty field) each model uses
# the assumed normal value for the patient's sex (R/renalFunction.R).
MIN_CREATININE <- 0.2
MAX_CREATININE <- 15

MIN_AGE <- 0
MAX_AGE <- 90
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
defaultSex <- SEX_FEMALE

# Resolution for linear interpolation
RESOLUTION <- 100

# Max number of emails a single session can send out, to limit abuse
EMAIL_SESSION_LIMIT <- 25

MAX_DOSE_ROWS <- 500L
MAX_EVENT_ROWS <- 100L
MAX_TARGET_ROWS <- 20L
MAX_DOSE_VALUE <- 1e9
MAX_INPUT_TEXT <- 2000L
MAX_PLOT_WIDTH <- 4096L
MIN_PLOT_WIDTH <- 100L
MAX_YAXIS_HEIGHT <- 350L
MIN_YAXIS_HEIGHT <- 150L
MAX_DRUGNAME_LENGTH <- 128L
MAX_TIME_STRING_LENGTH <- 32L
MAX_UNIT_STRING_LENGTH <- 32L

# Be sure there are more items below then potential facets on the simulation plot
#                     1     2     3     4     5     6     7     8     9    10    11    12    13    14   15
bolusUnits <- c("g","mg","mcg", "ng","g/kg","mg/kg","mcg/kg","ng/kg")
infusionUnits <- c("g/min","g/hr","g/kg/hr","mg/min","mg/hr","mg/kg/min","mg/kg/hr","mcg/min","mcg/hr","mcg/kg/min","mcg/kg/hr")
poUnits <- c("g PO", "g/kg PO", "mg PO", "mg/kg PO", "mcg PO", "mcg/kg PO")
inUnits <- c("g IN", "g/kg IN", "mg IN", "mg/kg IN", "mcg IN", "mcg/kg IN")
imUnits <- c("g IM", "g/kg IM", "mg IM", "mg/kg IM", "mcg IM", "mcg/kg IM")

# Constant-rate oral input: a daily oral dose spread evenly over the day, as
# Pollak, Bouillon and Shafer modelled long-term oral amiodarone (400 mg/d as
# 16.7 mg/h for 24 h; R/drugs_amiodarone.R).  Oral by route, so doseRoute()
# reads it as PO, but a rate by kind: simCpCe() runs each row as the drug's
# running input rate until the next, like an infusion row, on the drug's
# apparent oral parameters (no ka, no bioavailability).  Kept out of
# infusionUnits, which lists the intravenous rates.  (Claude Code,
# 2026-10-07, at the request of Steven L. Shafer.)
poRateUnits <- c("mg/day PO")

allUnits <- c(bolusUnits, infusionUnits, poUnits, poRateUnits, inUnits, imUnits)

# Target-controlled infusion (tci.R).  The "dose" of a target row is the target
# concentration, in the drug's concentration units per ml.
TCI_UNIT_PLASMA <- "Plasma target"
TCI_UNIT_EFFECT <- "Effect site target"
tciUnits <- c(TCI_UNIT_PLASMA, TCI_UNIT_EFFECT)
TCI_INTERVAL <- 10 / 60        # minutes between rate changes (10 s, as STANPUMP)
TCI_PLASMA_SWITCH <- 0.05      # effect site this close to target: hold the plasma
TCI_MAX_RATE <- Inf            # pump ceiling in base mass units per minute

# Scheduled (repeating) doses (scheduled.R).  A bolus, PO, IM or IN unit with
# one of these suffixes, e.g. "mg PO bid", gives the dose at the entered time
# and then again every interval (minutes) until the end of the plot.
SCHEDULE_INTERVALS <- c(qd = 24 * 60, bid = 12 * 60, tid = 8 * 60, qid = 6 * 60)
scheduledUnits <- as.vector(t(outer(
  c(bolusUnits, poUnits, imUnits, inUnits), names(SCHEDULE_INTERVALS), paste
)))

# Units for the inhaled gases (Class "gas" in drugDefaults_global.csv): carrier
# gases are flowmeter settings in L/min, potent agents are vaporizer settings in %.
# Kept out of allUnits, which lists the mass-based units offered for IV/PO/IM/IN
# drugs, but they are legitimate entries in the dose table.
gasUnits <- c("L/min", "%")

MINS_PER_HOUR <- 60
MINS_PER_DAY  <- 60 * 24
MINS_PER_WEEK <- 60 * 24 * 7
MINS_PER_YEAR <- 525600  # more than 52 weeks because of leap years

maxtimes <- data.frame(
  times = c(MINS_PER_HOUR * c(1, 2, 4, 6, 12),
            MINS_PER_DAY * c(1, 2, 4),
            MINS_PER_WEEK * c(1, 2, 4, 8, 16, 32),
            MINS_PER_YEAR),
  steps = c(10, 15, 30, 60, 120,
            MINS_PER_DAY / c(6, 3, 2),
            MINS_PER_DAY * c(1, 2, 4), MINS_PER_WEEK * c(1, 2, 4),
            MINS_PER_YEAR / 12)
)

REFERENCE_TIME_NONE <- "none"
NORMALIZE_NONE <- "none"
PK_EVENT_DEFAULT <- "default"

PLOT_ID_EVENTS      <- "Events"
PLOT_ID_MEAC        <- "MEAC"
PLOT_ID_INTERACTION <- "Interaction"
PLOT_NAME_EVENTS      <- "Events"
PLOT_NAME_MEAC        <- "% MEAC"
PLOT_NAME_INTERACTION <- "p response"

DEBUG_LEVEL_OFF <- 0
DEBUG_LEVEL_NORMAL <- 1
DEBUG_LEVEL_VERBOSE <- 2

# If a drug reference contains a URL at the very end of the citation, and the URL
# is one of the following websites and is served over https, then it will be
# shown in the UI as a link.
CITATION_WEBSITES <- c(
  "PubMed" = "pubmed.ncbi.nlm.nih.gov",
  "DOI"    = "doi.org"
)

DEFAULT_CONFIG <- list(
  title = "stanpumpR",
  source_link = "https://github.com/StevenLShafer/stanpumpR",
  debug = DEBUG_LEVEL_OFF,
  long_title = FALSE
)
