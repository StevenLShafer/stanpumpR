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
# Regional anesthesia: a local anesthetic injected into tissue (a peripheral
# nerve block or a wound infiltration), absorbed first-order from the tissue
# into the systemic circulation.
ROUTE_RA <- "RA"
TPEAK_ROUTES <- c(ROUTE_IV, ROUTE_PO)

# Every route a dose can take, in the order the units dropdowns list them.  A
# dose's route is carried by its Units string; see doseRoute() in R/routes.R.
DOSE_ROUTES <- c(ROUTE_IV, ROUTE_PO, ROUTE_IM, ROUTE_IN, ROUTE_RA)

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
# the assumed normal value for the patient's age and sex (R/renalFunction.R).
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
# Regional anesthesia (ROUTE_RA): a single injection into tissue, the whole
# dose entering a depot that is absorbed first-order.  No rate units: a
# perineural catheter infusion would need a rate through the depot, which the
# engines do not carry, and no scheduled frequencies.
raUnits <- c("g RA", "g/kg RA", "mg RA", "mg/kg RA", "mcg RA", "mcg/kg RA")

# Constant-rate oral input: a daily oral dose spread evenly over the day, as
# Pollak, Bouillon and Shafer modelled long-term oral amiodarone (400 mg/d as
# 16.7 mg/h for 24 h; R/drugs_amiodarone.R).  Oral by route, so doseRoute()
# reads it as PO, but a rate by kind: simCpCe() runs each row as the drug's
# running input rate until the next, like an infusion row, on the drug's
# apparent oral parameters (no ka, no bioavailability).  Kept out of
# infusionUnits, which lists the intravenous rates.  (Claude Code,
# 2026-10-07, at the request of Steven L. Shafer.)
poRateUnits <- c("mg/day PO")

allUnits <- c(bolusUnits, infusionUnits, poUnits, poRateUnits, inUnits, imUnits, raUnits)

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
# Kept out of allUnits, which lists the mass-based units offered for IV/PO/IM/IN/RA
# drugs, but they are legitimate entries in the dose table.
gasUnits <- c("L/min", "%")

# The startup drug menu (R/startup-drugs.R).  The categories, in the order the
# menu lists them; each drug's is the Category column of
# drugDefaults_global.csv, and a drug with no category is not offered.
DRUG_CATEGORIES <- c(
  "Hypnotics and sedatives",
  "Opioids",
  "Oral analgesics",
  "Neuromuscular blockade",
  "Inhaled anesthetics",
  "Antibiotics",
  "Corticosteroids",
  "Local anesthetics",
  "Other"
)
# Ticked when the menu opens: the four drugs the app opened with before it
# had a menu.
STARTUP_DRUGS_DEFAULT <- c("propofol", "fentanyl", "remifentanil", "rocuronium")
# A chosen drug starts with one zero-dose row in its Default.Units, except
# these, which start with a bolus row and an infusion row, as they always have.
STARTUP_UNITS <- list(
  propofol     = c("mg", "mcg/kg/min"),
  remifentanil = c("mcg", "mcg/kg/min")
)
# Blank rows below the chosen drugs, to type into
STARTUP_BLANK_ROWS <- 6L

MINS_PER_HOUR <- 60
MINS_PER_DAY  <- 60 * 24
MINS_PER_WEEK <- 60 * 24 * 7
MINS_PER_YEAR <- 525600  # more than 52 weeks because of leap years

# Time units (R/utils-time.R).  The unit is a display and entry setting only:
# the engine, the scenarios, the events, the simulation cache and the exported
# Time columns are in minutes whatever it is.  A bare number typed into the
# dose table is in the unit; an entry with a colon is a clock time or an
# elapsed H:MM and is never scaled.  Values are minutes per unit.
TIME_UNITS <- c(minutes = 1, hours = MINS_PER_HOUR, days = MINS_PER_DAY, weeks = MINS_PER_WEEK)
TIME_UNIT_DEFAULT <- "minutes"
# Clock ("Actual time") entry addresses only the 24 hours after the procedure
# start, so it is offered for these units only; days and weeks are elapsed.
CLOCK_TIME_UNITS <- c("minutes", "hours")
TIME_MODES <- c("clock", "relative")
# A time converted to another unit is rounded to TIME_SNAP_DIGITS decimal
# places of a minute (0.001 min) and written with TIME_STRING_DIGITS
# significant digits.  Ten digits make every conversion, and any chain of them,
# return the identical minutes on a 0.001 minute grid up to a year (six digits
# did not: day 5 became 0.714286 weeks, 7200.00288 minutes, which moved a
# scheduled stop past a repeat).
TIME_STRING_DIGITS <- 10
TIME_SNAP_DIGITS <- 3

# TCI target rows and the inhaled agents are simulated only on plots of this
# length or less.  Beyond a week the TCI controller writes ever more rows, and
# the gas engine's uptake coupling is frozen over steps of about maximum/601.
ACUTE_MAX_PLOT_MINUTES <- MINS_PER_WEEK

# Drugs that are only meaningful over weeks to months.  Adding one to a plot
# shorter than a week offers to switch the Time units to days, 365 days.
LONG_TERM_DRUGS <- c("amiodarone")
LONG_TERM_PLOT_MINUTES <- 365 * MINS_PER_DAY

# The Max time choices for each time unit: the durations (minutes) and the
# tick spacing (minutes) of each.  "minutes" and "hours" offer the same
# durations, so switching between them never changes Max time.  365 days is
# not a multiple of its 30-day ticks: the axis runs past the last tick.
MAX_TIMES <- list(
  minutes = data.frame(
    times = MINS_PER_HOUR * c(1, 2, 4, 6, 8, 12, 18, 24),
    steps = c(10, 15, 30, 60, 60, 120, 180, 240)
  ),
  hours = data.frame(
    times = MINS_PER_HOUR * c(1, 2, 4, 6, 8, 12, 18, 24),
    steps = MINS_PER_HOUR * c(0.25, 0.25, 0.5, 1, 1, 2, 3, 4)
  ),
  days = data.frame(
    times = MINS_PER_DAY * c(2, 3, 4, 7, 14, 28, 56, 91, 182, 365),
    steps = MINS_PER_DAY * c(0.5, 0.5, 1, 1, 2, 7, 7, 7, 14, 30)
  ),
  weeks = data.frame(
    times = MINS_PER_WEEK * c(4, 8, 13, 26, 39, 52),
    steps = MINS_PER_WEEK * c(1, 1, 1, 2, 3, 4)
  )
)
# The word each unit's Max time choices are labelled in ("1 hour", "365 days")
MAX_TIME_LABEL_UNITS <- c(minutes = "hour", hours = "hour", days = "day", weeks = "week")
# Every Max time any unit offers.  input$maximum outside this is refused; one
# inside it but not in the current unit's list is a moment when the browser has
# not yet caught up with a change of unit.
MAX_TIME_VALUES <- sort(unique(unlist(lapply(MAX_TIMES, `[[`, "times"))))
# How close (minutes) the last dose or event may come to the end of the plot
# before the plot is lengthened to show what follows it.  30 minutes is the
# original rule, kept so that the minute and hour plots are unchanged.
TIME_EXTEND_MARGIN <- c(minutes = 30, hours = 30, days = MINS_PER_DAY / 2, weeks = MINS_PER_WEEK)

REFERENCE_TIME_NONE <- "none"
NORMALIZE_NONE <- "none"
PK_EVENT_DEFAULT <- "default"

PLOT_ID_EVENTS      <- "Events"
PLOT_ID_MEAC        <- "MEAC"
PLOT_ID_INTERACTION <- "Interaction"
PLOT_NAME_EVENTS      <- "Events"
PLOT_NAME_MEAC        <- "% MEAC"
PLOT_NAME_INTERACTION <- "p response"

# The time line the closed-form engines simulate on (R/simulationTimeGrid.R).
# PRE_DOSE_OFFSET puts a point just before each dose, where the time until
# threshold jumps.  Up to GRID_LEGACY_MAXIMUM (a day) each gap between knots
# gets the GRID_LOG_POINTS geometric offsets it always had, so those plots are
# unchanged point for point.  Beyond it the fill starts at
# maximum / GRID_FINE_POINTS (or at the drug's own start, if that is later)
# and no step is longer than maximum / GRID_UNIFORM_POINTS.
#
# The two counts were chosen by measurement, the closed form evaluated at 8 to
# 32 points inside every step against the straight line the plot draws across
# it (Claude Code, 2026-10-07):
#
#   GRID_UNIFORM_POINTS = 500.  On a 52-week plot of a two-compartment model
#   with Pollak 2000's amiodarone parameters (half-lives 17.3 h and 55.4 d),
#   given as Pollak's seven-step oral regimen at constant rates or as
#   400 mg/day stopped at day 180, no chord strays from the curve by more than
#   0.4% of the peak.  1000 does no better -- the largest error sits where the
#   geometric steps hand over to uniform ones, and that point moves with the
#   step -- and doubles the points on a plot with few doses.  The uniform
#   steps there are 17.5 h, two to four pixels on a full-width plot.
#
#   GRID_FINE_POINTS = 20000.  What decides it is the peak after an oral dose,
#   which nothing else puts a point near.  The earliest in the library is
#   oxycodone's plasma peak, 30 min after the dose.  Starting the fill at
#   maximum / 20000 (26 min on a 52-week plot) draws every oral drug's plasma
#   and effect-site peak, under daily dosing for 52 weeks, to within 0.4% of
#   its height; starting it at maximum / 2000 (4.4 h) drew oxycodone's at 42%,
#   and maximum / 10000 at 94%.  On a plot shorter than 20000 x start (about
#   two weeks) the fill starts exactly where it always did.
#
#   Cost, 10 mg of oxycodone four times a day for 52 weeks with the time until
#   threshold: 24,752 points and 1.7 s, against 52,416 points and 10.2 s on the
#   line before this change (5,824 points and 0.4 s at 2000).  Once a day:
#   9,100 points and 0.5 s, against 15,652 and 2.7 s.
PRE_DOSE_OFFSET     <- 0.01
GRID_LOG_POINTS     <- 41
GRID_LEGACY_MAXIMUM <- MINS_PER_DAY
GRID_UNIFORM_POINTS <- 500
GRID_FINE_POINTS    <- 20000

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
