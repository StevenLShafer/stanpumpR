UNIT_YEAR <- 1
UNIT_MONTH <- 0.08333333 # 1/12
UNIT_KG <- 1
UNIT_LB <- 0.453592
UNIT_CM <- 1
UNIT_INCH <- 2.54

MIN_AGE <- 0
MAX_AGE <- 110
DEIDENTIFIED_MAX_AGE <- 89
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

# Max number of slides a single session may email, to limit abuse of the
# shared sending account (the app is unauthenticated and public).
EMAIL_SESSION_LIMIT <- 25

# Server-side security limits. Browser controls are not a security boundary:
# every value can be forged over the Shiny websocket or through a bookmark.
MAX_DOSE_ROWS <- 500L
MAX_EVENT_ROWS <- 100L
MAX_TARGET_ROWS <- 20L
MAX_DOSE_VALUE <- 1e9
MAX_INPUT_TEXT <- 2000L
MAX_PLOT_WIDTH <- 4096

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

PLOT_ID_EVENTS      <- "Events"
PLOT_ID_MEAC        <- "MEAC"
PLOT_ID_INTERACTION <- "Interaction"
PLOT_NAME_EVENTS      <- "Events"
PLOT_NAME_MEAC        <- "% MEAC"
PLOT_NAME_INTERACTION <- "p response"

DEBUG_LEVEL_OFF <- 0
DEBUG_LEVEL_NORMAL <- 1
DEBUG_LEVEL_VERBOSE <- 2

DEFAULT_CONFIG <- list(
  title = "stanpumpR",
  # "Examples and Help" nav link. Points to the separately deployed help app on
  # shinyapps.io, whose source is maintained in its own repository:
  # https://github.com/StevenLShafer/stanpumpR_HelpPage
  help_link = "https://steveshafer.shinyapps.io/stanpumpR_HelpPage",
  debug = DEBUG_LEVEL_OFF,
  allow_url_debug = FALSE,
  # URL bookmarking is the default so saved simulations are shareable as plain
  # links and work on hosts without server-side state storage (e.g.
  # shinyapps.io, which throws "server is not configured for saving sessions to
  # disk" when bookmark_mode = "server"). The URL carries no confidential data:
  # recipient, comments, and exact age are excluded (see bookmarksToExclude) and
  # ages >= 90 are normalized to 89 before persistence. URL state is decoded by
  # Shiny via jsonlite (safeFromJSON) -- never unserialize()/eval() -- and every
  # restored value is re-validated on the reactive path (validateDoseTableInput
  # gates the only eval(call(drug,...)) sink), so a crafted URL cannot inject
  # code. Set "server" only on a host that supports disk-backed bookmarks (e.g.
  # Posit Connect / Connect Cloud); "disable" turns saved-state sharing off.
  bookmark_mode = "url",
  email_enabled = FALSE,
  email_smtp_host = "smtp.gmail.com",
  email_smtp_port = 587,
  email_smtp_ssl = TRUE,
  long_title = FALSE
)
