showIntroModal <- function() {
  showModal(
    modalDialog(
      title = "Welcome to stanpumpR",
      p(
        "stanpumpR, derived from the original STANPUMP program developed at
        Stanford University,  performs pharmacokinetic simulations
        based on mathematical models published in the peer-reviewed
        literature. stanpumpR is intended to help clinicians and investigators
        better understand the mathematical implications of published models.
        stanpumpR is only an advisory program. How these models are applied to
        individual patients is a matter of clinical judgment by the health care
        provider."
      ),
      tags$button(
        type = "button",
        class = "btn btn-warning",
        `data-dismiss` = "modal",
        "OK"
      ),
      footer = NULL,
      easyClose = TRUE,
      fade = TRUE,
      size = "s"
    )
  )
}

checkNumericCovariates <- function(age, weight, height, errorFx = NULL) {
  msg <- ""
  success <- TRUE
  if (!is.numeric(age) || !is.numeric(weight) || !is.numeric(height)) {
    success <- FALSE
  }
  if (!age %btwn% c(MIN_AGE, MAX_AGE)) {
    msg <- glue("Age must be between {MIN_AGE} and {MAX_AGE}")
    success <- FALSE
  }
  if (!weight %btwn% c(MIN_WEIGHT, MAX_WEIGHT)) {
    msg <- glue("Weight must be between {MIN_WEIGHT} and {MAX_WEIGHT}")
    success <- FALSE
  }
  if (!height %btwn% c(MIN_HEIGHT, MAX_HEIGHT)) {
    msg <- glue("Height must be between {MIN_HEIGHT} and {MAX_HEIGHT}")
    success <- FALSE
  }

  if (nzchar(msg) && is.function(errorFx)) {
    errorFx(msg)
  }
  success
}

getInitialValues <- function() {
  reactiveValues(
    age                = 0,
    weight             = 0,
    height             = 0,
    sex                = 0,
    ageUnit            = 0,
    heightUnit         = "",
    weightUnit         = "",
    plotMaximum        = 60,
    plasmaLinetype     = "blank",
    effectsiteLinetype = "solid",
    normalization      = "none",
    title              = 0,
    caption            = "",
    typical            = "Range",
    logY               = FALSE,
    DT                 = doseTableInit, # Used to determine of replot flag needs to be set, e.g., sameTable(DT, DT)
    # Set to DT after processDoseTable. Note that the NULL assignment is assumed without
    # being explicit, as done here. This is mostly to help keep inventory of the prior
    # variables
    ET                 = eventTableInit, # Used to determine if plot needs to be set.
    referenceTime      = "none",
    plotMEAC           = FALSE,
    plotInteraction    = FALSE,
    plotCost           = FALSE,
    plotEvents         = FALSE,
    plotRecovery       = FALSE,
    holdPlot           = FALSE,
    DrugTimeUnits      = "",
    timeDT             = 0,
    Refresh            = 0, # Used to see if the refresh button has been clicked
    RefreshFlag        = FALSE, # Used to force simulationPlot to update with refresh button
    referenceTime      = "none"
  )
}

getInitialDrugs <- function() {
  drugs <- reactiveValues()
  isolate(
    for (idx in seq(nrow(drugDefaults_global))) {
      drug <- drugDefaults_global$Drug[idx]
      drugs[[drug]] <- NULL
      drugs[[drug]]$Color <- drugDefaults_global$Color[idx]
      drugs[[drug]]$endCe <- drugDefaults_global$endCe[idx]
      drugs[[drug]]$endCeText <- drugDefaults_global$endCeText[idx]
    }
  )
  drugs
}

# This is not a pure function - it's given a reactiveValues and modifies it
recalculatePK <- function(drugs, drugDefaults, age, weight, height, sex, DEBUG = FALSE) {
  for (idx in seq(nrow(drugDefaults))) {
    drug <- drugDefaults$Drug[idx]
    outputComments("Getting PK for", drug, echo = DEBUG)
    drugs[[drug]] <- modifyList(
      drugs[[drug]],
      getDrugPK(
        drug = drug,
        weight = weight,
        height = height,
        age = age,
        sex = sex,
        drugDefaults = drugDefaults[idx, ]
      )
    )
    drugs[[drug]]$DT <- NULL # Remove old dose table, if any
    drugs[[drug]]$CpCe <- NULL # Remove old simulations results, if any
    drugs[[drug]]$equiSpace <- NULL # Ditto
    drugs[[drug]]$maxCp <- 1
    drugs[[drug]]$maxCe <- 1
    drugs[[drug]]$recovery <- 1
  }
}
