# Single Paper Review: the paper's drug, built in memory.
#
# From an extraction (R/paper-review-schema.R) this builds what getDrugPK()
# would get from a drugs_<name>.R function and a drugDefaults_global.csv row,
# without either: a model list and a one-row defaults frame, both held in the
# session.  getDrugPK(model = ...) and simCpCe() then run it exactly as they
# run a library drug.  Nothing here writes a file or touches the library.
#
# Units are converted from closed lists (paperReviewUnit()), and covariate
# effects are evaluated from closed forms (paperReviewCovariateMultiplier()):
# nothing the extraction contains is evaluated as code.
#
# See docs/paper-review.md.  (Claude Code, 2026-10-30, at the request of
# Steven L. Shafer.)

# The patient the paper's typical values describe when it prints no
# reference: the population's central values, or these.
PAPER_REVIEW_DEFAULT_PATIENT <- list(weight = 70, height = 170, age = 40, sex = "male")

# An absent compartment, as the library writes one (R/drugs_esmolol.R,
# R/drugs_hydralazine.R): a placeholder volume with no clearance into it.
# getDrugPK() refuses a zero volume.  The first live review (a
# two-compartment paper) found that this module had written zeros.
PAPER_REVIEW_ABSENT_VOLUME <- 1

# The most discrete doses one regimen row expands into (two years of daily
# tablets is 730).
PAPER_REVIEW_MAX_REPEATS <- 2000

#' Is this dose row a constant input rather than a discrete dose?
#'
#' An infusion; a row with a rate (per minute, hour or day); or an amount
#' given over a duration that is not a repeated dose.
#' @keywords internal
paperReviewIsRateDose <- function(d)
{
  perTime <- d$per_time %or% "none"
  if (perTime != "none") return(TRUE)
  if (identical(d$route, "iv_infusion")) return(TRUE)
  if (identical(d$route, "iv_bolus")) return(FALSE)
  is.finite(paperReviewNum(d$duration)) && !is.finite(paperReviewNum(d$interval))
}

# stanpumpR's Concentration.Units (amount per mL) for each printed unit, and
# the factor that turns the printed number into that unit.
PAPER_REVIEW_CONCENTRATION_MAP <- list(
  "pg/mL"  = list(units = "pg",  factor = 1),
  "ng/L"   = list(units = "pg",  factor = 1),
  "ng/mL"  = list(units = "ng",  factor = 1),
  "mcg/L"  = list(units = "ng",  factor = 1),
  "mcg/mL" = list(units = "mcg", factor = 1),
  "mg/L"   = list(units = "mcg", factor = 1)
)

#' Convert a printed unit to stanpumpR's internal units
#'
#' @param units one of \code{PAPER_REVIEW_PARAMETER_UNITS}
#' @returns a list: \code{kind} ("volume", "clearance", "rate", "time",
#'   "fraction" or NA), \code{factor} (multiply the printed value by it) and
#'   \code{perKg}
#' @keywords internal
paperReviewUnit <- function(units)
{
  u <- function(kind, factor, perKg = FALSE) list(kind = kind, factor = factor, perKg = perKg)
  switch(units,
    "L"         = u("volume", 1),
    "mL"        = u("volume", 1e-3),
    "L/kg"      = u("volume", 1, TRUE),
    "mL/kg"     = u("volume", 1e-3, TRUE),
    "L/min"     = u("clearance", 1),
    "mL/min"    = u("clearance", 1e-3),
    "L/h"       = u("clearance", 1 / 60),
    "mL/h"      = u("clearance", 1e-3 / 60),
    "L/day"     = u("clearance", 1 / 1440),
    "L/min/kg"  = u("clearance", 1, TRUE),
    "mL/min/kg" = u("clearance", 1e-3, TRUE),
    "L/h/kg"    = u("clearance", 1 / 60, TRUE),
    "mL/h/kg"   = u("clearance", 1e-3 / 60, TRUE),
    "1/min"     = u("rate", 1),
    "1/h"       = u("rate", 1 / 60),
    "1/day"     = u("rate", 1 / 1440),
    "s"         = u("time", 1 / 60),
    "min"       = u("time", 1),
    "h"         = u("time", 60),
    "day"       = u("time", 1440),
    "fraction"  = u("fraction", 1),
    "percent"   = u("fraction", 0.01),
    "unitless"  = u("fraction", 1),
    u(NA_character_, NA_real_))
}

# The kind of quantity each role must be.
PAPER_REVIEW_ROLE_KIND <- c(
  CL = "clearance", Q2 = "clearance", Q3 = "clearance",
  V1 = "volume", V2 = "volume", V3 = "volume",
  k10 = "rate", k12 = "rate", k21 = "rate", k13 = "rate", k31 = "rate",
  ka = "rate", ke0 = "rate", F = "fraction", tlag = "time", tPeak = "time")

# Minutes per time unit.
paperReviewMinutes <- function(units)
  switch(units, s = 1 / 60, min = 1, h = 60, day = 1440, week = 10080, NA_real_)

# The simulator's time units, and their names.
PAPER_REVIEW_SIM_TIME_UNITS <- c(minutes = "min", hours = "h", days = "day", weeks = "week")

# The simulator's Max time: what it opens at, and the longest it takes (the
# main simulator's longest Max time, 365 days), in minutes.
PAPER_REVIEW_MAX_TIME_DEFAULT <- 240
PAPER_REVIEW_MAX_TIME_LIMIT <- max(MAX_TIME_VALUES)

#' The simulator's Max time, in minutes, or NA when it cannot be used
#'
#' A blank numeric input is \code{NA}, which \code{\%or\%} passes through (it
#' supplies its fallback only for \code{NULL}), and which reached
#' \code{simCpCe()} as \code{max(NA, ...)}: the plot showed R's own "missing
#' value where TRUE/FALSE needed" (the UI review of 2026-11-04).  So blank,
#' non-numeric, non-finite, zero, negative or longer than
#' \code{PAPER_REVIEW_MAX_TIME_LIMIT} are all NA here, for the caller to say so.
#'
#' @param value \code{input$maximum}, in the unit shown.
#' @param minutesPerUnit minutes per unit shown.
#' @returns minutes, or \code{NA_real_}.
#' @keywords internal
paperReviewMaxTimeMinutes <- function(value, minutesPerUnit)
{
  if (length(value) != 1 || length(minutesPerUnit) != 1) return(NA_real_)
  x <- suppressWarnings(as.numeric(value)) * suppressWarnings(as.numeric(minutesPerUnit))
  if (is.na(x) || !is.finite(x) || x <= 0 || x > PAPER_REVIEW_MAX_TIME_LIMIT) return(NA_real_)
  x
}

#' The time unit the paper uses for a regimen
#'
#' The unit of a figure showing the regimen, since that is how the paper
#' presents its time course; else the unit of the regimen's doses; else
#' minutes.  Seconds become minutes.
#'
#' @param extraction the extraction
#' @param regimen a regimen, or NULL
#' @returns one of PAPER_REVIEW_SIM_TIME_UNITS
#' @keywords internal
paperReviewRegimenTimeUnit <- function(extraction, regimen)
{
  pick <- function(u) if (is.null(u) || !u %in% PAPER_REVIEW_SIM_TIME_UNITS) NULL else u
  if (!is.null(regimen$id)) {
    for (f in extraction$figures %or% list()) {
      shows <- identical(f$regimen_id, regimen$id) ||
        any(vapply(f$series %or% list(), function(s) identical(s$regimen_id, regimen$id), logical(1)))
      if (shows && !is.null(u <- pick(f$time_units))) return(u)
    }
  }
  for (d in regimen$doses %or% list())
    if (!is.null(u <- pick(d$time_units))) return(u)
  "min"
}

#' A printed concentration in the model's units
#'
#' @param units one of \code{PAPER_REVIEW_CONCENTRATION_UNITS}
#' @param modelUnits the drug's Concentration.Units ("pg", "ng", "mcg")
#' @param molecularWeight g/mol, for a molar unit
#' @returns the factor that turns a printed value into \code{modelUnits} per
#'   mL, or NA when there is none
#' @keywords internal
paperReviewConcentrationFactor <- function(units, modelUnits, molecularWeight = NULL)
{
  perMl <- c(pg = 1e-3, ng = 1, mcg = 1e3)     # ng/mL per unit/mL
  if (units %in% c("nmol/L", "mcmol/L")) {
    if (!is.numeric(molecularWeight) || !isTRUE(molecularWeight > 0)) return(NA_real_)
    ngPerMl <- molecularWeight / 1000 * if (units == "mcmol/L") 1000 else 1
    return(ngPerMl / perMl[[modelUnits]])
  }
  map <- PAPER_REVIEW_CONCENTRATION_MAP[[units]]
  if (is.null(map)) return(NA_real_)
  map$factor * perMl[[map$units]] / perMl[[modelUnits]]
}

# --- Small accessors over the extraction -------------------------------------

`%or%` <- function(a, b) if (is.null(a)) b else a

paperReviewNum <- function(x)
  if (is.numeric(x) && length(x) == 1 && is.finite(x)) x else NA_real_

#' The parameters of one role
#' @keywords internal
paperReviewParameters <- function(extraction, role, route = NULL)
{
  Filter(function(p) identical(p$role, role) &&
           (is.null(route) || is.null(p$route) || identical(p$route, route)),
         extraction$parameters %or% list())
}

#' The typical value of a role, in internal units, at a reference weight
#'
#' @returns a list: \code{value} (NA when absent or not convertible),
#'   \code{perKg}, \code{location}, \code{problem}
#' @keywords internal
paperReviewParameterValue <- function(extraction, role, weight, route = NULL)
{
  found <- paperReviewParameters(extraction, role, route)
  if (length(found) == 0)
    return(list(value = NA_real_, perKg = FALSE, location = NA_character_, problem = NULL))
  p <- found[[1]]
  unit <- paperReviewUnit(p$units %or% "other")
  value <- paperReviewNum(p$value)
  problem <- NULL
  if (is.na(unit$kind) || !identical(unit$kind, PAPER_REVIEW_ROLE_KIND[[role]])) {
    problem <- paste0(role, " is printed in '", p$units, "', which is not a ",
                      PAPER_REVIEW_ROLE_KIND[[role]], " unit the review can convert.")
    value <- NA_real_
  } else {
    value <- value * unit$factor * if (unit$perKg) weight else 1
  }
  list(value = value, perKg = unit$perKg, location = p$location %or% NA_character_,
       problem = problem, entry = p)
}

# --- Covariates -------------------------------------------------------------

#' The patient the paper's typical values describe
#'
#' Each covariate's printed reference value where there is one, else the
#' population's central value, else PAPER_REVIEW_DEFAULT_PATIENT.
#'
#' @returns a list: \code{weight}, \code{height}, \code{age}, \code{sex} and
#'   \code{source}, a named character vector saying where each came from
#' @keywords internal
paperReviewReferencePatient <- function(extraction)
{
  pop <- extraction$population %or% list()
  ref <- function(covariate) {
    for (e in extraction$covariate_effects %or% list())
      if (identical(e$covariate, covariate) && !is.na(paperReviewNum(e$reference_value)))
        return(paperReviewNum(e$reference_value))
    NA_real_
  }
  pick <- function(name, covariate, summary) {
    r <- ref(covariate)
    if (!is.na(r)) return(list(value = r, source = "printed reference value"))
    c0 <- paperReviewNum(summary$central)
    if (!is.na(c0)) return(list(value = c0, source = paste("population", summary$central_kind %or% "central value")))
    list(value = PAPER_REVIEW_DEFAULT_PATIENT[[name]], source = "not printed; stanpumpR default")
  }
  w <- pick("weight", "weight", pop$weight_kg)
  h <- pick("height", "height", pop$height_cm)
  a <- pick("age", "age", pop$age_years)
  nF <- paperReviewNum(pop$n_female); nM <- paperReviewNum(pop$n_male)
  sex <- if (!is.na(nF) && !is.na(nM) && nF > nM) "female" else "male"
  sexSource <- if (is.na(nF) || is.na(nM)) "not printed; stanpumpR default"
    else if (nF == nM) "equal numbers; male taken" else "population majority"
  list(weight = w$value, height = h$value, age = a$value, sex = sex,
       source = c(weight = w$source, height = h$source, age = a$source, sex = sexSource))
}

# A covariate's value for a patient, or NA when the simulator does not take it.
paperReviewCovariateValue <- function(covariate, patient)
{
  switch(covariate,
    weight = patient$weight,
    height = patient$height,
    age = patient$age,
    sex_female = as.numeric(identical(patient$sex, "female")),
    bmi = patient$weight / (patient$height / 100)^2,
    lean_body_mass = lbmJames(patient$weight, patient$height, patient$sex),
    fat_free_mass = ffmAlSallami(patient$weight, patient$height, patient$age, patient$sex),
    NA_real_)
}

#' The multiplier one covariate effect applies to its parameter
#'
#' @param effect one entry of \code{extraction$covariate_effects}
#' @param patient a list of weight, height, age, sex
#' @param reference the reference patient, whose value stands in for a
#'   missing reference value
#' @returns a list: \code{multiplier} (1 when the effect is not applied),
#'   \code{applied}, \code{reason}
#' @keywords internal
paperReviewCovariateMultiplier <- function(effect, patient, reference)
{
  notApplied <- function(reason) list(multiplier = 1, applied = FALSE, reason = reason)
  form <- effect$form %or% "other"
  theta <- paperReviewNum(effect$coefficient)
  if (identical(form, "other")) return(notApplied("its form is not one the review can evaluate"))
  if (is.na(theta)) return(notApplied("no coefficient was printed"))
  x <- paperReviewCovariateValue(effect$covariate %or% "other", patient)
  if (is.na(x)) return(notApplied("the simulator does not take this covariate; held at its reference"))
  if (identical(form, "category_multiplier"))
    return(list(multiplier = if (x == 1) theta else 1, applied = TRUE, reason = NULL))
  ref <- paperReviewNum(effect$reference_value)
  if (is.na(ref)) ref <- paperReviewCovariateValue(effect$covariate, reference)
  m <- switch(form,
    power = (x / ref)^theta,
    linear = 1 + theta * (x - ref),
    exponential = exp(theta * (x - ref)))
  list(multiplier = m, applied = TRUE, reason = NULL)
}

#' The product of the covariate multipliers on one role
#' @keywords internal
paperReviewRoleMultiplier <- function(extraction, role, patient, reference)
{
  m <- 1
  for (e in extraction$covariate_effects %or% list())
    if (identical(e$parameter_role, role))
      m <- m * paperReviewCovariateMultiplier(e, patient, reference)$multiplier
  m
}

# --- The model --------------------------------------------------------------

#' The routes the paper's model can be dosed by, as stanpumpR route words
#' @keywords internal
paperReviewExtravascularRoute <- function(extraction)
{
  routes <- unlist(extraction$structure$routes %or% list())
  ev <- intersect(c("oral", "im", "sc", "intranasal", "sublingual"), routes)
  if (length(ev) == 0) NA_character_ else ev[[1]]
}

PAPER_REVIEW_ROUTE_SUFFIX <- c(oral = "PO", im = "IM", sc = "SC",
                               intranasal = "IN", sublingual = "SL")

#' Disposition volumes and clearances for one patient
#'
#' From the printed volumes and clearances, or the printed micro constants
#' and V1, each with its covariate effects applied.  The derived routes to a
#' model (from half-lives and Vss, or from exponentials) are taken by
#' paperReviewIdentifiability(), which passes its result as \code{closed}.
#'
#' @param extraction the extraction
#' @param patient the patient to evaluate at
#' @param closed NULL, or a list of v1..v3, cl1..cl3 at the reference patient
#'   from a closure route, in L and L/min
#' @returns a list: \code{v1}..\code{v3}, \code{cl1}..\code{cl3} (zeros for
#'   absent compartments), \code{problems}
#' @keywords internal
paperReviewDisposition <- function(extraction, patient, closed = NULL)
{
  reference <- paperReviewReferencePatient(extraction)
  n <- paperReviewNum(extraction$structure$compartments)
  if (is.na(n) || !n %in% 1:3) n <- 1
  problems <- character(0)
  val <- function(role) {
    v <- paperReviewParameterValue(extraction, role, patient$weight)
    if (!is.null(v$problem)) problems <<- c(problems, v$problem)
    v$value * paperReviewRoleMultiplier(extraction, role, patient, reference)
  }
  if (!is.null(closed)) {
    # A closure is solved at the reference patient; covariates scale it by
    # role (clearances by CL's and Q's effects, volumes by V's).
    m <- function(role) paperReviewRoleMultiplier(extraction, role, patient, reference)
    w <- patient$weight / reference$weight
    pk <- function(x, role, perKg) x * m(role) * if (perKg) w else 1
    perKg <- isTRUE(closed$perKg)
    out <- list(v1 = pk(closed$v1, "V1", perKg),
                v2 = if (is.null(closed$v2)) PAPER_REVIEW_ABSENT_VOLUME else pk(closed$v2, "V2", perKg),
                v3 = if (is.null(closed$v3)) PAPER_REVIEW_ABSENT_VOLUME else pk(closed$v3, "V3", perKg),
                cl1 = pk(closed$cl1, "CL", perKg), cl2 = pk(closed$cl2 %or% 0, "Q2", perKg),
                cl3 = pk(closed$cl3 %or% 0, "Q3", perKg))
    out$problems <- problems
    return(out)
  }
  v1 <- val("V1")
  if (!is.na(val("CL")) || is.na(val("k10"))) {
    out <- list(v1 = v1, cl1 = val("CL"),
                v2 = if (n >= 2) val("V2") else PAPER_REVIEW_ABSENT_VOLUME,
                cl2 = if (n >= 2) val("Q2") else 0,
                v3 = if (n >= 3) val("V3") else PAPER_REVIEW_ABSENT_VOLUME,
                cl3 = if (n >= 3) val("Q3") else 0)
  } else {
    k12 <- if (n >= 2) val("k12") else 0; k21 <- if (n >= 2) val("k21") else 1
    k13 <- if (n >= 3) val("k13") else 0; k31 <- if (n >= 3) val("k31") else 1
    out <- list(v1 = v1, cl1 = val("k10") * v1,
                v2 = if (n >= 2) k12 * v1 / k21 else PAPER_REVIEW_ABSENT_VOLUME, cl2 = k12 * v1,
                v3 = if (n >= 3) k13 * v1 / k31 else PAPER_REVIEW_ABSENT_VOLUME, cl3 = k13 * v1)
  }
  out$problems <- unique(problems)
  out
}

#' The paper's drug as a stanpumpR model and defaults row
#'
#' @param extraction the extraction
#' @param patient a list of weight, height, age, sex
#' @param closed a closure from paperReviewIdentifiability(), or NULL
#' @param absorption the extravascular absorption to give it, one entry of
#'   paperReviewAbsorptions() (R/paper-review-absorption.R), or NULL for none
#' @param absorptions all of them, kept on the drug so that a check can build
#'   the drug for another regimen (paperReviewDrugFor())
#' @returns a list: \code{model} (what a drug function returns),
#'   \code{defaults} (a one-row drugDefaults frame), \code{units} (the dose
#'   units offered), \code{route}, \code{absorption}, \code{ok},
#'   \code{problems}
#' @keywords internal
paperReviewDrug <- function(extraction, patient, closed = NULL, absorption = NULL,
                            absorptions = NULL)
{
  disposition <- paperReviewDisposition(extraction, patient, closed)
  problems <- disposition$problems
  set <- disposition[c("v1", "v2", "v3", "cl1", "cl2", "cl3")]
  ok <- all(vapply(set, function(x) is.finite(x) && x >= 0, logical(1))) &&
    isTRUE(set$v1 > 0) && isTRUE(set$cl1 > 0)
  if (!ok) problems <- c(problems,
    "The printed parameters do not give a complete, positive disposition model for this patient.")

  # Extravascular absorption, derived per regimen (R/paper-review-absorption.R):
  # ka and F at the reference patient, scaled by any printed covariate effect
  # on them; the dose's analyte fraction rides on the bioavailability, so a
  # dose is entered as the paper doses it.
  reference <- paperReviewReferencePatient(extraction)
  route <- NA_character_
  if (!is.null(absorption) && isTRUE(absorption$ok)) {
    route <- absorption$route
    field <- if (route == "sc") "SC" else PAPER_REVIEW_ROUTE_SUFFIX[[route]]
    set[[paste0("ka_", field)]] <- absorption$ka *
      paperReviewRoleMultiplier(extraction, "ka", patient, reference)
    set[[paste0("bioavailability_", field)]] <- absorption$F * absorption$fraction *
      paperReviewRoleMultiplier(extraction, "F", patient, reference)
    set[[paste0("tlag_", field)]] <- absorption$tlag
  }

  model <- list(PK = list(default = set), reference = "Single Paper Review (in memory)")
  effect <- extraction$structure$effect_site %or% "none"
  ke0 <- paperReviewParameterValue(extraction, "ke0", patient$weight)$value
  tPeak <- paperReviewParameterValue(extraction, "tPeak", patient$weight)$value
  if (identical(effect, "ke0") && is.finite(ke0) && ke0 > 0) {
    model$ke0 <- ke0
  } else if (identical(effect, "tpeak") && is.finite(tPeak) && tPeak > 0) {
    model$tPeak <- tPeak
  } else {
    model$tPeak <- 0
  }

  mapped <- PAPER_REVIEW_CONCENTRATION_MAP[[extraction$drug$concentration_units %or% "other"]]
  concUnits <- if (is.null(mapped)) "ng" else mapped$units
  if (is.null(mapped)) problems <- c(problems, paste0(
    "Concentrations are reported in '", extraction$drug$concentration_units,
    "'; the simulator plots ng/mL."))

  routes <- unlist(extraction$structure$routes %or% list())
  units <- character(0)
  if (any(c("iv_bolus", "iv_infusion") %in% routes) || is.na(route))
    units <- c("mg", "mcg", "mg/kg", "mcg/kg", "mg/min", "mg/hr", "mcg/min",
               "mcg/kg/min", "mcg/kg/hr", "mg/kg/hr")
  if (!is.na(route)) {
    suffix <- PAPER_REVIEW_ROUTE_SUFFIX[[route]]
    units <- c(units, paste(c("mg", "mcg", "mg/kg", "mcg/kg"), suffix))
  }
  # A paper that doses as a constant extravascular input (daily oral doses
  # modelled as continuous) gets those rate units too.
  rateRoutes <- unique(unlist(lapply(extraction$regimens %or% list(), function(r)
    vapply(r$doses %or% list(), function(d)
      if ((d$route %or% "other") %in% names(PAPER_REVIEW_ROUTE_SUFFIX) && paperReviewIsRateDose(d))
        d$route else NA_character_, ""))))
  for (rr in rateRoutes[!is.na(rateRoutes)])
    units <- c(units, paste0(c("mg/day", "mg/hr"), " ", PAPER_REVIEW_ROUTE_SUFFIX[[rr]]))
  units <- unique(units)
  defaults <- data.frame(
    Drug = paperReviewDrugName(extraction), Concentration.Units = concUnits,
    Bolus.Units = units[1], Infusion.Units = "mg/hr", Default.Units = units[1],
    Color = "#1f4e9c", Lower = 0, Upper = 0, Typical = 0, MEAC = 0, endCe = 0,
    stringsAsFactors = FALSE)
  defaults$Units <- I(list(units))
  list(model = model, defaults = defaults, units = units, route = route,
       absorption = absorption, absorptions = absorptions, closed = closed,
       ok = ok, problems = unique(problems))
}

#' Does a regimen have a discrete extravascular dose, which needs an
#' absorption?  A constant oral input does not: it enters the apparent
#' disposition directly.
#' @keywords internal
paperReviewNeedsAbsorption <- function(regimen)
  any(vapply(regimen$doses %or% list(), function(d)
    (d$route %or% "other") %in% names(PAPER_REVIEW_ROUTE_SUFFIX) && !paperReviewIsRateDose(d),
    logical(1)))

#' The drug as built for one regimen's absorption
#'
#' An intravenous regimen uses the drug as it is; an extravascular one, the
#' absorption derived for that regimen (each formulation its own).
#'
#' @param drug a paperReviewDrug() result carrying its \code{absorptions}
#' @param extraction the extraction
#' @param regimen the regimen
#' @param patient the patient
#' @returns the drug, or NULL when the regimen's absorption is not known
#' @keywords internal
paperReviewDrugFor <- function(drug, extraction, regimen, patient)
{
  if (!paperReviewNeedsAbsorption(regimen)) return(drug)
  a <- drug$absorptions[[regimen$id %or% ""]] %or% drug$absorptions[["route"]]
  if (is.null(a) || !isTRUE(a$ok)) return(NULL)
  if (identical(drug$absorption, a)) return(drug)
  paperReviewDrug(extraction, patient, drug$closed, absorption = a, absorptions = drug$absorptions)
}

#' The drug's display name, never a library drug's name
#' @keywords internal
paperReviewDrugName <- function(extraction)
{
  name <- extraction$drug$name %or% "drug"
  paste0(name, " (paper)")
}

#' Run the paper's drug through getDrugPK() and simCpCe()
#'
#' Quiet: stanpumpR's console comments are switched off while it runs, and
#' warnings are collected rather than printed, so that nothing from the
#' manuscript reaches a server log.
#'
#' @param drug the output of \code{paperReviewDrug()}
#' @param patient a list of weight, height, age, sex
#' @param doses a dose table: Time (minutes), Dose, Units
#' @param maximum the end of the simulation, minutes
#' @returns a list: \code{pk} (getDrugPK's result), \code{sim} (simCpCe's),
#'   \code{curve} (a data frame of Time, Cp, Ce), \code{warnings}
#' @keywords internal
paperReviewSimulate <- function(drug, patient, doses, maximum)
{
  paperReviewQuietly(function() {
    pk <- getDrugPK(drug$defaults$Drug, patient$weight, patient$height,
                    patient$age, patient$sex, drug$defaults, model = drug$model)
    dose <- data.frame(Drug = drug$defaults$Drug, Time = as.numeric(doses$Time),
                       Dose = as.numeric(doses$Dose), Units = as.character(doses$Units),
                       stringsAsFactors = FALSE)
    events <- data.frame(Time = numeric(0), Event = character(0))
    sim <- simCpCe(dose, events, pk, maximum, FALSE)
    plasma <- sim$results[sim$results$Site == "Plasma", c("Time", "Y")]
    effect <- sim$results[sim$results$Site == "Effect Site", c("Time", "Y")]
    curve <- data.frame(Time = plasma$Time, Cp = plasma$Y)
    curve$Ce <- if (nrow(effect) == nrow(plasma)) effect$Y else NA_real_
    list(pk = pk, sim = sim, curve = curve)
  })
}

#' Run a function with stanpumpR's console output off and warnings captured
#'
#' @returns the function's value, with \code{warnings} appended when it is a
#'   list
#' @keywords internal
paperReviewQuietly <- function(f)
{
  old <- options(ECHO_OUTPUT_COMMENTS = NA)
  on.exit(options(old), add = TRUE)
  warnings <- character(0)
  value <- withCallingHandlers(
    f(),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) invokeRestart("muffleMessage"))
  if (is.list(value)) value$warnings <- warnings
  value
}

#' A paper regimen as a stanpumpR dose table
#'
#' @param regimen one entry of \code{extraction$regimens}
#' @param route the drug's extravascular route, for an extravascular dose
#' @returns a list: \code{doses} (Time in minutes, Dose, Units),
#'   \code{problems}
#' @keywords internal
paperReviewRegimenDoses <- function(regimen)
{
  rows <- list()
  problems <- character(0)
  add <- function(time, dose, unit)
    rows[[length(rows) + 1]] <<- data.frame(Time = time, Dose = dose, Units = unit,
                                            stringsAsFactors = FALSE)
  for (d in regimen$doses %or% list())
  {
    minutes <- paperReviewMinutes(d$time_units %or% "min")
    mass <- d$mass_unit %or% "other"
    if (!mass %in% c("g", "mg", "mcg", "ng") || is.na(minutes)) {
      problems <- c(problems, "A dose's units could not be converted.")
      next
    }
    perKg <- if (identical(d$per_kg, "yes")) "/kg" else ""
    t0 <- d$time * minutes
    route <- d$route %or% "other"
    extravascular <- route %in% names(PAPER_REVIEW_ROUTE_SUFFIX)
    if (!extravascular && !route %in% c("iv_bolus", "iv_infusion")) {
      problems <- c(problems, "A dose's route is not one the simulator offers.")
      next
    }
    suffix <- if (extravascular) paste0(" ", PAPER_REVIEW_ROUTE_SUFFIX[[route]]) else ""
    duration <- paperReviewNum(d$duration) * minutes
    interval <- paperReviewNum(d$interval) * minutes
    perTime <- d$per_time %or% "none"
    if (paperReviewIsRateDose(d)) {
      # A constant input: an infusion, or a daily oral dose the paper models as
      # one.  A rate on an extravascular route is a
      # constant input on the apparent parameters ("mg/day PO", R/routes.R).
      rateUnit <- switch(perTime, per_min = "/min", per_h = "/hr", per_day = "/day", "/min")
      amount <- d$amount
      if (perTime == "none") {
        # A total amount given over a duration: as a rate per minute.
        if (!is.finite(duration) || duration <= 0) {
          problems <- c(problems, "An infusion has no duration.")
          next
        }
        amount <- d$amount / duration
      }
      unit <- paste0(mass, perKg, rateUnit, suffix)
      add(t0, amount, unit)
      if (is.finite(duration) && duration > 0) add(t0 + duration, 0, unit)
    } else {
      unit <- paste0(mass, perKg, suffix)
      times <- t0
      if (is.finite(interval) && interval > 0) {
        # A dose repeated at an interval, for its duration (or once if none).
        n <- if (is.finite(duration) && duration > 0) ceiling(duration / interval - 1e-9) else 1
        if (n > PAPER_REVIEW_MAX_REPEATS) {
          problems <- c(problems, paste("A repeated dose was cut to", PAPER_REVIEW_MAX_REPEATS, "doses."))
          n <- PAPER_REVIEW_MAX_REPEATS
        }
        times <- t0 + interval * (seq_len(n) - 1)
      }
      for (t in times) add(t, d$amount, unit)
    }
  }
  doses <- if (length(rows)) do.call(rbind, rows) else
    data.frame(Time = numeric(0), Dose = numeric(0), Units = character(0))
  doses <- doses[order(doses$Time), , drop = FALSE]
  rownames(doses) <- NULL
  list(doses = doses, problems = unique(problems))
}
