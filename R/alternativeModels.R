# -----------------------------------------------------------------------------
# Alternative population models: Schnider's propofol and Minto's remifentanil
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code (Claude Fable 5.1), 2026-10-12, at the request of
# Steven L. Shafer.  Verified by tests/testthat/test-minto-schnider.R.
#
# Thomas Schnider and Charles Minto developed the first population models of
# propofol (Schnider 1998) and remifentanil (Minto 1997), and those two models
# are the ones most target-controlled infusion devices run today.  Their
# Stanford data were uploaded to the Open TCI initiative, with other
# investigators' studies, and Douglas Eleveld built from that pool the
# general-purpose models that extend to children and to the extremes of
# weight; those are the library's own propofol and remifentanil models
# (R/drugs_propofol.R, R/drugs_remifentanil.R).  See inst/help/history.md.
#
# For teaching it helps to see both.  The "Minto / Schnider" box under Graph
# Options (input$mintoSchnider) runs the Schnider and Minto models BESIDE the
# library's models whenever propofol or remifentanil is in the dose table:
#
#   * in ordinary simulation the alternative model's plasma and effect-site
#     concentrations are drawn as a second, distinct line in the drug's own
#     panel (propofol: Schnider; remifentanil: Minto);
#
#   * under target-controlled infusion (a "Plasma target" or "Effect site
#     target" row) the alternative model drives a TCI pump of its own, so its
#     infusion rates appear as a second line in the drug's TCI rate panel, and
#     the concentrations it predicts are the second line in the concentration
#     panel;
#
#   * the time until threshold is never computed for the alternative model: the
#     black line stays on the library's (Eleveld) kinetics.
#
# How it is built
# ---------------
# An alternative model is a COMPANION ENTRY in the drug list: recalculatePK()
# builds it through getDrugPK(model = ...) from the model function below, names
# it "<drug> (<label>)" ("propofol (Schnider)") and marks it with $companionOf
# (the parent drug) and $modelLabel.  processdoseTable() simulates it on the
# parent's dose rows, caching it like any other drug, with the recovery switch
# off.  simulationPlot() draws it in the parent's panel in a colour derived from
# the parent's (alternativeModelColor()), with its own legend entry, and puts
# its TCI rates on the parent's rate panel.  The companion is kept out of
# everything that aggregates over drugs -- the MEAC sum, the interaction panel,
# the opioid-MAC interaction -- so a second remifentanil line is never counted
# as a second opioid.
#
# The models
# ----------
# Schnider's propofol model is written exactly as STANPUMP ran it (DRUGS.C,
# "Schnider - no opioids"), with the time to peak effect of 1.6 min from
# Schnider 1999, so that ke0 is solved for the patient as every other drug's
# is.  Minto's remifentanil model is written exactly as STANPUMP ran it
# ("Minto/Schnider"), including its ke0 as a published function of age,
# 0.595 - 0.007 (age - 40) per minute, which is what the pumps implement;
# getDrugPK() then solves the time to peak effect from that ke0.
#
# Both use the James (1976) lean body mass, as the papers, STANPUMP and the
# pumps do (lbmJames()), and that is deliberate.  The library's own models
# use the Al-Sallami fat-free mass; these two are here to show what the pumps
# compute, and part of what they teach is why the pumps should run the
# Eleveld models instead.  James's lean body mass is a parabola in weight: it
# peaks at a BMI of 43 in men and 36 in women and then falls (above about
# 125 kg in a man of 170 cm, 92 kg in a woman of 160 cm), so the Schnider
# clearance, which falls with lean body mass, rises steeply in the obese,
# and far enough out the Minto volumes go negative; in a small child the
# Schnider clearance intercepts make CL1 negative.  Where a volume, clearance
# or ke0 comes out zero or negative the model is not defined for that
# patient, no companion is built, and the app says so
# (alternativeModelProblems()).  The same limit is why the pumps restrict
# these models to a BMI below 42 in men and 35 in women.  Scenario
# `james-lean-body-mass`.
# -----------------------------------------------------------------------------

#' The alternative models, keyed by the library drug each is a second model of
#'
#' `label` names the model in the legend and the hover ("propofol (Schnider)"),
#' `model` is the model function, with the signature every drug model has.
#' @noRd
ALTERNATIVE_MODELS <- list(
  propofol     = list(label = "Schnider", model = "propofolSchnider"),
  remifentanil = list(label = "Minto",    model = "remifentanilMinto")
)
ALTERNATIVE_MODEL_DRUGS <- names(ALTERNATIVE_MODELS)

# The checkbox under Graph Options, and the help page about the models
ALTERNATIVE_MODELS_LABEL     <- "Minto / Schnider"
ALTERNATIVE_MODELS_HELP_PAGE <- "models/minto-schnider"

#' The name of a drug's companion entry: "propofol (Schnider)"
#' @noRd
alternativeModelName <- function(drug) {
  vapply(drug, function(d) {
    if (!d %in% ALTERNATIVE_MODEL_DRUGS) return(NA_character_)
    paste0(d, " (", ALTERNATIVE_MODELS[[d]]$label, ")")
  }, character(1), USE.NAMES = FALSE)
}

#' The parent drug behind a companion's name, with or without the " TCI"
#' suffix the rate series carry; any other name is returned as it is
#'
#' The plot's hover and click look a panel up by the first series drawn in it,
#' and a dose is always added to, and a rate always read from, the library's
#' own drug.
#' @noRd
alternativeModelParent <- function(name) {
  for (d in ALTERNATIVE_MODEL_DRUGS) {
    companion <- alternativeModelName(d)
    name[name == companion] <- d
    name[name == paste(companion, "TCI")] <- paste(d, "TCI")
  }
  name
}

#' Is this entry of the drug list a companion (alternative-model) entry?
#' @noRd
isAlternativeModelEntry <- function(entry) {
  !is.null(entry$companionOf)
}

#' The companion's colour, from its parent's
#'
#' A light colour is darkened and a dark one lightened, by mixing with black
#' or white, so that the second line is plainly a different line while it
#' still reads as the same drug's.  Propofol's yellow becomes an olive, and
#' remifentanil's dark blue a periwinkle.
#' @noRd
alternativeModelColor <- function(color, amount = 0.45) {
  rgb <- grDevices::col2rgb(color)[, 1] / 255
  luminance <- sum(rgb * c(0.299, 0.587, 0.114))
  towards <- if (luminance > 0.5) 0 else 1
  mixed <- rgb * (1 - amount) + towards * amount
  grDevices::rgb(mixed[1], mixed[2], mixed[3])
}

#' Schnider's propofol model (Schnider 1998), as STANPUMP ran it
#'
#' Schnider TW, Minto CF, Gambus PL, Andresen C, Goodale DB, Shafer SL,
#' Youngs EJ.  The influence of method of administration and covariates on the
#' pharmacokinetics of propofol in adult volunteers.  Anesthesiology
#' 1998;88:1170-1182.  Volunteers aged 25 to 81 years, 44 to 123 kg, 155 to
#' 196 cm.  The time to peak effect of 1.6 min is from Schnider TW et al.,
#' Anesthesiology 1999;90:1502-1516, as for the library's own propofol, and
#' as the STANPUMP propofol file set it; it gives a ke0 close to the 0.456/min
#' that paper reported for its typical patient.
#'
#' `adjustToFFM` is accepted for the uniform signature and ignored: the model
#' carries its own lean-body-mass covariate, James's, kept deliberately (see
#' above).  The same arithmetic is written out, and then overwritten, at the top of R/drugs_propofol.R.
#' @noRd
propofolSchnider <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  lbm <- lbmJames(weight, height, sex)
  default <- list(
    v1  = 4.27,
    v2  = 18.9 - 0.391 * (age - 53),
    v3  = 238,
    cl1 = 1.89 + 0.0456 * (weight - 77) - 0.0681 * (lbm - 59) + 0.0264 * (height - 177),
    cl2 = 1.29 - 0.024 * (age - 53),
    cl3 = 0.836
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))
  list(
    PK = PK,
    tPeak = 1.6,
    reference = "Schnider TW et al., Anesthesiology 1998;88(5):1170-1182. https://pubmed.ncbi.nlm.nih.gov/9605675/"
  )
}

#' Minto's remifentanil model (Minto 1997), as STANPUMP ran it
#'
#' Minto CF, Schnider TW, Egan TD, Youngs E, Lemmens HJM, Gambus PL, Billard V,
#' Hoke JF, Moore KHP, Hermann DJ, Muir KT, Mandema JW, Shafer SL.  Influence
#' of age and gender on the pharmacokinetics and pharmacodynamics of
#' remifentanil.  I. Model development.  Anesthesiology 1997;86:10-23.
#' Volunteers aged 20 to 85 years.  ke0 is the paper's own, a function of
#' age, which is what the pumps implement and what STANPUMP set (DRUGS.C,
#' "k41 = 0.595-0.007*(age-40)"); no tPeak is given, so getDrugPK() solves the
#' time to peak effect from ke0 and the disposition for the patient.
#'
#' `adjustToFFM` is accepted for the uniform signature and ignored: the model
#' carries its own lean-body-mass covariate, James's, kept deliberately (see
#' above).  The same arithmetic is in the comments at the top of R/drugs_remifentanil.R.
#'
#' `pregnant` applies the same factor as the library's remifentanil model:
#' clearance x REMIFENTANIL_PREGNANCY_CL_FACTOR (Ansari 2016, "clearance more
#' than doubles"; see R/drugs_remifentanil.R), so that the two lines of the
#' remifentanil panel move together for a parturient.
#' @noRd
remifentanilMinto <- function(weight, height, age, sex, adjustToFFM = TRUE,
                              pregnant = FALSE)
{
  lbm <- lbmJames(weight, height, sex)
  clPregnancy <- if (isTRUE(pregnant)) REMIFENTANIL_PREGNANCY_CL_FACTOR else 1
  default <- list(
    v1  = 5.1  - 0.0201  * (age - 40) + 0.072 * (lbm - 55),
    v2  = 9.82 - 0.0811  * (age - 40) + 0.108 * (lbm - 55),
    v3  = 5.42,
    cl1 = (2.6  - 0.0162  * (age - 40) + 0.0191 * (lbm - 55)) * clPregnancy,
    cl2 = 2.05 - 0.0301  * (age - 40),
    cl3 = 0.076 - 0.00113 * (age - 40)
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))
  reference <- "Minto CF et al., Anesthesiology 1997;86(1):10-23. https://pubmed.ncbi.nlm.nih.gov/9009935/"
  if (isTRUE(pregnant)) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("clearance x ", REMIFENTANIL_PREGNANCY_CL_FACTOR)))
  }
  list(
    PK = PK,
    ke0 = 0.595 - 0.007 * (age - 40),
    reference = reference
  )
}

#' Run an alternative model's function for a patient
#'
#' @param pregnant the pregnancy flag, passed on when the model names it
#'   (R/pregnancy.R)
#' @returns the model function's output (PK sets, tPeak or ke0, reference)
#' @noRd
alternativeModelOutput <- function(drug, weight, height, age, sex, pregnant = FALSE) {
  spec <- ALTERNATIVE_MODELS[[drug]]
  if (is.null(spec)) stop("No alternative model for ", drug)
  fn <- get(spec$model, mode = "function")
  args <- list(weight = weight, height = height, age = age, sex = sex)
  if ("pregnant" %in% names(formals(fn))) args$pregnant <- isTRUE(pregnant)
  do.call(fn, args)
}

#' Why an alternative model is not defined for a patient, or NULL
#'
#' The models' linear covariate terms can give a volume or clearance that is
#' zero or negative outside the population they were fitted to (see the
#' header).  Such a model cannot be simulated, so no companion is built for it
#' and the app shows this message instead.
#'
#' @param drug a drug in `ALTERNATIVE_MODEL_DRUGS`
#' @returns NULL when the model is defined, else one sentence
#' @noRd
alternativeModelProblem <- function(drug, weight, height, age, sex, pregnant = FALSE) {
  spec <- ALTERNATIVE_MODELS[[drug]]
  if (is.null(spec)) return(NULL)
  X <- tryCatch(alternativeModelOutput(drug, weight, height, age, sex, pregnant),
                error = function(e) NULL)
  if (is.null(X)) return(sprintf("%s's %s model could not be evaluated for this patient.", spec$label, drug))
  set <- X$PK[[PK_EVENT_DEFAULT]]
  values <- c(V1 = set$v1, V2 = set$v2, V3 = set$v3, CL1 = set$cl1, CL2 = set$cl2, CL3 = set$cl3)
  if (!is.null(X$ke0)) values <- c(values, ke0 = X$ke0)
  bad <- names(values)[!is.finite(values) | values <= 0]
  if (length(bad) == 0) return(NULL)
  listed <- if (length(bad) == 1) bad else
    paste(paste(bad[-length(bad)], collapse = ", "), "and", bad[length(bad)])
  sprintf(
    "%s's %s model is not defined for this patient: its %s %s not positive (%s). Only the library's model is shown.",
    spec$label, drug, listed, if (length(bad) == 1) "is" else "are",
    paste(sprintf("%s = %s", bad, signif(values[bad], 3)), collapse = ", ")
  )
}

#' The problems with the alternative models of the drugs dosed, as messages
#'
#' @param drugs the drugs in the dose table (any names; those without an
#'   alternative model are ignored)
#' @returns a character vector, empty when every alternative model is defined
#' @noRd
alternativeModelProblems <- function(drugs, weight, height, age, sex, pregnant = FALSE) {
  out <- character(0)
  for (drug in intersect(unique(drugs), ALTERNATIVE_MODEL_DRUGS)) {
    problem <- alternativeModelProblem(drug, weight, height, age, sex, pregnant)
    if (!is.null(problem)) out <- c(out, problem)
  }
  out
}

#' The companion entry of a drug for a patient, or NULL when the model is not
#' defined for them
#'
#' The entry is what getDrugPK() returns for the parent drug, built from the
#' alternative model's output instead of the drug's own function, so it has
#' every field the pipeline expects of a drug (units, typical band, MEAC,
#' threshold).  It is then named for the model and marked as a companion.
#'
#' @param drug a drug in `ALTERNATIVE_MODEL_DRUGS`
#' @param drugDefaults that drug's row of the drug library
#' @returns the entry, as recalculatePK() stores it, or NULL
#' @noRd
alternativeModelEntry <- function(drug, weight, height, age, sex, drugDefaults,
                                  adjustToFFM = TRUE, pregnant = FALSE) {
  if (!is.null(alternativeModelProblem(drug, weight, height, age, sex, pregnant))) return(NULL)
  spec <- ALTERNATIVE_MODELS[[drug]]
  model <- alternativeModelOutput(drug, weight, height, age, sex, pregnant)
  entry <- getDrugPK(
    drug = drug,
    weight = weight, height = height, age = age, sex = sex,
    drugDefaults = drugDefaults,
    adjustToFFM = adjustToFFM,
    pregnant = pregnant,
    model = model
  )
  entry$drug        <- alternativeModelName(drug)
  entry$companionOf <- drug
  entry$modelLabel  <- spec$label
  entry$Color       <- alternativeModelColor(drugDefaults$Color)
  entry
}
