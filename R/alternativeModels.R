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
# and far enough out Minto's V1 goes negative; in a small child the
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
#' `model` is the model function (with the signature every drug model has), and
#' `group` is the Graph Options switch that turns it on (a key of
#' `COMPANION_MODEL_GROUPS`).  The Minto / Schnider box runs the Schnider and
#' Minto models beside the library's; the Hamberg box runs the Hamberg genotype
#' model of warfarin's INR beside the library's (Xue) model.
#' @noRd
ALTERNATIVE_MODELS <- list(
  propofol     = list(label = "Schnider", model = "propofolSchnider", group = "mintoSchnider"),
  remifentanil = list(label = "Minto",    model = "remifentanilMinto", group = "mintoSchnider"),
  warfarin     = list(label = "Hamberg",  model = "warfarinHamberg",   group = "hamberg")
)
ALTERNATIVE_MODEL_DRUGS <- names(ALTERNATIVE_MODELS)

#' The Graph Options switches that turn companion models on
#'
#' Each is a checkbox (`input`), a label for the box, the legend and the export,
#' and the help page a companion's "About this model" link opens.  A group is
#' usable only while one of its drugs is in the dose table.
#' @noRd
COMPANION_MODEL_GROUPS <- list(
  mintoSchnider = list(input = "mintoSchnider", label = "Minto / Schnider",
                       helpPage = "models/minto-schnider"),
  hamberg       = list(input = "hamberg",       label = "Hamberg",
                       helpPage = "models/hamberg")
)

# Kept for the Minto / Schnider box and its scenarios, which name them directly.
ALTERNATIVE_MODELS_LABEL     <- COMPANION_MODEL_GROUPS$mintoSchnider$label
ALTERNATIVE_MODELS_HELP_PAGE <- COMPANION_MODEL_GROUPS$mintoSchnider$helpPage

#' The companion group a drug's alternative model belongs to, or NA
#' @noRd
companionModelGroup <- function(drug) {
  vapply(drug, function(d) {
    spec <- ALTERNATIVE_MODELS[[d]]
    if (is.null(spec)) NA_character_ else spec$group
  }, character(1), USE.NAMES = FALSE)
}

#' The drugs whose alternative model is turned on by a given group
#' @noRd
companionGroupDrugs <- function(group) {
  names(Filter(function(spec) identical(spec$group, group), ALTERNATIVE_MODELS))
}

#' The help page a companion entry's "About this model" link opens
#' @noRd
companionHelpPage <- function(entry) {
  group <- companionModelGroup(entry$companionOf)
  if (is.na(group)) ALTERNATIVE_MODELS_HELP_PAGE else COMPANION_MODEL_GROUPS[[group]]$helpPage
}

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

#' Hamberg's warfarin genotype model (Hamberg 2007), beside the library's Xue
#'
#' Hamberg A-K, Wadelius M, Lindh JD, Dahl M-L, Padrini R, Deloukas P, Rane A,
#' Jonsson EN.  A pharmacometric model describing the relationship between
#' warfarin dose and INR response with respect to variations in CYP2C9, VKORC1,
#' and age.  Clin Pharmacol Ther 2007;81:529-538.  150 patients (Tables 2-4,
#' Figure 2, Appendix equations 8-10), read from the full text on 2026-10-17.
#'
#' A companion of warfarin that draws a second INR line beside the library's
#' Xue 2017 model (R/drugs_warfarin.R, R/inr.R).  S- and R-warfarin are fitted
#' separately; the INR is driven by S-warfarin only, through two parallel
#' transit-compartment chains (the "transit" inr block, integrated by
#' inrTransit() in R/inr.R).  A racemic dose of D mg adds D/2 mg to each
#' enantiomer's absorption depot (F = 1).  Units: time in minutes, volumes in
#' litres, clearances in L/min (apparent, /F; the model has no body-size
#' covariate), concentrations in mg/L, and the plotted curve is the INR.
#'
#'   S-warfarin, two-compartment (Table 2):
#'     CL_S = 0.314 L/h x (1 - 0.0091 (age - 71)) x genotype factor
#'     V1 = 13.8 L   Q = 0.131 L/h   V2 = 6.59 L   Ka = 2 /h (fixed)
#'   genotype factor = 1 - the per-diplotype % reduction on CL_S:
#'     *1/*1 0, *1/*2 31.5%, *1/*3 45.3%, *2/*2 72.2%, *2/*3 69.0%, *3/*3 85.2%
#'   R-warfarin, one-compartment (Table 3):
#'     CL_R = 0.139 L/h x (1 - 0.0098 (age - 71))   V = 12.8 L   Ka = 2 /h
#'   PD (Table 4, Appendix eq 8-10):
#'     Emax = 1, gamma = 0.424, EC50 by VKORC1 (GG 4.61, GA 3.02, AA 2.20 mg/L),
#'     a six-compartment chain with MTT1 = 11.6 h and a one-compartment chain
#'     with MTT2 = 120 h (ktr = 1/MTT), INR = BASE + 20 (1 - A6 A7)^3.61, with
#'     INR_MAX = 20 fixed, lambda = 3.61, and BASE the drug-free baseline INR,
#'     1 for a typical patient.
#'
#' `adjustToFFM` is accepted for the uniform signature and ignored: the model
#' has no body-size covariate (sex and weight were not significant).
#' @noRd
warfarinHamberg <- function(weight, height, age, sex, adjustToFFM = TRUE,
                            cyp2c9 = CYP2C9_DEFAULT, vkorc1 = VKORC1_DEFAULT)
{
  if (length(cyp2c9) != 1 || !cyp2c9 %in% CYP2C9_VALUES)
    stop("Invalid cyp2c9: ", paste(cyp2c9, collapse = ", "), ". Must be one of: ",
         paste(CYP2C9_VALUES, collapse = ", "))
  if (length(vkorc1) != 1 || !vkorc1 %in% VKORC1_VALUES)
    stop("Invalid vkorc1: ", paste(vkorc1, collapse = ", "), ". Must be one of: ",
         paste(VKORC1_VALUES, collapse = ", "))

  # CYP2C9 % reduction on CL_S (Table 2); *1/*1 is the reference, factor 1.
  reduction <- c("*1/*1" = 0, "*1/*2" = 0.315, "*1/*3" = 0.453,
                 "*2/*2" = 0.722, "*2/*3" = 0.690, "*3/*3" = 0.852)
  clS <- 0.314 * (1 - 0.0091 * (age - 71)) * (1 - reduction[[cyp2c9]])   # L/h
  clR <- 0.139 * (1 - 0.0098 * (age - 71))                              # L/h
  ka  <- 2 / 60                                                         # 1/min

  sSet <- list(
    v1 = 13.8, v2 = 6.59, v3 = 1,
    cl1 = clS / 60, cl2 = 0.131 / 60, cl3 = 0,
    ka_PO = ka, tlag_PO = 0, bioavailability_PO = 1
  )
  rSet <- list(
    v1 = 12.8, v2 = 1, v3 = 1,
    cl1 = clR / 60, cl2 = 0, cl3 = 0,
    ka_PO = ka, tlag_PO = 0, bioavailability_PO = 1
  )

  ec50 <- c("GG" = 4.61, "GA" = 3.02, "AA" = 2.20)[[vkorc1]]            # mg/L

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(sSet))

  list(
    PK = PK,
    doseFraction = 0.5,                                   # S gets half the dose
    parallelSystems = list(
      list(name = "R-warfarin", doseFraction = 0.5,
           PK = list(default = rSet))
    ),
    inr = list(type = "transit", c50 = ec50, gamma = 0.424,
               mtt1 = 11.6 * 60, mtt2 = 120 * 60, n1 = 6L,
               baseline = 1, inrMax = 20, lambda = 3.61),
    tPeak = 0,
    reference = paste0(
      "Hamberg A-K et al., Clin Pharmacol Ther 2007;81:529-538 (S- and ",
      "R-warfarin PK and a two-chain transit-compartment INR model with ",
      "CYP2C9, VKORC1 and age; 150 patients). https://doi.org/10.1038/sj.clpt.6100084"
    )
  )
}

#' Run an alternative model's function for a patient
#'
#' @param pregnant the pregnancy flag, passed on when the model names it
#'   (R/pregnancy.R)
#' @param cyp2c9,vkorc1 the pharmacogenetic covariates, passed on when the model
#'   names them (warfarin's Hamberg companion reads both)
#' @returns the model function's output (PK sets, tPeak or ke0, reference)
#' @noRd
alternativeModelOutput <- function(drug, weight, height, age, sex, pregnant = FALSE,
                                   cyp2c9 = CYP2C9_DEFAULT, vkorc1 = VKORC1_DEFAULT) {
  spec <- ALTERNATIVE_MODELS[[drug]]
  if (is.null(spec)) stop("No alternative model for ", drug)
  fn <- get(spec$model, mode = "function")
  args <- list(weight = weight, height = height, age = age, sex = sex)
  if ("pregnant" %in% names(formals(fn))) args$pregnant <- isTRUE(pregnant)
  if ("cyp2c9"  %in% names(formals(fn))) args$cyp2c9  <- cyp2c9
  if ("vkorc1"  %in% names(formals(fn))) args$vkorc1  <- vkorc1
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
alternativeModelProblem <- function(drug, weight, height, age, sex, pregnant = FALSE,
                                    cyp2c9 = CYP2C9_DEFAULT, vkorc1 = VKORC1_DEFAULT) {
  spec <- ALTERNATIVE_MODELS[[drug]]
  if (is.null(spec)) return(NULL)
  X <- tryCatch(alternativeModelOutput(drug, weight, height, age, sex, pregnant,
                                       cyp2c9 = cyp2c9, vkorc1 = vkorc1),
                error = function(e) NULL)
  if (is.null(X)) return(sprintf("%s's %s model could not be evaluated for this patient.", spec$label, drug))
  set <- X$PK[[PK_EVENT_DEFAULT]]
  values <- c(V1 = set$v1, V2 = set$v2, V3 = set$v3, CL1 = set$cl1, CL2 = set$cl2, CL3 = set$cl3)
  if (!is.null(X$ke0)) values <- c(values, ke0 = X$ke0)
  # The central volume, the elimination clearance and ke0 must be positive; the
  # peripheral volumes and intercompartmental clearances only need to be
  # non-negative, so a one- or two-compartment companion (Hamberg's warfarin)
  # with no third compartment is not flagged, while a covariate term that turns
  # one of them negative (Minto's CL3 at a high age) still is.  The order of the
  # names is kept for the message.
  strict <- names(values) %in% c("V1", "CL1", "ke0")
  bad <- names(values)[!is.finite(values) |
                         (strict & values <= 0) | (!strict & values < 0)]
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
alternativeModelProblems <- function(drugs, weight, height, age, sex, pregnant = FALSE,
                                     cyp2c9 = CYP2C9_DEFAULT, vkorc1 = VKORC1_DEFAULT) {
  out <- character(0)
  for (drug in intersect(unique(drugs), ALTERNATIVE_MODEL_DRUGS)) {
    problem <- alternativeModelProblem(drug, weight, height, age, sex, pregnant,
                                       cyp2c9 = cyp2c9, vkorc1 = vkorc1)
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
                                  adjustToFFM = TRUE, pregnant = FALSE,
                                  cyp2c9 = CYP2C9_DEFAULT, vkorc1 = VKORC1_DEFAULT) {
  if (!is.null(alternativeModelProblem(drug, weight, height, age, sex, pregnant,
                                       cyp2c9 = cyp2c9, vkorc1 = vkorc1))) return(NULL)
  spec <- ALTERNATIVE_MODELS[[drug]]
  model <- alternativeModelOutput(drug, weight, height, age, sex, pregnant,
                                  cyp2c9 = cyp2c9, vkorc1 = vkorc1)
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
