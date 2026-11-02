# -----------------------------------------------------------------------------
# Pregnancy: the Patient Profile's Pregnant field and what reads it
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Claude Code, 2026-10-13, at the request of Steven L. Shafer: apply the
# pharmacokinetic changes of pregnancy described in Ansari J, Carvalho B,
# Shafer SL, Flood P.  Pharmacokinetics and pharmacodynamics of drugs commonly
# used in pregnancy and parturition.  Anesth Analg 2016;122:786-804, to the
# drugs of that review that are in the library.
#
# Claude Code, 2026-10-19, at the same request extended to every drug: the
# literature was reviewed for the pharmacokinetics of pregnancy of each drug
# model in the library (PubMed abstracts and PubMed Central full text), and
# the findings are inst/extdata/pregnancyRegistry.csv, one row per drug,
# built by data-raw/pregnancyRegistry.R.  docs/pregnancy.md describes the
# review and lists the papers still to be obtained.
#
# HOW THE FLAG TRAVELS
# ====================
# `pregnant` is a single TRUE or FALSE.  Like the serum creatinine and the
# CYP phenotypes it reaches only the drug models that name it in their
# signature (getDrugPK()), so a drug with nothing to apply is not touched,
# and the library's existing simulations do not move while the flag is off.
# getDrugPK() refuses TRUE for a man.  The app sets it from the Pregnant
# control, which is shown only for a woman of PREGNANCY_MIN_AGE to
# PREGNANCY_MAX_AGE years (R/constants.R); pregnancyPossible() applies the
# same rule on the server, so a hidden control cannot leave the flag on.
#
# WHAT IS APPLIED, AND WHAT IS NOT
# ================================
# The flag describes LATE PREGNANCY (the third trimester and term), where
# most of the data were collected; where a source reports trimesters, the
# third-trimester value is the one applied.  A model applies a factor when
# the registry's group is "applied": a measured human ratio (a population
# model with a pregnancy covariate, a paired pregnant-against-postpartum
# study, a controlled comparison or a therapeutic-drug-monitoring series of
# useful size), or a change derived from one with documented arithmetic
# (midazolam, metoprolol, phenytoin, valproate), or, for a drug cleared
# mostly unchanged by filtration whose model reads renal function, the 50%
# rise in glomerular filtration (PREGNANCY_GFR_FACTOR).  Each model documents
# what it applies in a PREGNANCY section of its own file, with the source and
# the arithmetic, and test-pregnancy.R checks that exactly the "applied"
# drugs name `pregnant` and that the clearance and volume factors in the
# registry are what the models do.
#
# A change known only in direction, or measured too weakly (a handful of
# subjects, no control group, a PBPK prediction alone), is recorded and not
# applied; so is a drug reported as unchanged or never studied.  A model fitted
# in pregnant women (calcium, magnesium sulfate, oxytocin) already describes
# them, and the flag does nothing to it.
#
# THE RISE IN FILTRATION AND THE MEASURED CREATININE
# ==================================================
# Serum creatinine falls in pregnancy because filtration rises.  A model that
# reads the Patient Profile's creatinine through Cockcroft-Gault or eGFR
# therefore moves toward the rise, in direction, when a pregnant woman's
# measured creatinine is entered (a lower creatinine reads as higher
# filtration in any of these equations), so PREGNANCY_GFR_FACTOR is not
# applied on top of it -- multiplying again would count the direction twice.
# That is not the same claim as the equation being VALIDATED for maternal
# GFR: Cockcroft-Gault, CKD-EPI and MDRD were all fitted in nonpregnant
# adults, and plugging a pregnant woman's creatinine into one is an
# extrapolation, not a measurement of her actual filtration.  It is simply
# the better extrapolation available when her creatinine is known, against
# the alternative of assuming she has a nonpregnant woman's typical value.
# The rise is applied as PREGNANCY_GFR_FACTOR only when the creatinine is
# left blank, the case in which the model assumes a normal NONPREGNANT
# creatinine: pregnancyGfrFactor().
#
# NOT MODELLED
# ============
# The fall in albumin and the higher free fraction of highly bound drugs
# except where the plotted total concentration is changed by it (phenytoin,
# valproate), placental transfer, the fetus, labour's effect on gastric
# emptying, the return of each change after delivery, and the trimesters
# before the third.  The fall in MAC of the volatile anesthetics is a
# pharmacodynamic change and is recorded, not applied.  The weight entered
# is the woman's actual weight; the fat-free-mass page says why the scaling
# may be turned off for her.
# -----------------------------------------------------------------------------

#' The review the library's pregnancy adjustments follow
#' @noRd
PREGNANCY_REFERENCE <- paste0(
  "Ansari J, Carvalho B, Shafer SL, Flood P. Anesth Analg 2016;122:786-804. ",
  "https://doi.org/10.1213/ANE.0000000000001143"
)

#' A citation of the pregnancy review for a drug's `reference` string
#'
#' Appended by a model to its own citation when the flag is on, so that the
#' References panel names the source of the adjustment only when one is
#' applied.
#'
#' @param applied what the model changed, e.g. "clearance x 1.7"
#' @param source the citation the factor is from: the review by default, or
#'   the drug's own pregnancy study
#' @returns one string
#' @noRd
pregnancyReference <- function(applied, source = PREGNANCY_REFERENCE) {
  paste0("pregnancy: ", applied, " (", source, ")")
}

#' Can the patient be pregnant?
#'
#' The rule the Patient Profile uses to offer the Pregnant control: a woman
#' from `PREGNANCY_MIN_AGE` up to, but not including, `PREGNANCY_MAX_AGE`
#' years.  The server applies it to the control's value, so a control hidden
#' by a change of age or sex leaves the flag off.
#'
#' @param age age in years
#' @param sex "male" or "female"
#' @returns TRUE or FALSE
#' @noRd
pregnancyPossible <- function(age, sex) {
  identical(sex, SEX_FEMALE) && is_valid_number(age) &&
    age >= PREGNANCY_MIN_AGE && age < PREGNANCY_MAX_AGE
}

#' Validate the pregnancy flag
#'
#' @param pregnant TRUE, FALSE or NULL (read as FALSE: the control has not
#'   reported, or a caller that predates the field)
#' @param sex the patient's sex, already validated
#' @returns TRUE or FALSE
#' @noRd
checkPregnant <- function(pregnant, sex) {
  if (is.null(pregnant)) return(FALSE)
  if (!is.logical(pregnant) || length(pregnant) != 1 || is.na(pregnant)) {
    stop("Invalid pregnant: ", paste(pregnant, collapse = ", "),
         ". Must be TRUE or FALSE")
  }
  if (pregnant && !identical(sex, SEX_FEMALE)) {
    stop("pregnant = TRUE requires sex = \"", SEX_FEMALE, "\"")
  }
  pregnant
}

#' The pregnancy registry
#'
#' One row per drug of the library (every drug with a Category in
#' drugDefaults_global.csv): what is known of its pharmacokinetics in
#' pregnancy, what the model applies, the sources and the papers still to be
#' obtained.  Built by data-raw/pregnancyRegistry.R.
#'
#' @returns a data frame read from \code{inst/extdata/pregnancyRegistry.csv}:
#'   `drug`, `category`, `group` (one of PREGNANCY_GROUPS), `finding`,
#'   `applied` ("" unless the group is "applied"), the factors `cl_factor`,
#'   `v_factor`, `f_factor` and `gfr_factor` (NA where none), `trimester`,
#'   `comparator`, `design`, `n`, `confidence`, `evidence_read`, `source`,
#'   `source_url`, `to_obtain` and `notes`
#' @keywords internal
pregnancyRegistry <- function()
  utils::read.csv(
    system.file("extdata", "pregnancyRegistry.csv", package = "stanpumpR"),
    stringsAsFactors = FALSE, na.strings = "NA", encoding = "UTF-8")

#' The registry's groups, in the order the help tabulates them
#' @noRd
PREGNANCY_GROUPS <- c("applied", "unquantified", "unchanged", "unknown",
                      "source_pregnant", "not_applicable")

#' What is known for each drug in pregnancy
#'
#' The registry (pregnancyRegistry()), with `NA` text read as "".  The help's
#' pregnancy page tabulates it beside the factors measured from the models
#' themselves (R/help-generated.R), each drug's page carries its row
#' (R/help-drugs.R), and test-pregnancy.R checks that exactly the drugs with
#' an `applied` entry name `pregnant` in their signature.
#'
#' @returns a data frame: one row per drug, the registry's columns
#' @noRd
pregnancyFindings <- function() {
  f <- pregnancyRegistry()
  text <- vapply(f, is.character, logical(1))
  f[text] <- lapply(f[text], function(x) ifelse(is.na(x), "", x))
  f
}

#' The drugs the registry must cover
#'
#' Every drug of the library a user can dose: the rows of the drug table with
#' a Category, which leaves out the metabolites (formed, never dosed) and the
#' carrier gases (air, oxygen, ventilation).
#' @noRd
pregnancyCoveredDrugs <- function(drugDefaults = getDrugDefaultsGlobal()) {
  category <- drugDefaults$Category
  drugDefaults$Drug[!is.na(category) & nzchar(category)]
}

#' The rise in filtration a renal term applies in pregnancy
#'
#' PREGNANCY_GFR_FACTOR when the patient is pregnant and the serum creatinine
#' is left blank, so that the model's assumed creatinine is the normal one for
#' a woman who is NOT pregnant; 1 otherwise.  A measured creatinine already
#' moves the renal-function estimate in the direction of the rise, since
#' creatinine falls as filtration rises, so the factor is not layered on top
#' of it -- but an adult renal equation run on that creatinine is still an
#' extrapolation to pregnancy, not a validated maternal GFR.
#'
#' @param pregnant the pregnancy flag
#' @param creatinine the serum creatinine as the model received it (NULL when
#'   the field is blank)
#' @returns one number
#' @noRd
pregnancyGfrFactor <- function(pregnant, creatinine) {
  if (isTRUE(pregnant) && is.null(creatinine)) PREGNANCY_GFR_FACTOR else 1
}

#' The drugs whose models read the pregnancy flag
#' @returns their names, in library order
#' @noRd
pregnancyDrugs <- function(drugDefaults = getDrugDefaultsGlobal()) {
  drugs <- drugDefaults$Drug[!isGasDrug(drugDefaults$Drug)]
  Filter(function(drug) {
    exists(drug, mode = "function") &&
      "pregnant" %in% names(formals(get(drug, mode = "function")))
  }, drugs)
}
