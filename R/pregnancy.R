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
# HOW THE FLAG TRAVELS
# ====================
# `pregnant` is a single TRUE or FALSE.  Like the serum creatinine and the
# CYP phenotypes it reaches only the drug models that name it in their
# signature (getDrugPK()), so a drug the review does not quantify is not
# touched, and the library's existing simulations do not move while the flag
# is off.  getDrugPK() refuses TRUE for a man.  The app sets it from the
# Pregnant control, which is shown only for a woman of PREGNANCY_MIN_AGE to
# PREGNANCY_MAX_AGE years (R/constants.R); pregnancyPossible() applies the
# same rule on the server, so a hidden control cannot leave the flag on.
#
# WHAT IS APPLIED, AND WHAT IS NOT
# ================================
# Eight drugs in the library are adjusted.  The review gives a figure for
# the first five; the three antibiotics take its general rise in glomerular
# filtration (below).  Each model documents what it applies in its own file,
# with the review's wording and the arithmetic:
#
#   morphine        clearance x 1.7 (Table 3, "clearance >70%", volume
#                   unchanged)
#   remifentanil    clearance x 2 ("clearance more than doubles"), on the
#                   Eleveld and Kim models and on the Minto companion
#   oxycodone       clearance x 3.8/2.6 (elimination half-time 3.8 to 2.6 h)
#   methadone       clearance x 24/14 (half-life 8-20 h against 24 h)
#   dexamethasone   clearance x 2 ("clearance doubled in single gestation")
#   cefazolin       creatinine clearance x 1.5 in its renal term
#   gentamicin      eGFR x 1.5 in its renal term
#   vancomycin      creatinine clearance x 1.5 in its renal term
#
# The three antibiotics carry the review's physiological statement, a 50%
# rise in glomerular filtration (PREGNANCY_GFR_FACTOR), through the renal
# function their published clearance equations read; the review says each of
# them clears faster in pregnancy but prints no factor.  The larger volume of
# distribution the review describes for them is unquantified and NOT applied,
# so their simulated peaks in a parturient are too high.
#
# A drug the review describes only in direction (buprenorphine, codeine,
# hydrocodone, midazolam, clindamycin, aspirin) is left unchanged, and the
# help's pregnancy page says what the review found.  So is every drug the
# review reports as unchanged or unknown.  pregnancyFindings() is that table,
# one row per drug of the review's tables that is in the library (drugs the
# review names only in its text, such as phenytoin, valproate, prednisone,
# naproxen and amiodarone, have no row), and a test keeps it in step
# with the models that name the flag.
#
# NOT MODELLED
# ============
# The fall in albumin and the higher free fraction of highly bound drugs
# (the review names midazolam, phenytoin and valproate), placental transfer,
# the fetus, labour's effect on gastric emptying, and the return of each
# change after delivery.  The weight entered is the woman's actual weight;
# the fat-free-mass page says why the scaling may be turned off for her.
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

#' What the pregnancy review found for each of its drugs in the library
#'
#' One row per drug of Ansari 2016's tables that is in the drug library, in
#' the order of those tables: what the review reports, and what the model
#' applies (nothing, for a change the review gives no magnitude for, or
#' reports as absent or unknown).  The help's pregnancy page tabulates it
#' beside the factors measured from the models themselves
#' (R/help-generated.R), and test-pregnancy.R checks that exactly the drugs
#' with an `applied` entry name `pregnant` in their signature.
#'
#' @returns a data frame: `drug`, `group` (one of "applied", "unquantified",
#'   "unchanged", "unknown"), `finding` (the review's statement) and
#'   `applied` (what the model does; "" when nothing)
#' @noRd
pregnancyFindings <- function() {
  row <- function(drug, group, finding, applied = "") {
    data.frame(drug = drug, group = group, finding = finding, applied = applied,
               stringsAsFactors = FALSE)
  }
  rbind(
    # --- Table 3, opioids ---------------------------------------------------
    row("alfentanil", "unchanged",
        "No change in volume of distribution or clearance."),
    row("buprenorphine", "unquantified",
        "Increased metabolism to inactive metabolites by induction of CYP3A4; a dose increase is needed in the third trimester. No magnitude is given."),
    row("codeine", "unquantified",
        "A prodrug: CYP2D6, which forms its morphine, is induced in pregnancy, so morphine forms faster and peaks higher, most of all in ultrarapid metabolisers. No magnitude is given."),
    row("fentanyl", "unknown", "Unknown."),
    row("hydrocodone", "unquantified",
        "Increased CYP2D6 activity increases conversion to hydromorphone. No magnitude is given."),
    row("hydromorphone", "unknown", "Unknown."),
    row("pethidine", "unchanged",
        "Increased inactivation by CYP3A4 is expected but is likely counterbalanced by decreased clearance; no change in half-life."),
    row("methadone", "applied",
        "Clearance increased by induction of CYP3A4 and CYP2B6; the elimination half-life is 8 to 20 hours against 24 hours when not pregnant, so a higher dose and a shorter interval are needed.",
        "Clearance of both enantiomers × 24/14 = 1.71, the 24-hour half-life over the midpoint of 8 to 20 hours; volumes unchanged."),
    row("morphine", "applied",
        "Volume of distribution unchanged; clearance increased by more than 70% (glucuronidation by UGT); half-life decreased.",
        "Elimination clearance × 1.7; volumes and distribution clearances unchanged."),
    row("oxycodone", "applied",
        "Clearance increased through the higher glomerular filtration rate and induction of CYP3A4 and CYP2D6; the elimination half-time in labouring women fell from 3.8 to 2.6 hours.",
        "Elimination clearance × 3.8/2.6 = 1.46; volumes and oxymorphone formation unchanged."),
    row("remifentanil", "applied",
        "Clearance more than doubles, attributed to the larger plasma volume, cardiac output and renal blood flow.",
        "Elimination clearance × 2, on the Eleveld and Kim models and on the Minto companion model."),
    # --- Table 4, sedative hypnotics ------------------------------------------
    row("diazepam", "unchanged", "No change in clearance."),
    row("etomidate", "unknown", "Unknown."),
    row("ketamine", "unknown", "No human data; unchanged in the pregnant ewe."),
    row("midazolam", "unquantified",
        "Peak plasma concentration reduced after oral and parenteral dosing with an unchanged half-life, from CYP3A-induced hepatic metabolism. No magnitude is given."),
    row("propofol", "unchanged",
        "The C50 for loss of consciousness is unchanged in the first trimester; the pharmacokinetics have not been well studied, and free propofol is expected to be higher as albumin falls."),
    # --- Table 5, muscle relaxants ------------------------------------------
    row("rocuronium", "unknown",
        "Pharmacokinetics unknown; onset unchanged, with a possibly prolonged clinical duration."),
    # --- Table 6, local anaesthetics ----------------------------------------
    row("bupivacaine", "unchanged",
        "No change in absorption or peak concentration; some change in metabolism (less 4'-hydroxylation, more N-dealkylation)."),
    row("lidocaine", "unknown", "Unknown."),
    row("mepivacaine", "unknown", "Unknown."),
    row("ropivacaine", "unknown", "Unknown."),
    # --- Table 7, antibiotics -----------------------------------------------
    row("cefazolin", "applied",
        "Clearance and volume of distribution increased, the clearance by increased renal excretion; a 2 g dose and a 3- to 6-hour interval are recommended.",
        "Creatinine clearance × 1.5, the review's rise in glomerular filtration, in the published renal term, so unbound clearance × 1.27; volumes unchanged."),
    row("ceftriaxone", "unchanged",
        "Not significantly altered (the table queries a longer half-life)."),
    row("clindamycin", "unquantified",
        "Decreased AUC/MIC ratio. No magnitude is given."),
    row("gentamicin", "applied",
        "Increased volume of distribution and clearance, giving low peak concentrations; the dosing interval should be 6 hours, not 8.",
        "eGFR × 1.5, the review's rise in glomerular filtration, in the published renal term, so clearance × 1.5; volumes unchanged."),
    row("metronidazole", "unchanged", "Unchanged."),
    row("vancomycin", "applied",
        "Increased volume of distribution and clearance; the change in half-life is unknown.",
        "Creatinine clearance × 1.5, the review's rise in glomerular filtration, in the published clearance term; volumes unchanged."),
    # --- Table 8, antiemetics -----------------------------------------------
    row("dexamethasone", "applied",
        "Clearance doubled in a singleton pregnancy, by placental metabolism; a multiple gestation needs more.",
        "Elimination clearance × 2; volumes unchanged."),
    row("ondansetron", "unchanged",
        "No change in steady-state concentrations in the first trimester."),
    # --- Table 2 and the text, analgesics ----------------------------------
    row("aspirin", "unquantified",
        "Slower uptake and a lower peak plasma concentration after a single dose. No magnitude is given."),
    row("acetaminophen", "unchanged",
        "Maternal absorption, metabolism and clearance of oral acetaminophen are unchanged.")
  )
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
