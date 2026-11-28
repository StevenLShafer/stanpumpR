# -----------------------------------------------------------------------------
# The Patient Profile's Infection
# -----------------------------------------------------------------------------
# Drafted by Claude Code, 2026-11-23, at the request of Steven L. Shafer
# (2026-11-18: "Add an infection field. However, that should be a separate
# project.").
#
# Several antibiotic population models change their parameters with the
# patient's infection, and the Patient Profile had no way to enter it.  A model
# that has such a term names `infection = INFECTION_DEFAULT` in its signature;
# getDrugPK() passes the field only to the models that name it, so the
# Patient Profile's field is read, shown (relevantFields(), R/patient-fields.R)
# and listed on its hover tip only for those drugs, as Pulmonary hypertension
# is (R/pulmonaryHypertension.R).  The first model to read it is ceftolozane
# (R/drugs_ceftolozane.R, Zhang 2021, drawn for Zerbaxa; 2026-11-25), the
# second ceftazidime (R/drugs_ceftazidime.R, Franzese 2022, drawn for Avycaz;
# 2026-11-25, its own ventilation factor within pneumonia); imipenem
# (R/drugs_imipenem.R, Patel 2022) reads it too.  When
# this file was written NO model read it:
# the field was built ahead of the two drugs of the library expansion that
# need it (docs/library-expansion/plan.csv), and is hidden until one of them
# joins the library.
#
# THE SOURCES, READ ON THE PDF (2026-11-23)
# =========================================
# Patel M, Bellanti F, Daryani NM, et al.  Population pharmacokinetic/
# pharmacodynamic assessment of imipenem/cilastatin/relebactam in patients
# with hospital-acquired/ventilator-associated bacterial pneumonia.  Clin
# Transl Sci 2022;15:396-408 (doi 10.1111/cts.13158; PMID 34704389).  Infection type was tested as healthy / cUTI / cIAI /
# HABP-VABP (Methods); cIAI and cUTI could not be estimated apart and were
# pooled WITH the healthy participants as the reference group (Results;
# Table 3, footnote f), leaving two terms, both on imipenem's Table 3:
#     CL x (1 - 0.38)  with pneumonia
#     V1 x (1 - 0.39)  with pneumonia, then x (1 + 0.23) with ventilation
# Footnote g: "The ventilation effect on participants with pneumonia
# receiving ventilation.  The reference group was participants with
# pneumonia."  So ventilation is a state WITHIN pneumonia, not a covariate of
# its own.  Its sign: the table body prints +0.23 (95% CI 0.02 to 0.45, and
# the bootstrap's 0.24, 0.02 to 0.48), while footnote c's equation prints
# "(1+(Flag -0.23 Ventilation))"; the confidence interval lies wholly above
# zero, so the estimate is +0.23 and the footnote's minus is a misprint (the
# same footnote garbles its "x exp(eta2)" as "+exp(eta2)").  Relebactam's own
# terms are -0.43 (CL), -0.29 and +0.36 (V1), its footnote printing the
# ventilation term as "Flag x 0.36".
#
# Zhang Z, Patel YT, Fiedler-Kelly J, et al.  Population pharmacokinetic
# analysis for plasma and epithelial lining fluid ceftolozane/tazobactam
# concentrations in patients with ventilated nosocomial pneumonia.  J Clin
# Pharmacol 2021;61:254-268 (doi 10.1002/jcph.1733; PMID 32949031), Table 2
# and its equations (the adult plasma models).  Infection type was cUTI, cIAI,
# pneumonia or "other infection", the healthy participant the reference;
# the "other infection" patients were the critically ill augmented-renal-
# clearance cohort of study MK-7625A-007 (Methods; Table 2's note), 1% of the
# data, kept "as it represented a separate pathological state".  Every
# pneumonia patient (ASPECT-NP) was ventilated: ventilated HABP or VAP.
#     ceftolozane  CL x 1.18 cUTI, 1.43 cIAI;  Vc x 1.25 cUTI, 1.59 cIAI,
#                  2.00 pneumonia, 2.14 other;  Vp none
#     tazobactam   CL none;  Vc x 1.49 cIAI, 2.17 pneumonia, 2.49 other;
#                  Vp x 1.25 cUTI, 1.34 cIAI, 2.06 pneumonia
# End-stage renal disease (ceftolozane CL x 0.320, Vc x 1.30; tazobactam CL x
# 0.626, Vc x 0.749) is a renal state, not an infection, and is not part of
# this field.
#
# THE CATEGORIES
# ==============
# The union the two sources need:
#     none                 healthy, no infection (both sources' reference)
#     cUTI                 complicated urinary tract infection
#     cIAI                 complicated intra-abdominal infection
#     pneumonia            pneumonia, not mechanically ventilated
#     pneumoniaVentilated  pneumonia, mechanically ventilated
#     other                another infection
# Ventilation is a category of pneumonia rather than a yes/no of its own
# because both sources define it that way: Patel's term applies only within
# pneumonia, and Zhang's pneumonia patients were all ventilated.  A separate
# box would invite a ventilated patient without pneumonia, a combination
# neither source describes.  How a drug's model reads each category is that
# drug's own decision, made when it is built; what the sources suggest:
#   imipenem (Patel): none, cUTI and cIAI are the reference; "other" is not in
#     the source, so the drug must say what it does with it.
#   ceftolozane/tazobactam (Zhang): both pneumonia categories take the
#     pneumonia factors (its pneumonia was ventilated pneumonia, so a
#     non-ventilated patient is an extrapolation the drug's page must state);
#     "other" is Zhang's other infection, which was a critically ill cohort.
# A new source with a category not here (bacteraemia, febrile neutropenia,
# skin and soft-tissue infection) adds one; a category's VALUE never changes,
# since a bookmark carries it.
#
# THE DEFAULT (decided by Steven L. Shafer, 2026-11-24)
# ===========
# INFECTION_DEFAULT is "none", the healthy reference both sources were fitted
# against, so a model that reads the field gives its published typical values
# until an infection is entered.  Put to him as a [decision] on 2026-11-23,
# with the case against it: a patient given imipenem or ceftolozane/
# tazobactam is almost by definition infected, so "none" describes a patient
# who rarely receives the drug, and for these drugs an infection raises the
# volume (Zhang) or lowers clearance and volume together (Patel), so the
# plot then overstates the concentration an infected patient reaches.  The
# alternatives put to him were a default of the drug's commonest indication
# (which differs by drug, while the field is one for the patient) and no
# default, the field required.  He chose "none" (2026-11-24); the Cautions
# page states the overstatement.  Do not change it without reopening that
# decision.
# -----------------------------------------------------------------------------

INFECTION_NONE                 <- "none"
INFECTION_CUTI                 <- "cUTI"
INFECTION_CIAI                 <- "cIAI"
INFECTION_PNEUMONIA            <- "pneumonia"
INFECTION_PNEUMONIA_VENTILATED <- "pneumoniaVentilated"
INFECTION_OTHER                <- "other"

# The choices, in the order the Patient Profile lists them, each with its label
INFECTION_LABELS <- c(
  "None (healthy)"                         = INFECTION_NONE,
  "Complicated urinary tract infection"    = INFECTION_CUTI,
  "Complicated intra-abdominal infection"  = INFECTION_CIAI,
  "Pneumonia, not ventilated"              = INFECTION_PNEUMONIA,
  "Pneumonia, mechanically ventilated"     = INFECTION_PNEUMONIA_VENTILATED,
  "Other infection"                        = INFECTION_OTHER
)
INFECTION_VALUES <- unname(INFECTION_LABELS)

# Steven L. Shafer's decision of 2026-11-24: see THE DEFAULT above.
INFECTION_DEFAULT <- INFECTION_NONE

#' Check the infection category
#'
#' @param x one of `INFECTION_VALUES`, or NULL (`INFECTION_DEFAULT`)
#' @returns the category
#' @keywords internal
checkInfection <- function(x)
{
  if (is.null(x)) return(INFECTION_DEFAULT)
  if (!is.character(x) || length(x) != 1 || is.na(x) || !x %in% INFECTION_VALUES)
    stop("Invalid infection: ", paste(x, collapse = ", "),
         ". Must be one of: ", paste(INFECTION_VALUES, collapse = ", "))
  x
}

#' The label the Patient Profile shows for an infection category
#'
#' @param x one of `INFECTION_VALUES`
#' @returns its label, e.g. "Pneumonia, mechanically ventilated"
#' @keywords internal
infectionLabel <- function(x)
{
  names(INFECTION_LABELS)[match(x, INFECTION_LABELS)]
}
