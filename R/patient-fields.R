# The Patient Profile's hover tips: which drugs each pharmacogenetic field and
# the serum creatinine reach.
#
# getDrugPK() passes a covariate only to the models that name it in their
# signature (`cyp2d6`, `cyp2c19`, `cyp2c9`, `cyp2b6`, `creatinine`), so the
# signatures are the one authoritative list of what a field affects.  The
# tooltips read them, as the Cautions page's test does, so a new model that
# names a covariate joins its tooltip by itself, and a tooltip can never
# promise an effect the code does not have.

# The covariates the Patient Profile offers, in the order the panel shows
# them, with the name of each as the panel labels it.
PATIENT_FIELD_LABELS <- c(
  cyp2d6     = "CYP 2D6",
  cyp2c19    = "CYP 2C19",
  cyp2c9     = "CYP 2C9",
  cyp2b6     = "CYP 2B6",
  vkorc1     = "VKORC1",
  creatinine = "Serum creatinine"
)

# A qualification printed after a drug's name in its tooltip, where a model
# names a covariate but applies it to only part of itself.  Meloxicam's renal
# term is on its intravenous system (ANJESO) only; its oral fit has none
# (R/drugs_meloxicam.R).
PATIENT_FIELD_READER_NOTES <- list(
  creatinine = c(meloxicam = "intravenous only")
)

#' The drugs whose model reads a Patient Profile covariate
#'
#' @param arg the covariate as the model signatures name it: one of
#'   `names(PATIENT_FIELD_LABELS)`.
#' @param drugDefaults the drug table (`getDrugDefaultsGlobal()`).
#' @return the drug names, in the dose table's spelling, alphabetical.
#' @noRd
patientFieldReaders <- function(arg, drugDefaults = getDrugDefaultsGlobal()) {
  drugs <- drugDefaults$Drug
  readers <- Filter(function(drug) helpDrugDeclares(drug, arg), drugs)
  sort(unique(as.character(readers)))
}

#' An English list: "a", "a and b", "a, b and c"
#' @noRd
patientFieldList <- function(x) {
  n <- length(x)
  if (n == 0) return("")
  if (n == 1) return(x)
  paste(paste(x[-n], collapse = ", "), x[n], sep = " and ")
}

#' The sentence naming the drugs a Patient Profile field reaches
#'
#' "Read by codeine, hydrocodone and tramadol; no other drug is affected."
#' A drug with a note in `PATIENT_FIELD_READER_NOTES` carries it in
#' parentheses.
#'
#' @inheritParams patientFieldReaders
#' @noRd
patientFieldReadersSentence <- function(arg, drugDefaults = getDrugDefaultsGlobal()) {
  readers <- patientFieldReaders(arg, drugDefaults)
  if (length(readers) == 0) return("No drug in the library reads it.")
  notes <- PATIENT_FIELD_READER_NOTES[[arg]]
  named <- vapply(readers, function(drug) {
    if (drug %in% names(notes)) sprintf("%s (%s)", drug, notes[[drug]]) else drug
  }, character(1), USE.NAMES = FALSE)
  sprintf("Read by %s; no other drug is affected.", patientFieldList(named))
}

#' The hover tip of a Patient Profile field
#'
#' One short paragraph: what the field is, then the drugs that read it
#' (`patientFieldReadersSentence()`), then for the serum creatinine what a
#' blank field means, with the assumed adult values quoted from
#' `R/renalFunction.R` so the tip can never disagree with the code.
#'
#' @inheritParams patientFieldReaders
#' @return a single string for `bslib::tooltip()`.
#' @noRd
patientFieldTooltip <- function(arg, drugDefaults = getDrugDefaultsGlobal()) {
  arg <- match.arg(arg, names(PATIENT_FIELD_LABELS))
  readers <- patientFieldReadersSentence(arg, drugDefaults)
  lead <- switch(
    arg,
    cyp2d6 = paste(
      "CYP2D6 metaboliser phenotype, as a genotyping laboratory reports it.",
      "It sets how much of the opioids that read it is converted to their",
      "active metabolites, and the clearance of the antipsychotics that",
      "read it."
    ),
    cyp2c19 = paste(
      "CYP2C19 metaboliser phenotype, the five CPIC categories.",
      "It sets the clearance of the drugs that read it."
    ),
    cyp2c9 = paste(
      "CYP2C9 genotype, as a diplotype (*1/*1 is the reference).",
      "It sets the clearance of meloxicam and the maximal elimination",
      "rate of phenytoin."
    ),
    cyp2b6 = paste(
      "CYP2B6 metaboliser phenotype, the five CPIC categories.",
      "It sets the non-renal clearance of each methadone enantiomer;",
      "the rapid and ultrarapid phenotypes take the normal metaboliser's",
      "values."
    ),
    vkorc1 = paste(
      "VKORC1 -1639G>A genotype (GG is the wild-type reference).",
      "It sets warfarin sensitivity: the A allele lowers the S-warfarin",
      "C50, so the INR rises on less drug. Read by the library (Xue) model",
      "and, with CYP2C9 and age, by the Hamberg companion model."
    ),
    creatinine = paste(
      "The patient's serum creatinine, from which the renally cleared",
      "drugs estimate renal function."
    )
  )
  trail <- if (arg == "creatinine") sprintf(
    paste(
      "Leave blank to assume a normal creatinine for the patient's age and",
      "sex: %s mg/dL for a man, %s for a woman, and the normal for age in a",
      "child. Renal impairment is then not represented."
    ),
    SCR_ASSUMED_MALE, SCR_ASSUMED_FEMALE
  ) else NULL
  paste(c(lead, readers, trail), collapse = " ")
}
