# The Patient Profile's hover tips: which drugs each pharmacogenetic field and
# the serum creatinine reach.
#
# getDrugPK() passes a covariate only to the models that name it in their
# signature (`cyp2d6`, `cyp2c19`, `cyp2c9`, `cyp2b6`, `creatinine`,
# `gestationalAge`), so the
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
  cyp4f2     = "CYP4F2",
  creatinine = "Serum creatinine",
  gestationalAge = "Gestational age at birth"
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
#' The grey hint shown in an empty Serum creatinine box
#'
#' The field is optional, and an empty box says nothing about what empty
#' MEANS: each renal model substitutes the normal creatinine for the patient's
#' age and sex (`assumedCreatinine()`, R/renalFunction.R).  The placeholder
#' shows that value, so the assumption is visible without hovering for the tip,
#' which states the same thing in words.  It is only a hint: it disappears as
#' soon as anything is typed, and an empty field still reaches the models as
#' NULL.
#'
#' @param age age in years
#' @param sex `"male"` or `"female"`
#' @return the placeholder string, e.g. `"0.9 assumed"`
#' @keywords internal
creatininePlaceholder <- function(age, sex) {
  sprintf("%s assumed",
          format(round(assumedCreatinine(age, sex), 2), nsmall = 1, trim = TRUE))
}

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
      "rate of phenytoin. It also lowers vitamin-K-antagonist clearance,",
      "so the INR rises on less drug: the warfarin model distinguishes only",
      "*1/*3 from *1/*1, while fluindione and acenocoumarol apply the fall",
      "to any *3 carrier."
    ),
    cyp2b6 = paste(
      "CYP2B6 metaboliser phenotype, the five CPIC categories.",
      "It sets the non-renal clearance of each methadone enantiomer;",
      "the rapid and ultrarapid phenotypes take the normal metaboliser's",
      "values."
    ),
    vkorc1 = paste(
      "VKORC1 -1639G>A genotype (GG is the wild-type reference, AA the",
      "sensitive one).",
      "It sets sensitivity to the vitamin-K antagonists: the A allele lowers",
      "the C50, so the INR rises on less drug. Read by the library warfarin",
      "(Xue) model, the Hamberg companion model (with CYP2C9 and age), and",
      "fluindione and acenocoumarol.",
      "AA, the most sensitive genotype, is assumed until a genotype is",
      "entered, so that the curve errs toward the larger INR. It is also the",
      "commonest genotype in the Asian patients of the source cohort, which",
      "excluded South Indians, while GG is commonest in Black patients and",
      "GA in White ones. These are group frequencies, not findings about a",
      "patient; the Patient Profile help page gives them with their source."
    ),
    cyp4f2 = paste(
      "CYP4F2 rs2108622 (1297C>T) genotype. A C allele lowers the warfarin",
      "C50, so the INR rises on less drug; T/T needs the most warfarin.",
      "CC, the most sensitive genotype, is assumed until a genotype is",
      "entered, so that the curve errs toward the larger INR."
    ),
    creatinine = paste(
      "The patient's serum creatinine, from which the renally cleared",
      "drugs estimate renal function."
    ),
    gestationalAge = paste(
      "The gestational age at birth, in weeks. With the age it gives the",
      "postmenstrual age, on which the clearance of the neonatal models",
      "that read it matures."
    )
  )
  trail <- if (arg == "creatinine") sprintf(
    paste(
      "Leave blank to assume a normal creatinine for the patient's age and",
      "sex: %s mg/dL for a man, %s for a woman, and the normal for age in a",
      "child. Renal impairment is then not represented."
    ),
    SCR_ASSUMED_MALE, SCR_ASSUMED_FEMALE
  ) else if (arg == "gestationalAge") sprintf(
    "Leave blank for a term birth (%s weeks): a premature infant's clearance is then overstated.",
    GESTATIONAL_AGE_TERM
  ) else NULL
  paste(c(lead, readers, trail), collapse = " ")
}

# The Patient Profile fields and Graph Options controls shown only while a drug
# they apply to is in the dose table (Steven L. Shafer, 2026-10-28: as the
# library grew, the panels filled with fields that do nothing for the drugs
# plotted).  Each name is the control's `relevantField()` id without its
# "field_" prefix, except the two Additional plots choices, which the server
# hides inside their checkbox group.
RELEVANT_FIELDS <- unname(c(
  "pregnant", "opioidTolerance", names(PATIENT_FIELD_LABELS), "osmolality",
  "showMEAC", "opioidMacInteraction",
  vapply(COMPANION_MODEL_GROUPS, `[[`, character(1), "input"),
  ASM_INTERACTIONS_INPUT,
  "plotMEAC", "plotInteraction"
))

# The drugs whose model reads each covariate: the model signatures, read once
# per drug table, since a session asks on every change of the dose table.
patientFieldReaderCache <- new.env(parent = emptyenv())
patientFieldReaderSets <- function(drugDefaults = getDrugDefaultsGlobal()) {
  key <- paste(drugDefaults$Drug, collapse = ",")
  if (!identical(patientFieldReaderCache$key, key)) {
    args <- c(names(PATIENT_FIELD_LABELS), "pregnant")
    patientFieldReaderCache$sets <- stats::setNames(
      lapply(args, patientFieldReaders, drugDefaults = drugDefaults), args)
    patientFieldReaderCache$key <- key
  }
  patientFieldReaderCache$sets
}

#' Which of the optional controls apply to the drugs in the dose table
#'
#' A covariate field is relevant when a drug in the table names it in its
#' model's signature (as getDrugPK() passes it), the baseline osmolality when
#' an osmotic agent is, and the opioid controls (Opioid tolerance, % MEAC, the
#' MEAC plot) when an opioid is: a drug of the Opioids category or with a
#' MEAC.  The opioid-MAC interaction needs an inhaled agent as well, and the
#' Interaction plot propofol.  A companion model's box is relevant when a
#' drug of its group is dosed, and the antiseizure interactions box when the
#' table holds an interaction the model applies.
#'
#' @param drugs the drug names in the dose table (blanks are ignored).
#' @param drugDefaults the session's drug table.
#' @return a logical vector named by `RELEVANT_FIELDS`.
#' @noRd
relevantFields <- function(drugs, drugDefaults = getDrugDefaultsGlobal()) {
  drugs <- unique(drugs[!is.na(drugs) & nzchar(drugs)])
  rows <- drugDefaults[drugDefaults$Drug %in% drugs, , drop = FALSE]
  readers <- patientFieldReaderSets(drugDefaults)
  reads <- function(arg) any(drugs %in% readers[[arg]])
  opioid <- any(rows$Category %in% "Opioids" |
                (!is.na(rows$MEAC) & rows$MEAC > 0))
  # a potent agent, not a carrier gas: only an agent has a MAC to interact with
  gas <- any(drugs %in% potentAgents())
  out <- c(
    pregnant        = reads("pregnant"),
    opioidTolerance = opioid,
    vapply(names(PATIENT_FIELD_LABELS), reads, logical(1)),
    osmolality      = any(rows$Concentration.Units %in% "mOsm"),
    showMEAC        = opioid,
    opioidMacInteraction = opioid && gas,
    vapply(names(COMPANION_MODEL_GROUPS), function(group)
      any(drugs %in% companionGroupDrugs(group)), logical(1)) |>
      stats::setNames(vapply(COMPANION_MODEL_GROUPS, `[[`, character(1), "input")),
    stats::setNames(nrow(activeAntiseizureInteractions(drugs)) > 0,
                    ASM_INTERACTIONS_INPUT),
    plotMEAC        = opioid,
    plotInteraction = opioid && "propofol" %in% drugs
  )
  out[RELEVANT_FIELDS]
}
