# The Patient Profile's hover tips: which drugs each pharmacogenetic field and
# the serum creatinine reach.
#
# getDrugPK() passes a covariate only to the models that name it in their
# signature (`cyp2d6`, `cyp2c19`, `cyp2c9`, `cyp2b6`, `creatinine`,
# `gestationalAge`, `albumin`, `ast`, `pulmonaryHypertension`, `infection`,
# and the clotting-factor baselines of R/factorBaselines.R),
# so the signatures are the one authoritative list of what a field affects.
# The tooltips read them, as the Cautions page's test does, so a new model that
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
  gestationalAge = "Gestational age at birth",
  albumin    = "Serum albumin",
  ast        = "Serum AST",
  pulmonaryHypertension = "Pulmonary hypertension",
  infection  = "Infection",
  # The clotting-factor baselines, one per factor (R/factorBaselines.R)
  stats::setNames(vapply(FACTOR_BASELINE_FIELDS, `[[`, character(1), "label"),
                  names(FACTOR_BASELINE_FIELDS))
)

# A qualification printed after a drug's name in its tooltip, where a model
# names a covariate but applies it to only part of itself.  Meloxicam's renal
# term is on its intravenous system (ANJESO) only; its oral fit has none
# (R/drugs_meloxicam.R).
PATIENT_FIELD_READER_NOTES <- list(
  creatinine = c(meloxicam = "intravenous only", mirabegron = "on bioavailability")
)

#' The drugs whose model reads a Patient Profile covariate
#'
#' @param arg the covariate as the model signatures name it: one of
#'   `names(PATIENT_FIELD_LABELS)`.
#' @param drugDefaults the drug table (`getDrugDefaultsGlobal()`).
#' @return the drug names, in the dose table's spelling, alphabetical.
#' @noRd
patientFieldReaders <- function(arg, drugDefaults = getDrugDefaultsGlobal()) {
  # a candidate model of a drug whose model the program chooses is read
  # through its drug's name (R/modelSelection.R), so only offered names appear
  drugs <- setdiff(drugDefaults$Drug, modelCandidates())
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
  # A component of a combination product is entered as the product
  # (R/combination-products.R): "ceftolozane (given as Zerbaxa)"
  products <- combinationProductFor(readers)
  named <- vapply(seq_along(readers), function(i) {
    drug <- readers[i]
    if (drug %in% names(notes)) sprintf("%s (%s)", drug, notes[[drug]])
    else if (!is.na(products[i]))
      # a component that is also dosed in its own right (imipenem, as
      # PRIMAXIN) is "also given as" its product
      sprintf(if (isTRUE(drugDefaults$Category[match(drug, drugDefaults$Drug)] != ""))
                "%s (also given as %s)" else "%s (given as %s)", drug, products[i])
    else drug
  }, character(1))
  sprintf("Read by %s; no other drug is affected.", patientFieldList(named))
}

#' The grey hint shown in an empty Serum creatinine box
#'
#' The field is optional, and an empty box says nothing about what empty
#' MEANS: each renal model substitutes the normal creatinine for the patient's
#' age and sex (`assumedCreatinine()`, R/renalFunction.R).  The placeholder
#' shows that value, the assumed one for age and sex before any model's own
#' conversion (adultEquivalentCreatinine() for an equation fitted in adults),
#' so the assumption is visible without hovering for the tip,
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
    ),
    albumin = paste(
      "The patient's serum albumin, in g/dL. It sets the methotrexate",
      "clearance of the adult model (lower albumin, lower clearance) and,",
      "at 2.5 g/dL or less, the faster distribution of the children's model;",
      "docetaxel's clearance; nab-paclitaxel's maximal elimination rate;",
      "tolcapone's peripheral volume (held within its studied 2.6-5.5 g/dL);",
      "emicizumab's clearance (lower albumin, higher clearance; held within",
      "its studied 1.68-5.66 g/dL); and the clearance and central volume of",
      "total mycophenolic acid (lower albumin, higher both; held within its",
      "studied 2.2-5.3 g/dL); and, at 2.5 g/dL or less, micafungin's",
      "clearance and volumes in the intensive-care model (14% higher)."
    ),
    ast = paste(
      "The patient's serum aspartate aminotransferase (AST), in U/L. It sets",
      "epirubicin's clearance (higher AST, lower clearance), held within the",
      "7 to 815 U/L its study covered."
    ),
    pulmonaryHypertension = paste(
      "Does the patient have pulmonary arterial hypertension? A drug given both",
      "for PAH and for something else was studied in both populations, and",
      "patients with PAH, often with severe heart disease, clear it more slowly.",
      "Yes gives the PAH population's pharmacokinetics; No the population of",
      "its other use (erectile dysfunction, healthy volunteers)."
    ),
    infection = paste(
      "The patient's infection. Some antibiotic models were fitted in healthy",
      "volunteers and in patients with each kind of infection, whose clearance",
      "and volumes differ: pneumonia, for example, enlarges the central",
      "volume. None gives the healthy volunteers' pharmacokinetics; a",
      "ventilated pneumonia is a category of its own, as the sources fitted",
      "it."
    ),
    factorVIIIBaseline = paste(
      "The patient's own factor VIII activity, in IU/dL (% of normal), before",
      "any concentrate. The factor VIII curve is this activity plus the",
      "activity infused, so a perioperative target can be read on it."
    ),
    factorIXBaseline = paste(
      "The patient's own factor IX activity, in IU/dL (% of normal), before",
      "any concentrate. The factor IX curve is this activity plus the",
      "activity infused, so a perioperative target can be read on it."
    ),
    fibrinogenBaseline = paste(
      "The patient's own plasma fibrinogen (Clauss), in mg/dL, before any",
      "concentrate. The fibrinogen curve is this level plus the fibrinogen",
      "infused, so a target can be read on it."
    ),
    igGBaseline = paste(
      "The patient's own serum IgG, in g/L, before any intravenous",
      "immunoglobulin. The immunoglobulin curve is this level plus the IgG",
      "infused."
    ),
    antithrombinBaseline = paste(
      "The patient's own antithrombin activity, in IU/dL (% of normal),",
      "before any concentrate. The antithrombin curve is this level plus the",
      "activity infused, and the model's volume depends on it."
    ),
    vonWillebrandFactorBaseline = paste(
      "The patient's own von Willebrand factor activity (ristocetin",
      "cofactor, VWF:RCo), in IU/dL (% of normal), before any concentrate.",
      "The VWF curve is this level plus the activity infused, and the factor",
      "VIII curve drawn with it follows the total."
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
  ) else if (arg == "albumin") paste(
    "Leave blank to hold each model's reference albumin (4 g/dL in the",
    "adult methotrexate model, a normal albumin in the children's, 3.9 g/dL",
    "for nab-paclitaxel, 4.4 g/dL for tolcapone, 4.5 g/dL for emicizumab,",
    "3.0 g/dL for mycophenolate, above 2.5 g/dL for micafungin);",
    "hypoalbuminaemia is then",
    "not represented."
  ) else if (arg %in% names(FACTOR_BASELINE_FIELDS)) paste(
    "Leave blank to take the baseline of the population the model was",
    "fitted in, which the drug's help page gives (severe haemophilia A or B",
    "for factors VIII and IX, afibrinogenaemia for fibrinogen, Guillain-Barre",
    "syndrome for immunoglobulin, 60 IU/dL in the children antithrombin was",
    "fitted in, hereditary deficiency for antithrombin alfa, type 3 von",
    "Willebrand disease for VWF): a patient with",
    "a different level is then",
    "drawn too high or too low."
  ) else if (arg == "ast") sprintf(
    "Leave blank to assume a normal AST (%s U/L): liver dysfunction is then not represented.",
    EPIRUBICIN_REFERENCE_AST
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
  "showMEAC", "opioidMacInteraction", OPIOID_RECEPTOR_INPUT,
  OPIOID_SENSITIVITY_INPUT, BUPRENORPHINE_OFFSET_INPUT,
  vapply(COMPANION_MODEL_GROUPS, `[[`, character(1), "input"),
  ASM_INTERACTIONS_INPUT,
  "inhaledAgents",
  "showAdaptation", "showEffect",
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
#' MEAC.  The altitude slider and the endobronchial intubation box act on the
#' inhaled agents only, and show while a potent agent is in the table.  The opioid-MAC interaction needs an inhaled agent as well, and the
#' Interaction plot propofol.  A companion model's box is relevant when a
#' drug of its group is dosed, and the antiseizure interactions box when the
#' table holds an interaction the model applies.
#'
#' Three options draw a panel of something that is plotted, and so follow the
#' drugs that are DRAWN, those given a dose above 0 (`plotted`), rather than
#' every drug in the table: the Interaction plot needs propofol and an opioid
#' both drawn (Steven L. Shafer, 2026-11-18: it is for anesthetic drug
#' interactions, and appeared beside the startup table's drugs at 0),
#' Tolerance and tachyphylaxis a drawn drug with an applied adaptation model
#' (R/pdAdaptation.R), and % effect a drawn drug with a drawn % effect row
#' (R/pdEffect.R).
#'
#' @param drugs the drug names in the dose table (blanks are ignored).
#' @param drugDefaults the session's drug table.
#' @param plotted the drugs drawn, given a dose above 0 (positiveDoseDrugs()),
#'   and the metabolites they form, which need no row of their own in the
#'   dose table (codeine's morphine); by default every drug in `drugs`.
#' @return a logical vector named by `RELEVANT_FIELDS`.
#' @noRd
relevantFields <- function(drugs, drugDefaults = getDrugDefaultsGlobal(),
                           plotted = drugs) {
  drugs <- unique(drugs[!is.na(drugs) & nzchar(drugs)])
  plotted <- unique(plotted[!is.na(plotted) & nzchar(plotted)])
  isOpioid <- function(d) {
    r <- drugDefaults[drugDefaults$Drug %in% d, , drop = FALSE]
    any(r$Category %in% "Opioids" | (!is.na(r$MEAC) & r$MEAC > 0))
  }
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
    # offered only while a mu partial agonist or antagonist (buprenorphine,
    # naloxone) is in the table
    stats::setNames(any(drugs %in% opioidReceptorSpecialDrugs()), OPIOID_RECEPTOR_INPUT),
    # PROTOTYPE sliders: sensitivity shows with any special drug (it moves the
    # receptor panels); onset/offset shows only with buprenorphine (it scales
    # buprenorphine's biophase).
    stats::setNames(any(drugs %in% opioidReceptorSpecialDrugs()), OPIOID_SENSITIVITY_INPUT),
    stats::setNames("buprenorphine" %in% drugs, BUPRENORPHINE_OFFSET_INPUT),
    vapply(names(COMPANION_MODEL_GROUPS), function(group)
      any(drugs %in% companionGroupDrugs(group)), logical(1)) |>
      stats::setNames(vapply(COMPANION_MODEL_GROUPS, `[[`, character(1), "input")),
    stats::setNames(nrow(activeAntiseizureInteractions(drugs)) > 0,
                    ASM_INTERACTIONS_INPUT),
    # The physiology controls that act on the inhaled agents only: altitude
    # and endobronchial intubation (R/altitude.R, R/endobronchial.R)
    inhaledAgents   = gas,
    showAdaptation  = length(pdAdaptationDrugs(plotted, drugDefaults = drugDefaults)) > 0,
    showEffect      = length(pdEffectDrugs(plotted)) > 0,
    plotMEAC        = opioid,
    plotInteraction = "propofol" %in% plotted &&
                      isOpioid(setdiff(plotted, "propofol"))
  )
  out[RELEVANT_FIELDS]
}
