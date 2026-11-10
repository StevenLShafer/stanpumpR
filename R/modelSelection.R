# -----------------------------------------------------------------------------
# The program chooses the model (AGENTS.md, Key Rules; Shafer 2026-11-09)
# -----------------------------------------------------------------------------
# Where the library holds several published models of one drug, the clinician
# sees one name and stanpumpR chooses the model whose SOURCE POPULATION best
# matches the patient entered.  The rule (Shafer, 2026-11-10):
#
#   1. the candidates whose studied age range (and weight range, where the
#      source printed one) contains the patient;
#   2. among those, the one fitted to MORE patients, then the newer;
#   3. if none contains the patient, the one whose ranges lie nearest (years
#      outside the age range, then kilograms outside the weight range), then
#      rule 2.
#
# Each group's name is an ordinary drug function built by modelGroup(): its
# signature is the union of its candidates' (so getDrugPK() passes it every
# covariate any candidate reads, and the Patient Profile shows the field), it
# calls the chosen candidate with the arguments that candidate names, and it
# prefixes the reference with the choice.  A metabolite formed by a group's
# candidates is renamed to the group's metabolite, itself a group with the
# same population table, so parent and metabolite always come from the same
# source (the "Parents and their metabolites" rule).
#
# The candidates stay in the library under their own names, with no Category
# (so they are not offered in the dose table), for their tests and help.
# Where a source does not print its age range, an adult study is taken as 18
# years and over (ageMax Inf), which the table says.
# -----------------------------------------------------------------------------

#' The candidate models of each drug and their source populations
#'
#' One data frame per group: `model` (the candidate function), `source`,
#' `ageMin`/`ageMax` (years), `weightMin`/`weightMax` (kg; NA where not
#' printed), `n` (patients fitted) and `year`.
#' @keywords internal
MODEL_GROUPS <- list(
  etoposide = data.frame(
    model = c("etoposideKroghMadsen", "etoposidePediatric", "etoposideDuong"),
    source = c("Krogh-Madsen 2012", "Urien 2011", "Duong 2019"),
    ageMin = c(22, 14 / 52, 1.7), ageMax = c(68, 16.7, 8.3),
    weightMin = c(46, 5.3, 8.6), weightMax = c(117, 66, 31),
    n = c(23, 67, 51), year = c(2012, 2011, 2019)),
  carboplatin = data.frame(
    model = c("carboplatinDuffull", "carboplatinPediatric"),
    source = c("Duffull 1997", "Duong 2019"),
    ageMin = c(49, 1.7), ageMax = c(68, 8.3),
    weightMin = c(46, 8.6), weightMax = c(85, 31),
    n = c(12, 51), year = c(1997, 2019)),
  melphalan = data.frame(
    model = c("melphalanNath", "melphalanPediatric"),
    source = c("Nath 2010", "Duong 2019"),
    ageMin = c(36, 1.7), ageMax = c(73, 8.3),
    weightMin = c(42, 8.6), weightMax = c(132, 31),
    n = c(100, 51), year = c(2010, 2019)),
  methotrexate = data.frame(
    model = c("methotrexateIbarra", "methotrexatePediatric"),
    source = c("Ibarra 2023", "Taylor 2023"),
    ageMin = c(18, 0.1), ageMax = c(88, 32.3),
    weightMin = c(44, NA), weightMax = c(162, NA),
    n = c(243, 1668), year = c(2023, 2023)),
  vincristine = data.frame(
    model = c("vincristineIgarashi", "vincristinePediatric"),
    source = c("Igarashi 2021", "Nijstad 2022"),
    ageMin = c(25, 0.04), ageMax = c(82, 33.9),
    weightMin = c(NA, 2.9), weightMax = c(NA, 126),
    n = c(30, 206), year = c(2021, 2022)),
  oxaliplatin = data.frame(
    model = c("oxaliplatinBastian", "oxaliplatinNikanjam"),
    source = c("Bastian 2003", "Nikanjam 2015"),
    ageMin = c(18, 0.6), ageMax = c(Inf, 86),
    weightMin = c(41, 7), weightMax = c(107, 165),
    n = c(56, 186), year = c(2003, 2015)),
  fluorouracil = data.frame(
    model = c("fluorouracilBriki", "fluorouracilTan"),
    source = c("Briki 2026", "Tan 2026"),
    ageMin = c(24, 18), ageMax = c(90, Inf),
    weightMin = c(32, NA), weightMax = c(126, NA),
    n = c(726, 140), year = c(2026, 2026)),
  doxorubicin = data.frame(
    model = c("doxorubicinJoerger", "doxorubicinPediatric", "doxorubicinPerezBlanco"),
    source = c("Joerger 2007", "Voller 2015", "Perez-Blanco 2016"),
    ageMin = c(18, 0.2, 26), ageMax = c(Inf, 17.7, 84),
    weightMin = c(NA, NA, 43), weightMax = c(NA, NA, 110),
    n = c(59, 94, 44), year = c(2007, 2015, 2016)),
  doxorubicinol = data.frame(
    model = c("doxorubicinolJoerger", "doxorubicinolPediatric", "doxorubicinolPerezBlanco"),
    source = c("Joerger 2007", "Voller 2015", "Perez-Blanco 2016"),
    ageMin = c(18, 0.2, 26), ageMax = c(Inf, 17.7, 84),
    weightMin = c(NA, NA, 43), weightMax = c(NA, NA, 110),
    n = c(59, 94, 44), year = c(2007, 2015, 2016))
)

#' The metabolite group a parent group's candidates form
#' @keywords internal
MODEL_GROUP_METABOLITES <- c(doxorubicin = "doxorubicinol")

#' Every candidate model of every group
#' @returns the candidate function names
#' @keywords internal
modelCandidates <- function()
{
  unname(unlist(lapply(MODEL_GROUPS, function(g) g$model)))
}

#' The group a candidate model belongs to
#' @param drug drug names
#' @returns the group name of each, NA for a drug that is no candidate
#' @keywords internal
modelCandidateGroup <- function(drug)
{
  groups <- rep(names(MODEL_GROUPS), vapply(MODEL_GROUPS, nrow, integer(1)))
  groups[match(drug, modelCandidates())]
}

#' Which candidate of a group a patient gets
#'
#' @param group the group's name (a name of `MODEL_GROUPS`)
#' @param age years
#' @param weight kg
#' @returns the row of the group's table for the chosen candidate
#' @keywords internal
selectModel <- function(group, age, weight)
{
  tab <- MODEL_GROUPS[[group]]
  if (is.null(tab)) stop("No model group named '", group, "'.")
  gap <- function(x, lo, hi) ifelse(is.na(lo), 0, pmax(lo - x, 0)) +
    ifelse(is.na(hi), 0, pmax(x - hi, 0))
  ageGap <- gap(age, tab$ageMin, tab$ageMax)
  weightGap <- gap(weight, tab$weightMin, tab$weightMax)
  inside <- ageGap == 0 & weightGap == 0
  if (any(inside)) {
    tab <- tab[inside, , drop = FALSE]
    ord <- order(-tab$n, -tab$year)
  } else {
    ord <- order(ageGap, weightGap, -tab$n, -tab$year)
  }
  tab[ord[1], , drop = FALSE]
}

#' The sentence that names the model chosen and why
#' @keywords internal
modelChoiceSentence <- function(group, chosen, age, weight)
{
  tab <- MODEL_GROUPS[[group]]
  range <- function(lo, hi, unit)
    if (is.na(lo)) NULL else if (is.infinite(hi)) paste0(lo, " ", unit, " and over") else
      paste0(signif(lo, 3), "-", signif(hi, 3), " ", unit)
  pop <- paste(c(range(chosen$ageMin, chosen$ageMax, "years"),
                 range(chosen$weightMin, chosen$weightMax, "kg")), collapse = ", ")
  paste0("stanpumpR chose ", chosen$source, " of the ", nrow(tab), " ", group,
         " models (", paste(tab$source, collapse = ", "), ") for this patient: its ",
         chosen$n, " patients (", pop, ") ",
         if (gap0(chosen, age, weight)) "include the patient's age and weight, and no larger study does. "
         else "are the nearest to the patient's age and weight, no study including them. ")
}

gap0 <- function(row, age, weight)
{
  inRange <- function(x, lo, hi) is.na(lo) || (x >= lo && x <= hi)
  inRange(age, row$ageMin, row$ageMax) && inRange(weight, row$weightMin, row$weightMax)
}

#' Build a group's drug function
#'
#' @param group the group's name
#' @returns a function with the union of the candidates' arguments that calls
#'   the candidate `selectModel()` chooses for the patient
#' @keywords internal
modelGroup <- function(group)
{
  candidates <- MODEL_GROUPS[[group]]$model
  forms <- list()
  for (m in candidates) {
    f <- formals(get(m, mode = "function"))
    for (a in names(f)) if (is.null(forms[[a]]) && !a %in% names(forms)) forms[a] <- list(f[[a]])
  }
  fn <- function() {
    args <- mget(names(formals()), envir = environment())
    chosen <- selectModel(group, args$age, args$weight)
    model <- get(chosen$model, mode = "function")
    X <- do.call(model, args[names(args) %in% names(formals(model))])
    X$reference <- paste0(modelChoiceSentence(group, chosen, args$age, args$weight),
                          X$reference)
    X$modelChosen <- chosen$model
    if (!is.null(X$metabolite) && group %in% names(MODEL_GROUP_METABOLITES))
      X$metabolite$name <- MODEL_GROUP_METABOLITES[[group]]
    X
  }
  formals(fn) <- forms
  fn
}

# ---- The groups' drug functions (the candidates are defined in R/drugs_*.R,
# ---- which collate before this file) ----

#' Etoposide, intravenous: the model chosen for the patient
#'
#' Krogh-Madsen 2012 (adults), Urien 2011 (children) or Duong 2019 (children
#' with neuroblastoma), chosen by \code{selectModel()}; see
#' \code{R/modelSelection.R}.
#' @param ... the patient's covariates, as the candidates take them
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
etoposide <- modelGroup("etoposide")

#' Carboplatin, intravenous: the model chosen for the patient
#'
#' Duffull 1997 (adults) or Duong 2019 (children with neuroblastoma), chosen by \code{selectModel()}; see \code{R/modelSelection.R}.
#' @param ... the patient's covariates, as the candidates take them
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
carboplatin <- modelGroup("carboplatin")

#' Melphalan, intravenous: the model chosen for the patient
#'
#' Nath 2010 (adults) or Duong 2019 (children with neuroblastoma), chosen by \code{selectModel()}; see \code{R/modelSelection.R}.
#' @param ... the patient's covariates, as the candidates take them
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
melphalan <- modelGroup("melphalan")

#' Methotrexate, high-dose intravenous: the model chosen for the patient
#'
#' Ibarra 2023 (adults) or Taylor 2023 (children and young adults), chosen by \code{selectModel()}; see \code{R/modelSelection.R}.
#' @param ... the patient's covariates, as the candidates take them
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
methotrexate <- modelGroup("methotrexate")

#' Vincristine, intravenous: the model chosen for the patient
#'
#' Igarashi 2021 (adults) or Nijstad 2022 (children and young adults), chosen by \code{selectModel()}; see \code{R/modelSelection.R}.
#' @param ... the patient's covariates, as the candidates take them
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
vincristine <- modelGroup("vincristine")

#' Oxaliplatin, intravenous: the model chosen for the patient
#'
#' Bastian 2003 (adults) or Nikanjam 2015 (children and adults), chosen by \code{selectModel()}; see \code{R/modelSelection.R}.
#' @param ... the patient's covariates, as the candidates take them
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
oxaliplatin <- modelGroup("oxaliplatin")

#' Fluorouracil, intravenous: the model chosen for the patient
#'
#' Briki 2026 or Tan 2026 (adults), chosen by \code{selectModel()}; see \code{R/modelSelection.R}.
#' @param ... the patient's covariates, as the candidates take them
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
fluorouracil <- modelGroup("fluorouracil")

#' Doxorubicin, intravenous: the model chosen for the patient
#'
#' Joerger 2007 (adults), Voller 2015 (children) or Perez-Blanco 2016 (adults
#' with NHL), with the doxorubicinol each forms, chosen by
#' \code{selectModel()}; see \code{R/modelSelection.R}.
#' @param ... the patient's covariates, as the candidates take them
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
doxorubicin <- modelGroup("doxorubicin")

#' Doxorubicinol formed from doxorubicin: the model chosen for the patient
#'
#' The doxorubicinol model of the same source as the doxorubicin chosen,
#' by \code{selectModel()}; see \code{R/modelSelection.R}.
#' @param ... the patient's covariates, as the candidates take them
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
doxorubicinol <- modelGroup("doxorubicinol")
