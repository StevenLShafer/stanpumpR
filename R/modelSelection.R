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
    model = c("carboplatinDuffull", "carboplatinPediatric", "carboplatinPeng"),
    source = c("Duffull 1997", "Duong 2019", "Peng 1995"),
    ageMin = c(49, 1.7, 1 / 12), ageMax = c(68, 8.3, 221 / 12),
    weightMin = c(46, 8.6, 5.0), weightMax = c(85, 31, 63),
    n = c(12, 51, 22), year = c(1997, 2019, 1995)),
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
    # Bastian's Table 1 prints 41-79 years (Gemini's chemotherapy audit of
    # 2026-11-23, F3); it was coded 18 and over.
    ageMin = c(41, 0.6), ageMax = c(79, 86),
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
    # Joerger 2007 studied 29-81 years (its Discussion; card PMID_18027989),
    # coded 18 and over until Gemini's chemotherapy audit of 2026-11-23, F7.
    ageMin = c(29, 0.2, 26), ageMax = c(81, 17.7, 84),
    weightMin = c(NA, NA, 43), weightMax = c(NA, NA, 110),
    n = c(59, 94, 44), year = c(2007, 2015, 2016)),
  cyclophosphamide = data.frame(
    model = c("cyclophosphamideJoerger", "cyclophosphamideMcCune"),
    source = c("Joerger 2007", "McCune 2009"),
    # Joerger 2007's cohort, 29-81 years, as for doxorubicin above.
    ageMin = c(29, 1.30), ageMax = c(81, 9.37),
    weightMin = c(NA, 8.7), weightMax = c(NA, 52.8),
    n = c(59, 22), year = c(2007, 2009)),
  paclitaxel = data.frame(
    model = c("paclitaxelJoerger", "paclitaxelCrombag"),
    source = c("Joerger 2006", "Crombag 2019"),
    ageMin = c(33, 18), ageMax = c(86, 84),
    weightMin = c(NA, NA), weightMax = c(NA, NA),
    n = c(168, 684), year = c(2006, 2019)),
  temozolomide = data.frame(
    model = c("temozolomideOstermann", "temozolomidePanetta"),
    source = c("Ostermann 2004", "Panetta 2003"),
    ageMin = c(30, 0.7), ageMax = c(79, 21.9),
    weightMin = c(46, NA), weightMax = c(105, NA),
    n = c(35, 39), year = c(2004, 2003)),
  # Quinine (2026-11-28, Shafer: "Build it, say so"): both sources are
  # malaria patients; Kloprogge's 22 women were all pregnant.  Le Jouan
  # prints its children's weight as a mean (13.6 +/- 3.8 kg), not a range.
  quinine = data.frame(
    model = c("quinineKloprogge", "quinineLeJouan"),
    source = c("Kloprogge 2014", "Le Jouan 2005"),
    ageMin = c(18, 0.55), ageMax = c(37, 6.7),
    weightMin = c(44, NA), weightMax = c(71, NA),
    n = c(22, 30), year = c(2014, 2005)),
  # Pulmonary arterial hypertension (2026-11-18, Shafer: "Use Miura's model
  # for children").  Miura 2026's models are given the range of the
  # source's CHILDREN, not its whole cohort, so that they are chosen for
  # children only; n is the whole fit's.  The adults' sildenafil source (45
  # healthy men) prints no age range, so it is taken as 18 and over.
  sildenafil = data.frame(
    model = c("sildenafilFDA", "sildenafilMiura"),
    source = c("FDA review of NDA 22-473", "Miura 2026"),
    ageMin = c(18, 0.4), ageMax = c(Inf, 14.8),
    weightMin = c(NA, 6.5), weightMax = c(NA, 54.9),
    n = c(45, 24), year = c(2009, 2026)),
  tadalafil = data.frame(
    model = c("tadalafilFergusonSells", "tadalafilMiura"),
    source = c("Ferguson-Sells 2022", "Miura 2026"),
    ageMin = c(2.5, 0.1), ageMax = c(90.3, 8.3),
    weightMin = c(NA, 5.4), weightMax = c(NA, 23.9),
    n = c(324, 31), year = c(2022, 2026)),
  ambrisentan = data.frame(
    model = c("ambrisentanOkour", "ambrisentanMiura"),
    source = c("Okour 2023", "Miura 2026"),
    ageMin = c(8, 0.4), ageMax = c(16, 8.8),
    weightMin = c(NA, 7.1), weightMax = c(NA, 21.6),
    n = c(39, 29), year = c(2023, 2026)),
  # Landiolol (R/landiolol.R): Honda 2008's healthy men, Kunisawa's
  # gynaecologic women (J Anesth 2015, received 2014) and Kunisawa's patients
  # with peripheral arterial disease (Ther Clin Risk Manag 2015), each range
  # read on its PDF (2026-11-25).
  landiolol = data.frame(
    model = c("landiololHonda", "landiololKunisawa2014", "landiololKunisawa2015"),
    source = c("Honda 2008", "Kunisawa 2015 (gynaecologic)", "Kunisawa 2015 (PAD)"),
    ageMin = c(20, 37, 64), ageMax = c(33, 71, 84),
    weightMin = c(50.7, 47.1, 40.4), weightMax = c(77.2, 73.0, 71.8),
    n = c(47, 9, 8), year = c(2008, 2015, 2015)),
  # Ciprofol (R/ciprofol.R; Shafer, 2026-11-26): Liu 2024's 462 adults
  # (18-77 years, 41.6-97.9 kg, Table 1) and Wang 2026's 25 children (1-9
  # years, 9.8-37 kg), ranges read on the papers.
  ciprofol = data.frame(
    model = c("ciprofolLiu", "ciprofolWang"),
    source = c("Liu 2024", "Wang 2026"),
    ageMin = c(18, 1), ageMax = c(77, 9),
    weightMin = c(41.6, 9.8), weightMax = c(97.9, 37),
    n = c(462, 25), year = c(2024, 2026)),
  # Antithrombin III, plasma-derived (Shafer, 2026-11-26): Moffett 2017's
  # children (inclusion "<19 years", adolescents 13-18) and Ilias 2000's
  # adults with severe sepsis (18-85 years), ranges read on the PDFs; no
  # weight range printed in either.  The ranges meet from 18 to 19 years,
  # where the rule gives Moffett, the larger study.
  antithrombin = data.frame(
    model = c("antithrombinMoffett", "antithrombinIlias"),
    source = c("Moffett 2017", "Ilias 2000"),
    ageMin = c(0, 18), ageMax = c(19, 85),
    weightMin = c(NA, NA), weightMax = c(NA, NA),
    n = c(184, 32), year = c(2017, 2000)),
  # Micafungin (R/drugs_micafunginJullien.R, R/drugs_micafunginWasmann.R):
  # Jullien 2017's ventilated ICU patients and Wasmann 2019's normal-weight
  # and morbidly obese volunteers, each range read on its Table 1
  # (2026-11-26).  Neither studied children, who are given the nearer by age,
  # Wasmann's.
  micafungin = data.frame(
    model = c("micafunginJullien", "micafunginWasmann"),
    source = c("Jullien 2017", "Wasmann 2019"),
    ageMin = c(29.9, 22), ageMax = c(92.7, 61),
    weightMin = c(48, 61.5), weightMax = c(141, 184),
    n = c(99, 24), year = c(2017, 2019)),
  # Teicoplanin (R/drugs_teicoplanin*.R; Shafer, 2026-11-27: Kim 2024 for
  # every adult, Zhang 2020 for children): Kim's healthy adults (inclusion
  # 19-55 years; no weight range printed), chosen by the nearest rule above
  # 55, and Zhang's children, each read in full.  Wang 2023 was rejected for
  # its A2-2-only assay (R/drugs_teicoplaninKim.R).
  # Caspofungin (R/drugs_caspofungin*.R; Shafer, 2026-11-28: Xu 2026 for
  # children, Wurthwein 2013 for adults).  Between Xu's 16 and Wurthwein's 18
  # years the nearer range wins; an adult outside 43-104 kg is still nearer
  # Wurthwein by age.
  caspofungin = data.frame(
    model = c("caspofunginWurthwein", "caspofunginXu"),
    source = c("Wurthwein 2013", "Xu 2026"),
    ageMin = c(18, 0.33), ageMax = c(74, 16),
    weightMin = c(43, 4.9), weightMax = c(104, 74),
    n = c(46, 29), year = c(2013, 2026)),
  teicoplanin = data.frame(
    model = c("teicoplaninKim", "teicoplaninZhang"),
    source = c("Kim 2024", "Zhang 2020"),
    ageMin = c(19, 0.2), ageMax = c(55, 14),
    weightMin = c(NA, 2.9), weightMax = c(NA, 69),
    n = c(12, 159), year = c(2024, 2020)),
  doxorubicinol = data.frame(
    model = c("doxorubicinolJoerger", "doxorubicinolPediatric", "doxorubicinolPerezBlanco"),
    source = c("Joerger 2007", "Voller 2015", "Perez-Blanco 2016"),
    # Joerger 2007 studied 29-81 years (its Discussion; card PMID_18027989),
    # coded 18 and over until Gemini's chemotherapy audit of 2026-11-23, F7.
    ageMin = c(29, 0.2, 26), ageMax = c(81, 17.7, 84),
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
#' @inheritParams etoposideKroghMadsen
#' @inheritParams etoposidePediatric
#' @inheritParams etoposideDuong
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
etoposide <- modelGroup("etoposide")

#' Carboplatin, intravenous: the model chosen for the patient
#'
#' Duffull 1997 (adults), Duong 2019 (children with neuroblastoma) or Peng 1995
#' (children), chosen by \code{selectModel()}; see \code{R/modelSelection.R}.
#' @inheritParams carboplatinDuffull
#' @inheritParams carboplatinPediatric
#' @inheritParams carboplatinPeng
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
carboplatin <- modelGroup("carboplatin")

#' Melphalan, intravenous: the model chosen for the patient
#'
#' Nath 2010 (adults) or Duong 2019 (children with neuroblastoma), chosen by \code{selectModel()}; see \code{R/modelSelection.R}.
#' @inheritParams melphalanNath
#' @inheritParams melphalanPediatric
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
melphalan <- modelGroup("melphalan")

#' Methotrexate, high-dose intravenous: the model chosen for the patient
#'
#' Ibarra 2023 (adults) or Taylor 2023 (children and young adults), chosen by \code{selectModel()}; see \code{R/modelSelection.R}.
#' @inheritParams methotrexateIbarra
#' @inheritParams methotrexatePediatric
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
methotrexate <- modelGroup("methotrexate")

#' Vincristine, intravenous: the model chosen for the patient
#'
#' Igarashi 2021 (adults) or Nijstad 2022 (children and young adults), chosen by \code{selectModel()}; see \code{R/modelSelection.R}.
#' @inheritParams vincristineIgarashi
#' @inheritParams vincristinePediatric
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
vincristine <- modelGroup("vincristine")

#' Oxaliplatin, intravenous: the model chosen for the patient
#'
#' Bastian 2003 (adults) or Nikanjam 2015 (children and adults), chosen by \code{selectModel()}; see \code{R/modelSelection.R}.
#' @inheritParams oxaliplatinBastian
#' @inheritParams oxaliplatinNikanjam
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
oxaliplatin <- modelGroup("oxaliplatin")

#' Fluorouracil, intravenous: the model chosen for the patient
#'
#' Briki 2026 or Tan 2026 (adults), chosen by \code{selectModel()}; see \code{R/modelSelection.R}.
#' @inheritParams fluorouracilBriki
#' @inheritParams fluorouracilTan
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
fluorouracil <- modelGroup("fluorouracil")

#' Doxorubicin, intravenous: the model chosen for the patient
#'
#' Joerger 2007 (adults), Voller 2015 (children) or Perez-Blanco 2016 (adults
#' with NHL), with the doxorubicinol each forms, chosen by
#' \code{selectModel()}; see \code{R/modelSelection.R}.
#' @inheritParams doxorubicinJoerger
#' @inheritParams doxorubicinPediatric
#' @inheritParams doxorubicinPerezBlanco
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
doxorubicin <- modelGroup("doxorubicin")

#' Doxorubicinol formed from doxorubicin: the model chosen for the patient
#'
#' The doxorubicinol model of the same source as the doxorubicin chosen,
#' by \code{selectModel()}; see \code{R/modelSelection.R}.
#' @inheritParams doxorubicinolJoerger
#' @inheritParams doxorubicinolPediatric
#' @inheritParams doxorubicinolPerezBlanco
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
doxorubicinol <- modelGroup("doxorubicinol")

#' Cyclophosphamide, intravenous: the model chosen for the patient
#'
#' Joerger 2007 (adults) or McCune 2009 (children, with autoinduction),
#' chosen by \code{selectModel()}; see \code{R/modelSelection.R}.
#' @inheritParams cyclophosphamideJoerger
#' @inheritParams cyclophosphamideMcCune
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
cyclophosphamide <- modelGroup("cyclophosphamide")

#' Paclitaxel, intravenous, solvent-based: the model chosen for the patient
#'
#' Crombag 2019 (684 adults, 18-84 years) or Joerger 2006 (168 adults,
#' 33-86 years), chosen by \code{selectModel()}; see \code{R/modelSelection.R}.
#' @inheritParams paclitaxelJoerger
#' @inheritParams paclitaxelCrombag
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
paclitaxel <- modelGroup("paclitaxel")

#' Temozolomide, oral: the model chosen for the patient
#'
#' Ostermann 2004 (35 adults, 30-79 years) or Panetta 2003 (39 patients,
#' 0.7-21.9 years), chosen by \code{selectModel()}; see
#' \code{R/modelSelection.R}.
#' @inheritParams temozolomideOstermann
#' @inheritParams temozolomidePanetta
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
temozolomide <- modelGroup("temozolomide")

#' Quinine, oral: the model chosen for the patient
#'
#' Kloprogge 2014 (22 pregnant women with malaria, 18-37 years) or Le Jouan
#' 2005 (30 children with malaria, 0.55-6.7 years), chosen by
#' \code{selectModel()}; see \code{R/modelSelection.R}.
#' @inheritParams quinineKloprogge
#' @inheritParams quinineLeJouan
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
quinine <- modelGroup("quinine")

#' Sildenafil: the model chosen for the patient
#'
#' The FDA review of NDA 22-473 (45 healthy volunteers; adults, with the Pulmonary
#' hypertension field) or Miura 2026 (children with pulmonary arterial
#' hypertension, 0.4-14.8 years), chosen by \code{selectModel()}; see
#' \code{R/modelSelection.R}.
#' @inheritParams sildenafilFDA
#' @inheritParams sildenafilMiura
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
sildenafil <- modelGroup("sildenafil")

#' Tadalafil: the model chosen for the patient
#'
#' Ferguson-Sells 2022 (324 patients, 2.5-90.3 years; with study LVBF's model
#' without pulmonary hypertension) or Miura 2026 (children with pulmonary
#' arterial hypertension, 0.1-8.3 years), chosen by \code{selectModel()}; see
#' \code{R/modelSelection.R}.
#' @inheritParams tadalafilFergusonSells
#' @inheritParams tadalafilMiura
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
tadalafil <- modelGroup("tadalafil")

#' Ambrisentan: the model chosen for the patient
#'
#' Okour 2023 (39 children, 8-16 years, offered to adults as the nearest) or
#' Miura 2026 (children with pulmonary arterial hypertension, 0.4-8.8
#' years), chosen by \code{selectModel()}; see \code{R/modelSelection.R}.
#' @inheritParams ambrisentanOkour
#' @inheritParams ambrisentanMiura
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
ambrisentan <- modelGroup("ambrisentan")

#' Landiolol: the model chosen for the patient
#'
#' Honda 2008 (47 healthy men, 20-33 years), Kunisawa 2015 (9 women having
#' gynaecologic surgery, 37-71 years) or Kunisawa 2015 (8 patients with
#' peripheral arterial disease, 64-84 years), chosen by \code{selectModel()};
#' see \code{R/modelSelection.R} and \code{R/landiolol.R}.
#' @inheritParams landiololHonda
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
landiolol <- modelGroup("landiolol")

#' Ciprofol: the model chosen for the patient
#'
#' Liu 2024 (462 adults, 18-77 years) or Wang 2026 (25 children, 1-9 years),
#' chosen by \code{selectModel()}; see \code{R/modelSelection.R} and
#' \code{R/ciprofol.R}.
#' @inheritParams ciprofolLiu
#' @param age age in years; it chooses the model, and Liu's V1 age term is held
#'   at 18 below that age
#' @param adjustToFFM \code{TRUE} (the default) scales the chosen model by
#'   fat-free mass (for Liu, the clearance's weight term at the adjusted weight
#'   and the other parameters anchored at Liu's cohort; for Wang, the per-kg
#'   values on the adjusted weight); \code{FALSE} uses the published values.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
ciprofol <- modelGroup("ciprofol")

#' Antithrombin III (human), Thrombate III: the model chosen for the patient
#'
#' Moffett 2017 (184 children under 19 years) or Ilias 2000 (32 adults with
#' severe sepsis, 18-85 years), chosen by \code{selectModel()}; see
#' \code{R/modelSelection.R}.
#' @inheritParams antithrombinMoffett
#' @inheritParams antithrombinIlias
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
antithrombin <- modelGroup("antithrombin")

#' Micafungin: the model chosen for the patient
#'
#' Jullien 2017 (99 ventilated ICU patients, 29.9-92.7 years, 48-141 kg) or
#' Wasmann 2019 (24 normal-weight and morbidly obese adults, 22-61 years,
#' 61.5-184 kg), chosen by \code{selectModel()}; see \code{R/modelSelection.R}.
#' @inheritParams micafunginJullien
#' @inheritParams micafunginWasmann
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
micafungin <- modelGroup("micafungin")

#' Teicoplanin: the model chosen for the patient
#'
#' Kim 2024 (12 healthy adults, 19-55 years; every adult, by the nearest
#' rule above 55) or Zhang 2020 (159 children, 0.2-14 years, 2.9-69 kg),
#' chosen by \code{selectModel()}; see \code{R/modelSelection.R}.
#' @inheritParams teicoplaninKim
#' @inheritParams teicoplaninZhang
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
teicoplanin <- modelGroup("teicoplanin")

#' Caspofungin: the model chosen for the patient
#'
#' Wurthwein 2013 (46 adults, 18-74 years, 43-104 kg) or Xu 2026 (29
#' critically ill children, 0.33-16 years, 4.9-74 kg), chosen by
#' \code{selectModel()}; see \code{R/modelSelection.R}.
#' @inheritParams caspofunginWurthwein
#' @inheritParams caspofunginXu
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
caspofungin <- modelGroup("caspofungin")
