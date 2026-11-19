# Tolerance and tachyphylaxis: pharmacodynamic adaptation
#
# Steven L. Shafer, 2026-11-18: "Many ICU drugs are characterized by
# tachyphylaxis. All opioids are characterized by tolerance. ... They do not
# affect pharmacokinetics. They affect pharmacodynamics."  And, on how to show
# it, the same day: like wind chill, the concentration is what the PK says but
# the drug "works like" less; and, the form he chose, "another graph that
# starts at 100% and declines over time, labeled (for example) 'vasopressin
# tachyphylaxis'", offered as a Graph Option only while a drug with possible
# tachyphylaxis is plotted.
#
# So nothing here touches the engines.  Each drug with a row in
# inst/extdata/pdAdaptation.csv whose `verification` is one that is applied
# (PD_ADAPTATION_APPLIED_VERIFICATION) has an adaptation state driven by its
# own simulated concentration, read off the series the plot already has, and
# the panel draws, as wind chill is reported, the concentration the drug
# works like as a percentage of the concentration present:
#
#   remaining(t) = 100 / P(t)   for a loss of POTENCY (tolerance: the EC50,
#                               and so the MEAC, rises by the factor P), or
#   remaining(t) = 100 * G(t)   for a loss of EFFICACY (an attenuation factor
#                               multiplying the effect).
#
# For a potency loss this is exactly the wind-chill ratio: the concentration
# that would give today's effect in a naive patient ("works like") is C / P,
# and C / P as a share of C is 100 / P.  For an efficacy loss with a linear
# concentration-effect relation (Chow 1985's cocaine) it is exact as well.
#
# The structures (`structure` in the CSV):
#
#   compartment   A hypothetical tolerance compartment (Porchet, Benowitz and
#                 Sheiner 1988; Ekblom 1993), filled from the driving
#                 concentration C (the effect site, or plasma where the source
#                 drives it from plasma) at first order:
#                   dCt/dt = k (C - Ct),  Ct(0) = 0
#                 and P = 1 + (Ct / c50)^hill when `max_fold` is blank (hill 1
#                 is Porchet's EC50 x (1 + Ct/Ka)), or
#                 P = 1 + (max_fold - 1) Ct^hill / (c50^hill + Ct^hill), a
#                 tolerance that saturates at max_fold.  It develops with
#                 exposure and recovers as the drug washes out.
#   competitive   The same compartment, competing with the drug for its
#                 receptor (Ouellet and Pollack 1995, morphine: a partial
#                 agonist of high affinity whose intrinsic activity, 5% of
#                 the maximum, is neglected): with the drug's Hill
#                 coefficient as `hill`, the EC50 rises by
#                 P = (1 + (Ct / c50)^hill)^(1 / hill).  Onset is fast where
#                 C is far above c50 and recovery slow, from one k.
#   attenuation   An attenuation factor G that starts at 1 when the drug is
#                 first present and falls at first order, k, towards
#                 `floor` (0 when blank): G = floor + (1 - floor) exp(-k t'),
#                 t' the time since C first exceeded 0 (Chow, Ambre, Ruo et
#                 al. 1985, cocaine).  It does not recover in the source.
#
# Ct is computed EXACTLY for a driving concentration that is linear between
# the simulated points (simulationTimeGrid() puts the points close where the
# curve bends): over a step of length h with C going from C0 by slope s,
#   Ct(h) = C(h) - s/k + (Ct0 - C0 + s/k) exp(-k h).
# It is an approximation only in reading C as linear between points, the same
# reading the plot itself draws.  The driving concentration is converted to
# mcg/mL (mcgPerMlScale()), the unit the CSV's c50 is in, so a Drug Library
# switch between mcg/mL and ng/mL changes nothing.
#
# From animals to humans (Shafer, 2026-11-19): "The tolerance models will
# primarily be from animals ... formal studies in patients can't be
# performed.  Opioid tolerance can be scaled among opioids based on potency
# ... look at key animal studies, understand the time course, and then scale
# to humans."  So a row may come from another species and stand for a whole
# class, and pdAdaptationRow() resolves it for the drug drawn:
#
#   time    A row with `weight_kg` (the studied animals' body weight) has its
#           rate constant moved to a 70 kg human in physiological time:
#           durations scale as body weight^b, b the row's `time_exponent` or
#           PD_ADAPTATION_TIME_EXPONENT (0.25), so k_human = k (weight_kg/70)^b.
#   potency A row with `c50_per_ec50` gives the tolerance c50 as a multiple of
#           the studied drug's own EC50 in the same study, so species and drug
#           potency drop out; for the drug drawn it becomes that multiple of
#           the drug's MEAC in the Drug Library, the human potency anchor.
#   class   A row with `scale_to` (a Drug Library Category, "Opioids") applies
#           to every drug of that category without a row of its own, by
#           potency as above; a drug with no MEAC (codeine, tramadol,
#           buprenorphine) gets none.

#' Verification values whose rows are applied
#' @noRd
PD_ADAPTATION_APPLIED_VERIFICATION <- c("read_full_text", "read_abstract")

#' The structures the panel can compute
#' @noRd
PD_ADAPTATION_STRUCTURES <- c("compartment", "competitive", "attenuation")

#' The panel's units, shown under its title
#'
#' A loss of potency (the compartment structures) draws 100 / P, the
#' concentration the drug works like as a share of the concentration
#' present: the share of its naive potency left, not of its naive effect,
#' which depends on where the concentration sits on the drug's curve.
#' An attenuation multiplies the effect itself.
#' @noRd
PD_ADAPTATION_UNITS <- c(potency = "% of naive potency", effect = "% of naive effect")

#' The units of a row's panel (PD_ADAPTATION_UNITS)
#' @param row an adaptation row (pdAdaptationRow())
#' @noRd
pdAdaptationUnits <- function(row)
{
  if (identical(as.character(row$structure), "attenuation"))
    PD_ADAPTATION_UNITS[["effect"]]
  else
    PD_ADAPTATION_UNITS[["potency"]]
}

.pdAdaptationCache <- new.env(parent = emptyenv())

#' The PD adaptation table, inst/extdata/pdAdaptation.csv
#' @noRd
pdAdaptationTable <- function()
{
  if (is.null(.pdAdaptationCache$table))
  {
    path <- system.file("extdata", "pdAdaptation.csv", package = "stanpumpR")
    .pdAdaptationCache$table <- utils::read.csv(path, stringsAsFactors = FALSE,
                                                na.strings = c("", "NA"),
                                                colClasses = c(scale_to = "character"))
  }
  .pdAdaptationCache$table
}

#' The allometric exponent for durations (physiological time)
#'
#' Durations scale as body weight^0.25 across mammals (Lindstedt and Calder
#' 1981; Boxenbaum 1982): a rat's minute is a human's few.
#' @noRd
PD_ADAPTATION_TIME_EXPONENT <- 0.25

#' The human an animal row is scaled to, kg
#' @noRd
PD_ADAPTATION_HUMAN_KG <- 70

#' The factor a source's durations are multiplied by in a 70 kg human
#'
#' The exponent is 0.25 unless the row gives its own (`time_exponent`): 0.25
#' is well founded for times set by transport through the body (heart beat,
#' breath, half-lives; ke0 for buprenorphine, Yassen 2007), less so for
#' processes inside the cell, receptor desensitization among them, which
#' cultured cells run at one rate whatever the animal's size (West 2002).
#' Kissin's own rat and human acute-tolerance times (81-88 min with
#' alfentanil, Kissin 1996; 129-138 min with remifentanil, Vinik and Kissin
#' 1998) would imply an exponent nearer 0.1.
#'
#' [decision] Shafer, 2026-11-19: the opioid class row (Ouellet and Pollack
#' 1995) keeps 0.25, so that it matches the volunteer studies that found no
#' acute tolerance (Gustorff 2002, Angst 2009) rather than Vinik and Kissin
#' 1998. Do not change it without his decision.
#'
#' @param weightKg the studied animals' body weight, kg (NA: human)
#' @param exponent the row's exponent (NA: PD_ADAPTATION_TIME_EXPONENT)
#' @return (70 / weightKg)^exponent, 1 for a human row
#' @noRd
pdAdaptationTimeScale <- function(weightKg, exponent = NA)
{
  w <- suppressWarnings(as.numeric(weightKg))
  b <- suppressWarnings(as.numeric(exponent))
  b <- ifelse(is.na(b), PD_ADAPTATION_TIME_EXPONENT, b)
  ifelse(is.na(w), 1, (PD_ADAPTATION_HUMAN_KG / w)^b)
}

#' A drug's MEAC in mcg/mL, NA when it has none
#' @noRd
pdAdaptationMeac <- function(drug, drugDefaults)
{
  r <- drugDefaults[drugDefaults$Drug %in% drug, , drop = FALSE]
  if (nrow(r) == 0) return(NA_real_)
  m <- suppressWarnings(as.numeric(r$MEAC[1]))
  if (is.na(m) || m <= 0) return(NA_real_)
  m * mcgPerMlScale(r$Concentration.Units[1])
}

#' A table row made concrete for one drug in a 70 kg human
#'
#' Moves an animal row's rate constant to human physiological time and turns
#' a c50 given relative to the source's EC50 into mcg/mL by the drug's MEAC.
#'
#' @param row one row of the table
#' @param drug the drug drawn
#' @param drugDefaults the Drug Library
#' @return the row, with `Drug` the drug drawn, `source_drug` the drug
#'   studied, `k_per_min` and `c50_mcg_per_ml` in human terms, and
#'   `time_scale`; NULL when the drug has no potency anchor the row needs
#' @noRd
pdAdaptationResolve <- function(row, drug, drugDefaults)
{
  out <- row
  out$source_drug <- row$Drug
  out$Drug <- drug
  out$time_scale <- pdAdaptationTimeScale(row$weight_kg, row$time_exponent)
  out$k_per_min <- as.numeric(row$k_per_min) / out$time_scale
  if (!is.na(row$c50_per_ec50))
  {
    meac <- pdAdaptationMeac(drug, drugDefaults)
    if (is.na(meac)) return(NULL)
    out$c50_mcg_per_ml <- as.numeric(row$c50_per_ec50) * meac
  }
  out
}

#' A drug's applied adaptation row, in human terms, or NULL
#'
#' The drug's own applied row if it has one, otherwise the first applied row
#' whose `scale_to` is the drug's Drug Library category, resolved for it
#' (pdAdaptationResolve()).
#'
#' @param drug a drug name
#' @param table the adaptation table (pdAdaptationTable())
#' @param drugDefaults the Drug Library
#' @return a one-row data frame, or NULL when nothing applies to the drug
#' @noRd
pdAdaptationRow <- function(drug, table = pdAdaptationTable(),
                            drugDefaults = getDrugDefaultsGlobal())
{
  applied <- table[table$verification %in% PD_ADAPTATION_APPLIED_VERIFICATION, , drop = FALSE]
  own <- applied[applied$Drug %in% drug, , drop = FALSE]
  if (nrow(own) > 0) return(pdAdaptationResolve(own[1, , drop = FALSE], drug, drugDefaults))
  category <- drugDefaults$Category[drugDefaults$Drug %in% drug]
  if (length(category) == 0 || is.na(category[1]) || !nzchar(category[1])) return(NULL)
  scaled <- applied[!is.na(applied$scale_to) & applied$scale_to == category[1], , drop = FALSE]
  if (nrow(scaled) == 0) return(NULL)
  pdAdaptationResolve(scaled[1, , drop = FALSE], drug, drugDefaults)
}

#' The drugs, among those given, that have an applied adaptation row
#' @noRd
pdAdaptationDrugs <- function(drugs, table = pdAdaptationTable(),
                              drugDefaults = getDrugDefaultsGlobal())
{
  drugs <- unique(as.character(drugs))
  drugs <- drugs[!is.na(drugs) & nzchar(drugs)]
  Filter(function(d) !is.null(pdAdaptationRow(d, table, drugDefaults)), drugs)
}

#' The tolerance compartment, exactly, for a driver linear between points
#'
#' @param time increasing times (minutes; repeats allowed, a jump)
#' @param conc the driving concentration at those times
#' @param k the rate constant, 1/min
#' @return Ct at each time, from Ct = 0 before the first
#' @noRd
pdToleranceCompartment <- function(time, conc, k)
{
  n <- length(time)
  ct <- numeric(n)
  if (n == 0) return(ct)
  conc[!is.finite(conc)] <- 0
  # The state starts at zero at the first point, which is time 0 for every
  # simulation; a drug already present then would have had no time to
  # adapt.
  for (i in seq_len(n)[-1])
  {
    h <- time[i] - time[i - 1]
    if (h <= 0) { ct[i] <- ct[i - 1]; next }
    s <- (conc[i] - conc[i - 1]) / h
    e <- exp(-k * h)
    ct[i] <- conc[i] - s / k + (ct[i - 1] - conc[i - 1] + s / k) * e
  }
  pmax(ct, 0)
}

#' The share of the naive potency (or, for an attenuation, effect) left, in percent
#'
#' @param time times, minutes, increasing
#' @param conc the driving concentration in mcg/mL at those times
#' @param row the drug's adaptation row (pdAdaptationRow())
#' @return the percentage at each time, 100 before any adaptation
#' @noRd
pdAdaptationRemaining <- function(time, conc, row)
{
  k <- as.numeric(row$k_per_min)
  stopifnot(is.finite(k), k > 0)
  form <- as.character(row$structure)
  if (form == "compartment")
  {
    c50  <- as.numeric(row$c50_mcg_per_ml)
    hill <- if (is.na(row$hill)) 1 else as.numeric(row$hill)
    maxFold <- as.numeric(row$max_fold)
    stopifnot(is.finite(c50), c50 > 0, is.finite(hill), hill > 0)
    ct <- pdToleranceCompartment(time, conc, k)
    x <- (ct / c50)^hill
    p <- if (is.na(maxFold)) 1 + x else 1 + (maxFold - 1) * x / (1 + x)
    return(100 / p)
  }
  if (form == "competitive")
  {
    # Ouellet and Pollack 1995: the tolerance compartment competes with the
    # drug at its receptor under the drug's own Hill coefficient, so the
    # EC50 rises by (1 + (Ct/c50)^hill)^(1/hill), linear in Ct once Ct is
    # well above c50
    c50  <- as.numeric(row$c50_mcg_per_ml)
    hill <- if (is.na(row$hill)) 1 else as.numeric(row$hill)
    stopifnot(is.finite(c50), c50 > 0, is.finite(hill), hill > 0)
    ct <- pdToleranceCompartment(time, conc, k)
    p <- (1 + (ct / c50)^hill)^(1 / hill)
    return(100 / p)
  }
  if (form == "attenuation")
  {
    lowest <- if (is.na(row$floor)) 0 else as.numeric(row$floor)
    present <- which(conc > 0)
    if (length(present) == 0) return(rep(100, length(time)))
    since <- pmax(time - time[present[1]], 0)
    return(100 * (lowest + (1 - lowest) * exp(-k * since)))
  }
  stop("Unknown PD adaptation structure: ", form, call. = FALSE)
}

#' A drug's adaptation series from its simulated entry
#'
#' Reads the driving concentration (`driver`: "Effect Site" or "Plasma") off
#' the entry's own full series, the one the plot draws, so no simulation is
#' added.  A drug with no effect site (a ke0 of zero) is driven by plasma.
#'
#' @param entry the drug's element of drugs()
#' @param row its adaptation row
#' @return data frame Time, Y (percent), or NULL when there is nothing to read
#' @noRd
pdAdaptationSeries <- function(entry, row)
{
  results <- entry$results
  if (is.null(results) || nrow(results) == 0) return(NULL)
  driver <- if (is.na(row$driver)) "Effect Site" else as.character(row$driver)
  r <- results[results$Site == driver & !is.na(results$Y), c("Time", "Y")]
  if (nrow(r) < 2 && driver == "Effect Site")
    r <- results[results$Site == "Plasma" & !is.na(results$Y), c("Time", "Y")]
  if (nrow(r) < 2) return(NULL)
  r <- r[order(r$Time), ]
  conc <- pmax(r$Y, 0) * mcgPerMlScale(entry$Concentration.Units)
  data.frame(Time = r$Time, Y = pdAdaptationRemaining(r$Time, conc, row))
}

#' The panel's series name for a drug ("<drug> tachyphylaxis")
#' @noRd
pdAdaptationName <- function(drug, row)
{
  paste(drug, if (identical(as.character(row$kind), "tolerance")) "tolerance" else "tachyphylaxis")
}

#' Split an adaptation series name into its drug
#'
#' @param series series names, e.g. "nitroglycerin tachyphylaxis"
#' @return the drug for each, NA where the name is not an adaptation panel's
#' @noRd
pdAdaptationSeriesDrug <- function(series, drugDefaults = getDrugDefaultsGlobal())
{
  pattern <- "^(.*) (tolerance|tachyphylaxis)$"
  hit <- grepl(pattern, series)
  drug <- ifelse(hit, sub(pattern, "\\1", series), NA_character_)
  # Only a Drug Library drug: "<x> tolerance" is otherwise someone's name.
  # (A class row draws a panel for drugs that have no row of their own.)
  ok <- !is.na(drug) & drug %in% c(drugDefaults$Drug, pdAdaptationTable()$Drug)
  ifelse(ok, drug, NA_character_)
}

#' The generated help block `pd-adaptation-table`
#'
#' Every row of inst/extdata/pdAdaptation.csv, applied rows first, with its
#' structure in words, so the help page cannot drift from the table.
#' @noRd
helpPdAdaptationTableMarkdown <- function()
{
  tab <- pdAdaptationTable()
  if (nrow(tab) == 0) return("*No drug has a tolerance or tachyphylaxis model yet.*")
  applied <- tab$verification %in% PD_ADAPTATION_APPLIED_VERIFICATION
  tab <- tab[order(!applied, tab$Drug), ]
  applied <- tab$verification %in% PD_ADAPTATION_APPLIED_VERIFICATION
  dash <- intToUtf8(0x2014)
  num <- function(x) ifelse(is.na(x), dash, formatC(as.numeric(x), digits = 3, format = "fg"))
  cell <- function(x) gsub("|", "\\|", ifelse(is.na(x), "", x), fixed = TRUE)
  # The half-time in the source's species, and in a 70 kg human
  hours <- function(k) formatC(log(2) / k / 60, digits = 3, format = "fg")
  scale <- pdAdaptationTimeScale(tab$weight_kg, tab$time_exponent)
  animal <- !is.na(tab$species) & tab$species != "human"
  halfTime <- ifelse(is.na(tab$k_per_min), dash, ifelse(
    animal & is.na(tab$weight_kg),
    paste0("not scaled (", hours(tab$k_per_min), " in the ", tab$species, ", weight not known)"),
    ifelse(scale == 1, hours(tab$k_per_min),
    paste0(hours(tab$k_per_min / scale), " (", hours(tab$k_per_min), " in the ",
           tab$species, ", ", num(tab$weight_kg), " kg)"))))
  c50Text <- ifelse(!is.na(tab$c50_per_ec50),
                    paste0(num(tab$c50_per_ec50), " x the EC50 (MEAC in humans)"),
                    paste0(num(tab$c50_mcg_per_ml), " mcg/mL"))
  scope <- ifelse(is.na(tab$scale_to), "",
                  paste0("; applies to every drug of ", tab$scale_to,
                         " with a MEAC, by its potency"))
  model <- ifelse(
    is.na(tab$structure), "no model yet", ifelse(
    tab$structure %in% c("compartment", "competitive"),
    paste0(ifelse(tab$structure == "competitive", "competitive ", ""),
           "tolerance compartment from ", ifelse(is.na(tab$driver), "Effect Site", tab$driver),
           "; c50 ", c50Text, scope,
           ifelse(is.na(tab$hill) | tab$hill == 1, "", paste0(", hill ", num(tab$hill))),
           ifelse(is.na(tab$max_fold), "", paste0(", at most ", num(tab$max_fold), "-fold"))),
    paste0("attenuation from first exposure",
           ifelse(is.na(tab$floor), "", paste0(", to ", num(100 * tab$floor), "%"))))
  )
  model <- paste0(model, ifelse(is.na(tab$notes), "", paste0(". ", tab$notes)))
  pmid <- ifelse(is.na(tab$pmid), "",
                 sprintf(" ([PMID %s](https://pubmed.ncbi.nlm.nih.gov/%s/))", tab$pmid, tab$pmid))
  rows <- sprintf(
    "| [%s](help:drugs/%s) | %s | %s | %s | %s | %s | %s%s |",
    tab$Drug, tab$Drug, tab$kind, cell(model), halfTime, cell(tab$species),
    ifelse(applied, "yes", "not yet"), cell(tab$source), pmid
  )
  paste(c(
    "| Drug | Kind | Model | Half-time in a 70 kg human (h) | Species | Applied | Source |",
    "|---|---|---|---|---|---|---|",
    rows
  ), collapse = "\n")
}
