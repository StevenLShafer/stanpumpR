# % effect: a general pharmacodynamic endpoint, 0 to 100% of maximal effect
#
# Steven L. Shafer, 2026-11-20, on irbesartan's blood pressure: plot the
# effect as a percentage of the maximal effect, 0 to 100% on a linear axis,
# so the plot does not depend on the patient's baseline; and then "I think
# that %effect will likely be a pharmacodynamic endpoint for many drugs.
# Let's create it now, and make it generally available."
#
# So any drug may declare one or more endpoints in inst/extdata/pdEffect.csv,
# one row each, and Graph Options -> % effect draws, under each drawn drug
# with a DRAWN row (`role` "drawn", `verification` one that is applied), a
# panel named "<drug> <endpoint>" in "% of maximal effect":
#
#   E(t) = 100 x C(t)^gamma / (EC50^gamma + C(t)^gamma)
#
# the sigmoid Emax model as a share of its own Emax.  The baseline and the
# Emax in the source's units (mmHg, say) drop out; the row keeps them
# (`emax`, `emax_units`) only for the help page, so a reader can turn a
# percentage back into the source's units.
#
# The driving concentration C, by the row's `driver` and `ke0_per_min`:
#
#   Plasma, ke0 given   an effect site OF THE ENDPOINT'S OWN, filled from the
#                       plasma curve at first order: dCe/dt = ke0 (Cp - Ce).
#                       This is how a PD model fitted with its own ke0 is
#                       carried onto a drug whose Drug Library entry is
#                       plasma only (irbesartan) or whose effect site belongs
#                       to another endpoint.  Ce is computed EXACTLY for a
#                       plasma curve linear between the simulated points, by
#                       the same step as the tolerance compartment
#                       (pdToleranceCompartment(), R/pdAdaptation.R): the two
#                       are one linear first-order lag.
#   Plasma, no ke0      the effect follows plasma directly.
#   Effect Site         the drug's own effect site, the one its panel draws.
#                       A drug with no effect site has no panel from such a
#                       row.
#
# Nothing here touches the engines: the panel reads the series the plot
# already has, as the tolerance panel does.  EC50 is in mcg/mL
# (`ec50_mcg_per_ml`) and the drug's concentration is converted to mcg/mL
# (mcgPerMlScale()), so a Drug Library switch between mcg/mL and ng/mL
# changes nothing.
#
#
# WHY NO DRUG CARRIES AN EEG-INDEX ENDPOINT (Steven L. Shafer, 2026-11-26)
#
# Several papers digested on 2026-11-25/26 report propofol Ce50 values against
# BIS, the A-line ARX index and the spectral edge, and they raised the obvious
# question: should propofol declare an EEG endpoint here?  It should NOT, and
# the reason is about what the EEG measures, not about the numbers.
#
# The EEG is not solely a measure of drug effect.  On emergence it is nearly
# unchanged for several minutes after the drug is stopped; then the patient
# opens their eyes and it changes INSTANTLY, showing brain arousal with the
# return of consciousness.  What the monitor reports is a combination of drug
# effect and brain state, so a sigmoid Emax model of concentration against
# index would attribute to the drug a change the brain made.
#
# Nor do the drugs act on it in one direction, which matters because clinical
# anaesthesia combines them.  Ketamine and midazolam ACTIVATE the EEG.  Opioids
# do little to it except in huge doses, where they DEPRESS it -- the basis of
# the Stanford opioid experiments that determined their ke0.  The hypnotics,
# propofol and the inhaled agents and etomidate, ACTIVATE and then DEPRESS it.
# A per-drug % effect panel would draw each of these as a monotone function of
# that drug's own concentration, which the index is not.
#
# This settles propofol.  Whether any EEG endpoint belongs in stanpumpR at all
# is QUEUED for a future session: do not add one without his decision.  The
# cards that raised it are papers/digests/PMID_15220769.md, PMID_16129974.md
# and PMID_23157660.md.
#
# A drug may carry several rows for one endpoint, one per published model.
# The first applied row with role "drawn" is drawn; rows with role
# "alternative" are listed on the help page so the disagreement between
# sources stays in view.  A row may be marked `provisional`, which the panel
# title does not show but the help page and the drug's help page do.

#' Verification values whose rows are applied
#' @noRd
PD_EFFECT_APPLIED_VERIFICATION <- c("read_full_text", "read_abstract")

#' The drivers a row may name
#' @noRd
PD_EFFECT_DRIVERS <- c("Plasma", "Effect Site")

#' The panel's units, shown under its title
#' @noRd
PD_EFFECT_UNITS <- "% of maximal effect"

.pdEffectCache <- new.env(parent = emptyenv())

#' The % effect table, inst/extdata/pdEffect.csv
#' @noRd
pdEffectTable <- function()
{
  if (is.null(.pdEffectCache$table))
  {
    path <- system.file("extdata", "pdEffect.csv", package = "stanpumpR")
    .pdEffectCache$table <- utils::read.csv(
      path, stringsAsFactors = FALSE, na.strings = c("", "NA"),
      colClasses = c(pmid = "character", doi = "character"))
  }
  .pdEffectCache$table
}

#' A drug's drawn % effect rows, one per endpoint
#'
#' @param drug a drug name
#' @param table the % effect table (pdEffectTable())
#' @return the drawn rows (role "drawn", verification applied), at most one
#'   per endpoint, the first in the table's order; zero rows when none
#' @noRd
pdEffectRows <- function(drug, table = pdEffectTable())
{
  rows <- table[table$Drug %in% drug &
                table$role %in% "drawn" &
                table$verification %in% PD_EFFECT_APPLIED_VERIFICATION, , drop = FALSE]
  rows[!duplicated(rows$endpoint), , drop = FALSE]
}

#' The drugs, among those given, with a drawn % effect row
#' @noRd
pdEffectDrugs <- function(drugs, table = pdEffectTable())
{
  drugs <- unique(as.character(drugs))
  drugs <- drugs[!is.na(drugs) & nzchar(drugs)]
  drugs[vapply(drugs, function(d) nrow(pdEffectRows(d, table)) > 0, logical(1))]
}

#' The sigmoid Emax model as a percentage of Emax
#'
#' @param conc concentrations, mcg/mL (negative or missing read as 0)
#' @param ec50 the concentration at half the maximal effect, mcg/mL
#' @param gamma the Hill coefficient
#' @return 100 C^gamma / (EC50^gamma + C^gamma)
#' @noRd
pdEffectPercent <- function(conc, ec50, gamma)
{
  stopifnot(is.finite(ec50), ec50 > 0, is.finite(gamma), gamma > 0)
  conc[!is.finite(conc)] <- 0
  # (C/EC50)^gamma / (1 + (C/EC50)^gamma), written so a large C gives 100,
  # not Inf / Inf
  x <- pmax(conc, 0) / ec50
  100 / (1 + x^(-gamma))
}

#' The concentration that drives a row's effect, from a drug's results
#'
#' @param results the drug's results (Time, Y, Site)
#' @param units the drug's Concentration.Units
#' @param row one % effect row
#' @return data frame Time, C (mcg/mL), or NULL when there is nothing to read
#' @noRd
pdEffectDriver <- function(results, units, row)
{
  if (is.null(results) || nrow(results) == 0) return(NULL)
  driver <- if (is.na(row$driver)) "Plasma" else as.character(row$driver)
  if (!driver %in% PD_EFFECT_DRIVERS)
    stop("Unknown % effect driver: ", driver, call. = FALSE)
  r <- results[results$Site == driver & !is.na(results$Y), c("Time", "Y")]
  if (nrow(r) < 2) return(NULL)
  r <- r[order(r$Time), ]
  conc <- pmax(r$Y, 0) * mcgPerMlScale(units)
  ke0 <- suppressWarnings(as.numeric(row$ke0_per_min))
  # The endpoint's own effect site, from plasma (see the header); one linear
  # first-order lag, the same exact step as the tolerance compartment
  if (driver == "Plasma" && !is.na(ke0))
  {
    stopifnot(ke0 > 0)
    conc <- pdToleranceCompartment(r$Time, conc, ke0)
  }
  data.frame(Time = r$Time, C = conc)
}

#' A drug's % effect series for one row, from its simulated entry
#'
#' @param entry the drug's element of drugs()
#' @param row one % effect row
#' @return data frame Time, Y (% of maximal effect), C (the driving
#'   concentration, mcg/mL), or NULL when there is nothing to read
#' @noRd
pdEffectSeries <- function(entry, row)
{
  d <- pdEffectDriver(entry$results, entry$Concentration.Units, row)
  if (is.null(d)) return(NULL)
  data.frame(Time = d$Time,
             Y = pdEffectPercent(d$C, as.numeric(row$ec50_mcg_per_ml), as.numeric(row$gamma)),
             C = d$C)
}

#' The panel's series name ("<drug> <endpoint>")
#' @noRd
pdEffectName <- function(drug, row)
{
  paste(drug, row$endpoint)
}

#' The % effect panel names for a set of drugs, for the panel order
#'
#' One vector per endpoint slot, each as long as `drugs`: the k-th drawn
#' endpoint's panel name for each drug, NA for a drug with fewer, so that
#' simulationPlot() can interleave them with each drug's other panels.
#'
#' @param drugs drug names (the Drug Library's)
#' @return a list of character vectors, at least one
#' @noRd
pdEffectPanelNames <- function(drugs, table = pdEffectTable())
{
  drawn <- table[table$role %in% "drawn" &
                 table$verification %in% PD_EFFECT_APPLIED_VERIFICATION, , drop = FALSE]
  drawn <- drawn[!duplicated(paste(drawn$Drug, drawn$endpoint, sep = "\r")), , drop = FALSE]
  slot <- stats::ave(seq_len(nrow(drawn)), drawn$Drug, FUN = seq_along)
  lapply(seq_len(max(c(slot, 1))), function(k) {
    mine <- drawn[slot == k, , drop = FALSE]
    paste(drugs, mine$endpoint[match(drugs, mine$Drug)])[seq_along(drugs)] |>
      replace(!(drugs %in% mine$Drug), NA_character_)
  })
}

#' Split a % effect series name into its drug and row
#'
#' Matched exactly against the drawn rows, so a drug whose name merely ends
#' in an endpoint's words is not mistaken for a panel.
#'
#' @param series series names, e.g. "irbesartan SBP fall"
#' @return the drug for each, NA where the name is not a % effect panel's
#' @noRd
pdEffectSeriesDrug <- function(series, table = pdEffectTable())
{
  drawn <- table[table$role %in% "drawn" &
                 table$verification %in% PD_EFFECT_APPLIED_VERIFICATION, , drop = FALSE]
  names <- paste(drawn$Drug, drawn$endpoint)
  drawn$Drug[match(as.character(series), names)]
}

#' The % effect row behind a panel's series name, or NULL
#' @noRd
pdEffectSeriesRow <- function(series, table = pdEffectTable())
{
  drug <- pdEffectSeriesDrug(series, table)
  if (is.na(drug)) return(NULL)
  rows <- pdEffectRows(drug, table)
  rows[paste(rows$Drug, rows$endpoint) == series, , drop = FALSE][1, , drop = FALSE]
}

#' The generated help block `pd-effect-table`
#'
#' Every row of inst/extdata/pdEffect.csv, drawn rows first, so the help page
#' cannot drift from the table.
#' @noRd
helpPdEffectTableMarkdown <- function()
{
  tab <- pdEffectTable()
  if (nrow(tab) == 0) return("*No drug has a % effect model yet.*")
  # A row is drawn when it is the first applied "drawn" row for its drug and
  # endpoint (pdEffectRows()); decided in the table's own order, then sorted
  applied <- tab$verification %in% PD_EFFECT_APPLIED_VERIFICATION
  candidate <- applied & tab$role %in% "drawn"
  key <- paste(tab$Drug, tab$endpoint, sep = "\r")
  drawn <- candidate & !duplicated(ifelse(candidate, key, NA), incomparables = NA)
  ord <- order(tab$Drug, !drawn, seq_len(nrow(tab)))
  tab <- tab[ord, ]
  applied <- applied[ord]
  drawn <- drawn[ord]
  dash <- intToUtf8(0x2014)
  num <- function(x) ifelse(is.na(x), dash, formatC(as.numeric(x), digits = 3, format = "fg"))
  cell <- function(x) gsub("|", "\\|", ifelse(is.na(x), "", x), fixed = TRUE)
  ke0 <- suppressWarnings(as.numeric(tab$ke0_per_min))
  site <- ifelse(tab$driver %in% "Effect Site", "the drug's own effect site",
          ifelse(is.na(ke0), "plasma, directly",
                 paste0("its own effect site, ke0 ", num(ke0 * 60), " /h (t\u00bd ",
                        num(log(2) / ke0), " min)")))
  emax <- ifelse(is.na(tab$emax), dash, paste(num(tab$emax), ifelse(is.na(tab$emax_units), "", tab$emax_units)))
  status <- ifelse(drawn, ifelse(tab$provisional %in% TRUE, "drawn, provisional", "drawn"),
            ifelse(applied, "alternative", "not yet"))
  pmid <- ifelse(is.na(tab$pmid), "",
                 sprintf(" ([PMID %s](https://pubmed.ncbi.nlm.nih.gov/%s/))", tab$pmid, tab$pmid))
  rows <- sprintf(
    "| [%s](help:drugs/%s) | %s | %s | %s | %s | %s | %s | %s | %s%s |",
    tab$Drug, tab$Drug, cell(tab$endpoint), cell(site), num(tab$ec50_mcg_per_ml),
    num(tab$gamma), cell(emax), cell(tab$population), status, cell(tab$source), pmid
  )
  paste(c(
    "| Drug | Endpoint | Driven by | EC50 (mcg/mL) | Hill | Emax (source's units) | Population | Status | Source |",
    "|---|---|---|---|---|---|---|---|---|",
    rows
  ), collapse = "\n")
}
