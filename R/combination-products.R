# -----------------------------------------------------------------------------
# Combination products: one dose-table entry, drawn as its components
# -----------------------------------------------------------------------------
# Steven L. Shafer, 2026-11-24: "For Coartem, both components are active.
# Both should appear. However, for combination products where one of the
# moieties exists solely to change the clinical efficacy of the other
# moiety, only show the clinically active moiety." and, for Coartem, "draw
# both. Build it with Coartem."
#
# A product whose components are each a drug with an effect of its own is
# entered in the dose table under the product's name, in the product's own
# unit (Coartem: tablets of 20 mg artemether and 120 mg lumefantrine), and
# simulated and drawn as its components, each from its own library model, in
# its own panel.  A product with only one active component (Zosyn, Paxlovid)
# needs none of this: it is one drug whose source's model carries the effect
# of the other moiety (docs/library-expansion/README.md, "Which moieties a
# combination product draws").
#
# Where the rows are rewritten.  The committed dose table keeps the product,
# because that is what the user typed: the dose grid, Edit doses, Stop, Skip
# next dose, the export's dose sheet and the bookmark all read it.  What the
# simulation reads is rewritten into one row per component by
# expandCombinationProducts(): in the app at doseTableIV() (R/app_server.R),
# the one feed of drugs(), the Bayesian update and the Visualizer, and in the
# Shiny-free path at the top of simulateDrugsWithCovariates(), which the
# Simulation API also reaches.  A row of 0 (Stop, Skip next dose, a scheduled
# 0 that ends a schedule) becomes a 0 for every component, so the schedule
# rules of R/scheduled.R hold for each.  The schedule's suffix is kept, so
# "4 tablet PO bid" becomes "80 mg PO bid" of artemether and "480 mg PO bid"
# of lumefantrine, which simCpCe() repeats as it does any scheduled dose.
#
# The other way: a click on a component's panel (or on a metabolite of one,
# which formedMetaboliteParent() first maps to its parent) acts on the
# product, through combinationProductOf().
#
# The components have library rows with no dose units and no Category, as a
# metabolite-only drug has, so they are not offered on their own: artemether
# and lumefantrine are given in the United States only as Coartem
# (docs/library-expansion/combination-only.csv).
# -----------------------------------------------------------------------------

#' The combination products whose components are drawn separately
#'
#' Each entry is one of two kinds.  A product dosed in units of its own
#' (Coartem) has `unit`, the word that stands for one unit of the product in
#' its dose units ("tablet" in "tablet PO"), and `components`, mg of each
#' component drug in one unit of the product.  A product labelled and dosed
#' by the combined weight of its ingredients (Zerbaxa: "1.5 g" is 1 g
#' ceftolozane and 0.5 g tazobactam) has `shares`, the fraction of the
#' product's weight that is each drawn component; it is dosed in the
#' ordinary mass units (g, mg/kg, g/hr, and their schedules), each of which
#' its component receives unchanged with the dose times its share.  Either
#' is named by the component's library name.
#'
#' A product with one drawn component (Zerbaxa) is still a product here: its
#' beta-lactamase inhibitor is not drawn (docs/library-expansion/README.md,
#' "Which moieties a combination product draws"), and the expansion is what
#' lets the drug be the product, as Steven L. Shafer named it, while its
#' panel is labelled with the moiety drawn (2026-11-18, "Use Zerbaxa and
#' Avycaz too"; "The PK should be labelled piperacillin", for Zosyn).
#' @keywords internal
COMBINATION_PRODUCTS <- list(
  Coartem = list(
    unit = "tablet",
    # Coartem label, section 11: "Each Coartem Tablet contains 20 mg of
    # artemether and 120 mg of lumefantrine."
    components = c(artemether = 20, lumefantrine = 120)
  ),
  Zerbaxa = list(
    # Zerbaxa label (NDA 206829, DailyMed 2026-05-28), section 2: "ZERBAXA
    # 1.5 gram (g) (ceftolozane 1 g and tazobactam 0.5 g)"; the pediatric
    # "30 mg/kg ... Provides 20 mg/kg ceftolozane and 10 mg/kg tazobactam".
    # Tazobactam is not drawn.
    shares = c(ceftolozane = 2 / 3)
  )
)

#' Is a combination product dosed by the combined weight of its ingredients?
#' @param product a product's name
#' @returns TRUE for a product with `shares` (Zerbaxa), FALSE otherwise
#' @keywords internal
combinationByMass <- function(product)
{
  isTRUE(isCombinationProduct(product)) && !is.null(COMBINATION_PRODUCTS[[product]]$shares)
}

#' How much of each component one unit of the product carries
#'
#' @param product a product's name
#' @returns a named vector: mg per unit of the product (`components`), or
#'   the fraction of the product's weight (`shares`)
#' @keywords internal
combinationAmounts <- function(product)
{
  spec <- COMBINATION_PRODUCTS[[product]]
  if (!is.null(spec$shares)) spec$shares else spec$components
}

#' The mass units a product dosed by weight is offered in, with schedules
#' @keywords internal
combinationMassUnits <- function()
{
  c(bolusUnits, infusionUnits,
    as.vector(outer(bolusUnits, names(SCHEDULE_INTERVALS), paste)))
}

#' Is this a combination product drawn as its components?
#' @param drug drug names
#' @returns a logical vector
#' @keywords internal
isCombinationProduct <- function(drug)
{
  !is.na(drug) & drug %in% names(COMBINATION_PRODUCTS)
}

#' The component drugs of a combination product
#' @param product a product's name
#' @returns the component drugs' names, or character(0)
#' @keywords internal
combinationComponents <- function(product)
{
  if (length(product) != 1 || !isTRUE(isCombinationProduct(product)))
    return(character(0))
  names(combinationAmounts(product))
}

#' Drug names with each combination product replaced by its components
#'
#' For what asks which drugs are simulated (the Patient Profile fields a
#' drug reads, the drugs a level can be measured for) from the names the
#' user typed.
#' @param drugs drug names
#' @returns the names, products replaced in place, without repeats
#' @keywords internal
combinationExpandDrugNames <- function(drugs)
{
  drugs <- as.character(drugs)
  unique(unlist(lapply(drugs, function(d)
    if (isTRUE(isCombinationProduct(d))) combinationComponents(d) else d),
    use.names = FALSE))
}

#' The combination product a drug is a component of
#'
#' @param drug drug names
#' @returns for each, the product it is a component of, or NA
#' @keywords internal
combinationProductFor <- function(drug)
{
  vapply(as.character(drug), function(d) {
    for (product in names(COMBINATION_PRODUCTS))
      if (isTRUE(d %in% combinationComponents(product))) return(product)
    NA_character_
  }, character(1), USE.NAMES = FALSE)
}

#' The combination product a component was given in
#'
#' @param drug a drug name, as a panel or series names it
#' @param doseTable the dose table the user holds (with products, not
#'   expanded)
#' @returns the product in `doseTable` that `drug` is a component of, where
#'   `drug` is not itself in `doseTable`; otherwise `drug`, unchanged
#' @keywords internal
combinationProductOf <- function(drug, doseTable)
{
  if (length(drug) != 1 || is.na(drug) || is.null(doseTable)) return(drug)
  held <- as.character(doseTable$Drug)
  if (drug %in% held) return(drug)
  for (product in intersect(names(COMBINATION_PRODUCTS), held))
    if (drug %in% combinationComponents(product)) return(product)
  drug
}

#' Is this a unit a combination product can be dosed in?
#'
#' For a product with a unit word, only that word leads it ("tablet PO",
#' "tablet PO bid"): a mass unit would hand each component the product's
#' dose.  For a product dosed by weight (Zerbaxa), the ordinary mass units
#' and their schedules; a TCI target, which names a concentration of no one
#' component, is not one of them.
#' @param product a product's name
#' @param units dose units
#' @returns a logical vector
#' @keywords internal
combinationUnitOk <- function(product, units)
{
  if (combinationByMass(product))
    return(as.character(units) %in% combinationMassUnits())
  startsWith(as.character(units), paste0(COMBINATION_PRODUCTS[[product]]$unit, " "))
}

#' What a combination product is dosed in, for a message
#' @param product a product's name
#' @returns e.g. "tablets", or "units of mass (g, mg/kg, g/hr)"
#' @keywords internal
combinationDosedIn <- function(product)
{
  if (combinationByMass(product)) "units of mass (g, mg/kg, g/hr)" else
    paste0(COMBINATION_PRODUCTS[[product]]$unit, "s")
}

#' Why a unit added to a combination product cannot be simulated, or ""
#'
#' The Drug Library's check of a new unit (drugLibraryUnitProblems()): the
#' product's dose row is expanded and each component must show a
#' concentration.
#' @param product the product's name
#' @param DT a one-row dose table of the product
#' @param library the drug library the simulation reads
#' @param events an empty event table
#' @returns "" when every component moves, else the reason
#' @keywords internal
combinationUnitProblem <- function(product, DT, library, events)
{
  DT <- expandCombinationProducts(DT)
  drugs <- recalculatePK(NULL, library, DT, age = 35, weight = 70, height = 170, sex = "male")
  sims <- processdoseTable(DT, events, drugs, MINS_PER_DAY, FALSE)
  still <- Filter(function(d) !any(is.finite(sims[[d]]$wide$Plasma) & sims[[d]]$wide$Plasma > 0),
                  combinationComponents(product))
  if (length(still)) paste("no concentration of", paste(still, collapse = " or "),
                           "follows a dose") else ""
}

#' A combination product's unit rewritten as its component's
#'
#' "tablet PO" becomes "mg PO" and "tablet PO bid" "mg PO bid": the product's
#' unit word, which leads the unit, is replaced by mg.
#' @param units the product's dose units
#' @param unitWord the product's unit word
#' @returns the component's dose units
#' @keywords internal
combinationComponentUnits <- function(units, unitWord)
{
  units <- as.character(units)
  lead <- paste0(unitWord, " ")
  ok <- startsWith(units, lead)
  units[ok] <- paste0("mg ", substring(units[ok], nchar(lead) + 1L))
  units
}

#' Rewrite each combination product's rows as rows of its components
#'
#' @param DT a dose table (Drug, Time, Dose, Units, and any other columns,
#'   which each component row keeps)
#' @returns the table with every product row replaced by one row per
#'   component, the dose in mg of that component; a table without products
#'   is returned unchanged
#' @keywords internal
expandCombinationProducts <- function(DT)
{
  if (is.null(DT) || !is.data.frame(DT) || nrow(DT) == 0 ||
      !("Drug" %in% names(DT))) return(DT)
  isProduct <- isCombinationProduct(as.character(DT$Drug))
  if (!any(isProduct)) return(DT)

  keep <- DT[!isProduct, , drop = FALSE]
  pieces <- list(keep)
  for (product in unique(as.character(DT$Drug[isProduct])))
  {
    spec <- COMBINATION_PRODUCTS[[product]]
    rows <- DT[!is.na(DT$Drug) & DT$Drug == product, , drop = FALSE]
    # Only the product's own unit word can be split into mg of each component
    wrong <- !(combinationUnitOk(product, rows$Units) %in% TRUE)
    if (any(wrong))
      stop(product, " is dosed in ", combinationDosedIn(product), ", not in ",
           paste(unique(as.character(rows$Units[wrong])), collapse = ", "), ".", call. = FALSE)
    amounts <- combinationAmounts(product)
    for (component in names(amounts))
    {
      part <- rows
      part$Drug <- component
      part$Dose <- as.numeric(rows$Dose) * amounts[[component]]
      # A product dosed by weight hands each component the same unit
      units <- if (combinationByMass(product)) as.character(rows$Units) else
        combinationComponentUnits(rows$Units, spec$unit)
      part$Units <- if (is.factor(rows$Units)) factor(units) else units
      pieces[[length(pieces) + 1L]] <- part
    }
  }
  out <- do.call(rbind, pieces)
  # Each drug's rows in time order, as doseTableClean() leaves them.
  timeOrder <- suppressWarnings(as.numeric(out$Time))
  out <- out[order(as.character(out$Drug), timeOrder), , drop = FALSE]
  rownames(out) <- NULL
  out
}
