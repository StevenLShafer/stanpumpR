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
#' Each entry: `unit`, the word that stands for one unit of the product in
#' its dose units ("tablet" in "tablet PO"), and `components`, mg of each
#' component drug in one unit of the product, named by the component's
#' library name.
#' @keywords internal
COMBINATION_PRODUCTS <- list(
  Coartem = list(
    unit = "tablet",
    # Coartem label, section 11: "Each Coartem Tablet contains 20 mg of
    # artemether and 120 mg of lumefantrine."
    components = c(artemether = 20, lumefantrine = 120)
  )
)

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
  names(COMBINATION_PRODUCTS[[product]]$components)
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
#' Only the product's own unit word leads it ("tablet PO", "tablet PO bid"):
#' a mass unit would hand each component the product's dose.
#' @param product a product's name
#' @param units dose units
#' @returns a logical vector
#' @keywords internal
combinationUnitOk <- function(product, units)
{
  startsWith(as.character(units), paste0(COMBINATION_PRODUCTS[[product]]$unit, " "))
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
      stop(product, " is dosed in ", spec$unit, "s, not in ",
           paste(unique(as.character(rows$Units[wrong])), collapse = ", "), ".", call. = FALSE)
    for (component in names(spec$components))
    {
      part <- rows
      part$Drug <- component
      part$Dose <- as.numeric(rows$Dose) * spec$components[[component]]
      units <- combinationComponentUnits(rows$Units, spec$unit)
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
