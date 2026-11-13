# Physiology sliders: "what if" changes to organ function
#
# Four factors, each a multiple of normal function on a log scale from 0.1
# (a tenth of normal) to 10 (ten times normal), all 1 by default:
#
#   liver    hepatic metabolic capacity (Vm, and so intrinsic clearance)
#   renal    renal excretion of unchanged drug
#   cardiac  cardiac output; liver blood flow is a fixed share of it
#   gi       gastrointestinal absorption rate (ka of the oral routes)
#
# They change a drug's PARAMETERS, once, before the engines see them
# (applyPhysiology(), called by getDrugPK()).  The engines are unchanged: the
# PK is fixed throughout the simulation, as it is for any covariate.
#
# THE LIVER: the well-stirred model (Wilkinson and Shand 1975; Shafer,
# "Basic pharmacokinetics and pharmacodynamics", chapter 2, Figures 10-12).
# Hepatic clearance is CL_H = Q E, with E = CL_int / (Q + CL_int).  With a
# flow factor q and a capacity factor l (CL_int scales with Vm), the ratio of
# the new hepatic clearance to the old one needs only the drug's extraction
# ratio E at normal flow:
#
#   CL_H' / CL_H = q l / (q (1 - E) + l E)
#
# so a flow-limited drug (E near 1) follows hepatic blood flow (q) and is
# nearly indifferent to metabolic capacity, and a capacity-limited drug (E
# near 0) follows metabolic capacity (l) and is nearly indifferent to flow,
# which is Figures 10 and 11 of the chapter.  The new extraction ratio is
# E' = l E / (q (1 - E) + l E), which sets the oral first-pass loss.
#
# Each drug's clearance is split three ways in
# inst/extdata/physiologyParameters.csv: the renal fraction (unchanged drug
# in urine), the hepatic fraction, and the rest (extrahepatic, non-renal:
# plasma esterases, Hofmann elimination), which no slider changes.  Only a
# drug with a row whose values were read in a published source is changed;
# every other drug ignores the sliders, and the app says which.
#
# CARDIAC OUTPUT also scales the intercompartmental clearances (Q2, Q3:
# distribution is perfusion-limited in the mammillary model) and the central
# volume, V1, in the SAME direction: a low cardiac output gives higher early
# concentrations after a bolus (less dilution in the first pass), which a
# mammillary model expresses as a smaller V1.  The exponent on V1 is
# PHYSIOLOGY_V1_CARDIAC_EXPONENT, an assumption, not a fitted value.
#
# ke0 is a property of the effect site, so it is held at its value at normal
# physiology, and the time to peak effect moves with the disposition.
#
# The GI factor scales ka only: the drug arrives faster or slower, the
# amount absorbed is unchanged.

#' Names of the physiology factors
#' @noRd
PHYSIOLOGY_FACTORS <- c("liver", "renal", "cardiac", "gi")

#' Labels of the physiology sliders
#' @noRd
PHYSIOLOGY_LABELS <- c(
  liver   = "Liver function",
  renal   = "Renal function",
  cardiac = "Cardiac output",
  gi      = "GI absorption"
)

#' Range of every physiology factor (multiples of normal function)
#' @noRd
PHYSIOLOGY_MIN <- 0.1
PHYSIOLOGY_MAX <- 10

#' Normal physiology
#' @noRd
PHYSIOLOGY_DEFAULT <- c(liver = 1, renal = 1, cardiac = 1, gi = 1)

#' Exponent of cardiac output on V1 (V1' = V1 x cardiac^exponent).  An
#' assumption: the direction is physiological (less dilution in the first
#' pass at low output), the size is not fitted.
#' @noRd
PHYSIOLOGY_V1_CARDIAC_EXPONENT <- 1

#' Exponent of cardiac output on the intercompartmental clearances.
#' @noRd
PHYSIOLOGY_Q_CARDIAC_EXPONENT <- 1

#' Validate a physiology vector
#'
#' @param physiology NULL (normal), or a numeric vector named by
#'   \code{PHYSIOLOGY_FACTORS}; a factor left out is 1
#' @returns a complete named numeric vector
#' @noRd
checkPhysiology <- function(physiology)
{
  if (is.null(physiology)) return(PHYSIOLOGY_DEFAULT)
  if (is.list(physiology)) physiology <- unlist(physiology)
  if (!is.numeric(physiology) || is.null(names(physiology)) ||
      !all(names(physiology) %in% PHYSIOLOGY_FACTORS) ||
      anyDuplicated(names(physiology)))
    stop("Invalid physiology: a numeric vector named by ",
         paste(PHYSIOLOGY_FACTORS, collapse = ", "))
  for (f in names(physiology))
    if (!is_valid_number(unname(physiology[[f]]), PHYSIOLOGY_MIN, PHYSIOLOGY_MAX))
      stop("Invalid physiology: ", f, " must be between ", PHYSIOLOGY_MIN,
           " and ", PHYSIOLOGY_MAX, " (multiples of normal function)")
  out <- PHYSIOLOGY_DEFAULT
  out[names(physiology)] <- physiology
  out
}

#' Is this normal physiology?
#' @noRd
physiologyIsNormal <- function(physiology)
{
  all(checkPhysiology(physiology) == 1)
}

#' The physiology table
#'
#' One row per drug: the renal and hepatic fractions of its clearance, its
#' hepatic extraction ratio, whether its oral bioavailability carries a
#' hepatic first pass, and where each value was read.
#' @returns a data frame read from
#'   \code{inst/extdata/physiologyParameters.csv}
#' @noRd
physiologyTable <- function()
{
  if (is.null(.physiologyCache$table))
  {
    path <- system.file("extdata", "physiologyParameters.csv", package = "stanpumpR")
    .physiologyCache$table <- utils::read.csv(path, stringsAsFactors = FALSE,
                                              na.strings = c("", "NA"))
  }
  .physiologyCache$table
}

.physiologyCache <- new.env(parent = emptyenv())

#' Verification values whose rows the sliders apply
#' @noRd
PHYSIOLOGY_APPLIED_VERIFICATION <- c("read_full_text", "read_abstract")

#' A drug's physiology row, or NULL when the sliders do not apply to it
#' @noRd
physiologyRow <- function(drug)
{
  tab <- physiologyTable()
  row <- tab[tab$drug == drug & tab$verification %in% PHYSIOLOGY_APPLIED_VERIFICATION, ]
  if (nrow(row) == 0) return(NULL)
  as.list(row[1, ])
}

#' Drugs the physiology sliders change
#' @noRd
physiologyDrugs <- function()
{
  tab <- physiologyTable()
  tab$drug[tab$verification %in% PHYSIOLOGY_APPLIED_VERIFICATION]
}

#' Ratio of the new hepatic clearance to the old (well-stirred model)
#'
#' @param E hepatic extraction ratio at normal flow and capacity, in [0, 1]
#' @param flow factor on hepatic blood flow
#' @param liver factor on hepatic metabolic capacity (intrinsic clearance)
#' @noRd
hepaticClearanceFactor <- function(E, flow, liver)
{
  flow * liver / (flow * (1 - E) + liver * E)
}

#' Hepatic extraction ratio at new flow and capacity (well-stirred model)
#' @noRd
hepaticExtractionAt <- function(E, flow, liver)
{
  liver * E / (flow * (1 - E) + liver * E)
}

#' Factor on a drug's total clearance
#'
#' @param row the drug's physiology row
#' @param physiology a validated physiology vector
#' @noRd
physiologyClearanceFactor <- function(row, physiology)
{
  fr <- row$fraction_renal
  fh <- row$fraction_hepatic
  E  <- row$extraction_ratio
  hepatic <- if (fh > 0) hepaticClearanceFactor(E, physiology[["cardiac"]],
                                                physiology[["liver"]]) else 1
  fr * physiology[["renal"]] + fh * hepatic + (1 - fr - fh)
}

#' Factor on oral bioavailability from the change in hepatic first pass
#'
#' F = F_abs (1 - E), so F' / F = (1 - E') / (1 - E).  The result is capped
#' so that the bioavailability never exceeds 1.
#' @noRd
physiologyFirstPassFactor <- function(row, physiology)
{
  if (!isTRUE(as.logical(row$oral_first_pass))) return(1)
  E <- row$extraction_ratio
  if (E >= 1) return(1)
  E2 <- hepaticExtractionAt(E, physiology[["cardiac"]], physiology[["liver"]])
  (1 - E2) / (1 - E)
}

#' Apply the physiology to one drug's model output
#'
#' @param X what the drug's model function returns
#' @param row the drug's physiology row
#' @param physiology a validated physiology vector
#' @param ke0 NULL, or a named list of the ke0 of each PK set at normal
#'   physiology, which each set then keeps
#' @returns X with its PK sets (and oral formulations, and transit chain)
#'   changed
#' @noRd
applyPhysiology <- function(X, row, physiology, ke0 = NULL)
{
  clFactor <- physiologyClearanceFactor(row, physiology)
  cardiac  <- physiology[["cardiac"]]
  v1Factor <- cardiac ^ PHYSIOLOGY_V1_CARDIAC_EXPONENT
  qFactor  <- cardiac ^ PHYSIOLOGY_Q_CARDIAC_EXPONENT
  fFactor  <- physiologyFirstPassFactor(row, physiology)
  gi       <- physiology[["gi"]]
  scaleF <- function(f) if (is.null(f)) NULL else min(1, f * fFactor)
  scaleKa <- function(ka) if (is.null(ka)) NULL else ka * gi

  for (event in names(X$PK))
  {
    s <- X$PK[[event]]
    s$v1  <- s$v1  * v1Factor
    s$cl1 <- s$cl1 * clFactor
    if (!is.null(s$cl2)) s$cl2 <- s$cl2 * qFactor
    if (!is.null(s$cl3)) s$cl3 <- s$cl3 * qFactor
    s$ka_PO  <- scaleKa(s$ka_PO)
    s$ka_PO2 <- scaleKa(s$ka_PO2)
    # A default oral bioavailability of 1 (none given) carries no first pass
    # to change, by the table's own oral_first_pass column.
    if (!is.null(s$ka_PO) && s$ka_PO > 0 && fFactor != 1)
    {
      f <- if (is.null(s$bioavailability_PO)) 1 else s$bioavailability_PO
      s$bioavailability_PO <- scaleF(f)
    }
    if (!is.null(ke0[[event]]) && ke0[[event]] > 0) s$ke0Physiology <- ke0[[event]]
    X$PK[[event]] <- s
  }
  if (!is.null(X$oralFormulations))
    X$oralFormulations <- lapply(X$oralFormulations, function(a) {
      a$ka_PO <- scaleKa(a$ka_PO)
      if (fFactor != 1)
        a$bioavailability_PO <- scaleF(if (is.null(a$bioavailability_PO)) 1 else a$bioavailability_PO)
      a
    })
  if (!is.null(X$transitChain$ktr)) X$transitChain$ktr <- X$transitChain$ktr * gi
  X$physiology <- list(
    factors = physiology,
    clearance = clFactor,
    v1 = v1Factor,
    intercompartmental = qFactor,
    bioavailability = fFactor,
    ka = gi
  )
  X
}

#' The model structures the sliders do not change
#'
#' Saturable or target-mediated elimination, parallel systems, a dose-size
#' rebuild and an INR or ACT output each carry clearance outside the PK set
#' that applyPhysiology() scales, so a row for such a drug would change only
#' part of its elimination.  test-physiology.R holds the table to this.
#' @noRd
physiologyUnsupported <- function(X)
{
  !is.null(X$michaelisMenten) || !is.null(X$parallelSystems) ||
    isTRUE(X$doseDependent) || !is.null(X$inr) || !is.null(X$act) ||
    !is.null(X$dosePhase) || !is.null(X$transitChain$metabolite)
}

#' Which dosed drugs the physiology sliders do not change
#'
#' @param drugs drug names
#' @returns the names, among \code{drugs}, that have no applied row
#' @noRd
physiologyUnaffected <- function(drugs)
{
  setdiff(unique(drugs), physiologyDrugs())
}

# ---- The sidebar panel -------------------------------------------------------

#' Input id of a physiology slider
#' @noRd
physiologyInputId <- function(factor) paste0("physiology_", factor)

#' How long the sliders must rest before the PK is resolved again (ms)
#' @noRd
PHYSIOLOGY_DEBOUNCE_MS <- 400

#' Slider step, in log10 units (about 2.3% per step)
#' @noRd
PHYSIOLOGY_SLIDER_STEP <- 0.01

#' The Physiology panel of the sidebar
#'
#' Each slider holds log10 of its factor, -1 to 1, so that it moves on a log
#' scale; app.js shows the factor itself (formatPhysiologySlider()).
#' @noRd
physiologyPanel <- function()
{
  tips <- c(
    liver = paste(
      "Hepatic metabolic capacity (intrinsic clearance), as a multiple of",
      "normal. Changes the clearance of capacity-limited drugs (low hepatic",
      "extraction ratio) nearly in proportion, and flow-limited drugs (high",
      "extraction ratio) hardly at all."),
    renal = paste(
      "Renal excretion of unchanged drug, as a multiple of normal. Changes",
      "only the renal share of each drug's clearance."),
    cardiac = paste(
      "Cardiac output, as a multiple of normal. Liver blood flow is taken as",
      "a fixed share of it, so the clearance of flow-limited drugs follows it.",
      "It also scales the intercompartmental clearances and the central",
      "volume (a low output gives higher early concentrations)."),
    gi = paste(
      "Gastrointestinal absorption rate (oral ka), as a multiple of normal.",
      "The drug arrives faster or slower; the amount absorbed is unchanged.")
  )
  sliders <- lapply(PHYSIOLOGY_FACTORS, function(f)
    fieldHelp(
      sliderInput(
        inputId = physiologyInputId(f),
        label = PHYSIOLOGY_LABELS[[f]],
        min = log10(PHYSIOLOGY_MIN),
        max = log10(PHYSIOLOGY_MAX),
        value = 0,
        step = PHYSIOLOGY_SLIDER_STEP,
        ticks = FALSE
      ) |> htmltools::tagAppendAttributes(class = "physiology-slider"),
      tips[[f]],
      placement = "right"
    )
  )
  bslib::accordion_panel(
    "Physiology",
    value = "physiology",
    icon = icon("heart-pulse"),
    tags$p(class = "small text-muted",
           "What if? Each slider is a multiple of normal function, from a",
           "tenth to ten times, on a log scale."),
    sliders,
    actionLink("physiologyReset", "Reset to normal"),
    uiOutput("physiologyNote")
  )
}

#' Factors from the slider inputs (log10 values)
#'
#' @param values a list of the four inputs' values, named by factor; a NULL
#'   (not yet reported) is normal
#' @noRd
physiologyFromInputs <- function(values)
{
  out <- PHYSIOLOGY_DEFAULT
  for (f in PHYSIOLOGY_FACTORS)
  {
    v <- values[[f]]
    if (is.numeric(v) && length(v) == 1 && is.finite(v))
      out[[f]] <- 10 ^ max(log10(PHYSIOLOGY_MIN), min(log10(PHYSIOLOGY_MAX), v))
  }
  # A slider at 0 is exactly normal, not 10^0 rounded.
  out[abs(log10(out)) < PHYSIOLOGY_SLIDER_STEP / 2] <- 1
  out
}

#' A short description of non-normal physiology ("liver x0.5, cardiac x2")
#' @noRd
physiologySummary <- function(physiology)
{
  physiology <- checkPhysiology(physiology)
  changed <- physiology[physiology != 1]
  if (length(changed) == 0) return("")
  paste(sprintf("%s ×%s", tolower(PHYSIOLOGY_LABELS[names(changed)]),
                formatC(changed, digits = 2, format = "fg")),
        collapse = ", ")
}

#' What the Physiology panel says under its sliders
#'
#' @param physiology the factors
#' @param drugs the dosed drugs
#' @noRd
physiologyNote <- function(physiology, drugs)
{
  if (physiologyIsNormal(physiology) || length(drugs) == 0) return(NULL)
  drugs <- unique(drugs)
  changed <- intersect(drugs, physiologyDrugs())
  unchanged <- setdiff(drugs, changed)
  tags$div(
    class = "small physiology-note",
    if (length(changed))
      tags$p(tags$strong("Changed: "), paste(changed, collapse = ", ")),
    if (length(unchanged))
      tags$p(class = "text-warning",
             tags$strong("Not changed (no physiology data yet): "),
             paste(unchanged, collapse = ", "))
  )
}

#' The generated help block `physiology-table`: every row of the physiology
#' table, applied first, with its sources.
#' @noRd
helpPhysiologyTableMarkdown <- function()
{
  tab <- physiologyTable()
  applied <- tab$verification %in% PHYSIOLOGY_APPLIED_VERIFICATION
  tab <- tab[order(!applied, tab$drug), ]
  num <- function(x) ifelse(is.na(x), "—", formatC(x, digits = 3, format = "fg"))
  cell <- function(x) gsub("|", "\\|", ifelse(is.na(x), "", x), fixed = TRUE)
  rows <- sprintf(
    "| [%s](help:drugs/%s) | %s | %s | %s | %s | %s | %s |",
    tab$drug, tab$drug, num(tab$fraction_renal), num(tab$fraction_hepatic),
    num(tab$extraction_ratio),
    ifelse(tab$verification %in% PHYSIOLOGY_APPLIED_VERIFICATION, "yes", "not yet"),
    cell(paste(tab$source_renal, tab$source_hepatic, tab$source_extraction, sep = "; ")),
    cell(tab$notes)
  )
  paste(c(
    "| Drug | Renal share | Hepatic share | Extraction ratio | Applied | Sources | Notes |",
    "|---|---|---|---|---|---|---|",
    rows
  ), collapse = "\n")
}
