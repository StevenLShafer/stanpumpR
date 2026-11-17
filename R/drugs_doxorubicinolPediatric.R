# -----------------------------------------------------------------------------
# Doxorubicinol formed in children (Voller 2015)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL (= ug/L) of doxorubicinol.
#
# Drafted by Claude Code, 2026-11-06.  Metabolite only: it has no units of
# its own and is not dosed; R/drugs_doxorubicinPediatric.R forms it, half of
# doxorubicin's clearance (fM fixed at 0.5), and its header gives the source.
# One compartment, CLM 42.5 L/h/m2 and VM 760 L/m2 at the median child (BSA
# 0.77 m2), scaled linearly on BSA as every parameter of the model is
# (1 + 1.30 (BSA - 0.77)); half-life 12.4 h.  It is a separate entry from
# the adults' doxorubicinol (Joerger 2007), which is a different model.
# -----------------------------------------------------------------------------

#' Doxorubicinol formed in children (Voller 2015)
#'
#' One compartment, scaled on body surface area; formed by
#' \code{doxorubicinPediatric()}.  See the header of
#' \code{R/drugs_doxorubicinolPediatric.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
doxorubicinolPediatric <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  p <- DOXORUBICIN_VOLLER
  s <- doxorubicinVollerScale(weight, height, age, sex, adjustToFFM)

  default <- list(
    v1  = p$vm * s,
    v2  = 1,                                 # one compartment
    v3  = 1,
    cl1 = p$clm * s / 60,
    cl2 = 0,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Voller S, Boos J, Krischke M, et al. Clin Pharmacokinet 2015;54:1139-1149 ",
    "(doxorubicinol formed from doxorubicin in children, fM 0.5: one compartment, ",
    "CLM 42.5 L/h/m2, VM 760 L/m2 at BSA 0.77 m2, scaled by 1 + 1.30 (BSA - 0.77)). ",
    "https://doi.org/10.1007/s40262-015-0272-4"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference
  )
}
