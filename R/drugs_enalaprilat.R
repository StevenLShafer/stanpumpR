# -----------------------------------------------------------------------------
# Enalaprilat (the active metabolite of enalapril)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-20, at the request of Steven L. Shafer, from
# the full text of Faisal M, Cawello W, Burckhardt BB, de Hoon J, Laer S,
# Front Pediatr 2019;7:281 (PMC6629781), read through PubMed Central.  Tests:
# tests/testthat/test-drugs-enalapril.R.
#
# WHAT THIS DRUG IS FOR
# =====================
# Enalaprilat is the moiety that inhibits converting enzyme; enalapril itself
# is an inactive prodrug.  This entry exists so that the enalaprilat formed
# from a dose of enalapril has a row and a panel of its own, the same
# arrangement aspirin and salicylate or fluoxetine and norfluoxetine have.  It
# is NOT independently dosable: its CSV row carries no Units and no Category,
# so it never appears in the dose table's autocomplete, the Add a dose dialog
# or the startup menu, and its whole curve comes from foldMetabolites().
# (visibleDrugNames() drops every drug with no dose units, and a click on its
# panel opens the dose dialogs on enalapril, formedMetaboliteParent(); before
# 2026-10-27 neither was so and enalaprilat was offered with no unit.)
#
# WHY THERE IS NO INTRAVENOUS ENALAPRILAT HERE
# ============================================
# Intravenous enalaprilat is a real product, and these parameters are absolute
# rather than apparent -- Faisal fitted serum AND urine for both moieties
# simultaneously, which closes the mass balance and makes the bioavailability
# a separately estimated parameter (0.606) instead of a scale hidden in the
# volumes.  So the arithmetic for an intravenous dose would run.  It is still
# not offered, because these parameters were fitted to enalaprilat FORMED from
# oral enalapril and never to an injected dose, and the library's rule is that
# a parameter set is used for what its source measured.  The registry's
# separate enalaprilat injection record says what would be needed, and warns
# in particular that the label's "about 11 hour" accumulation half-life comes
# from repeated ORAL enalapril and must not be read as an intravenous
# elimination half-life.
#
# THE MODEL
# =========
# Two compartments, from Table 1 of the source: VM 46.10 L (4.00% RSE), KQ1
# 0.060 /h (4.00%), KQ2 0.054 /h (10.0%), KME 0.184 /h (4.00%).  The
# peripheral volume is not fitted and is implied by the rate constants,
# VM x KQ1 / KQ2 = 51.2 L, which is the same relation advanceMichaelisMenten()
# uses for GHB's k23/k32.  The clearances follow from the rate constants and
# the volume: CL = KME x VM = 8.48 L/h and Q = KQ1 x VM = 2.77 L/h.  The
# implied half-lives are 2.7 and 18.1 hours, which is the "bi-phasic
# exponential decay" the source reports, and the one-compartment model it
# rejected could not produce.
#
# The source's own Discussion quotes the elimination rate constant as 0.175 /h
# where Table 1 estimates 0.184; Table 1 is used, being the parameter table
# with the relative standard errors and the two confidence intervals.  The
# same Discussion quotes the parent's total elimination rate constant as 0.93
# /h where its own KREN and KM sum to 0.993.  Neither affects anything here,
# but both are recorded so that a reader checking the numbers against the
# prose is not left wondering which is wrong.
#
# BODY SIZE
# =========
# The source tested weight on its parameters stepwise, with the exponent fixed
# at 1 for a volume and 0.75 for a clearance, and retained it on the PARENT's
# central volume only -- there is no published weight relation for
# enalaprilat.  So its reference values are its published ones and
# `legacyVolume` and `legacyClearance` are 1: with the fat-free-mass switch
# off this reproduces the source exactly, and with it on the library's own
# scaling applies, as for salicylate and every other model whose source
# fitted no size term.
#
# WHAT IS NOT MODELLED
# ====================
# No effect site and no blood pressure.  Converting-enzyme inhibition is a
# measurable pharmacodynamic endpoint for this moiety and a resting blood
# pressure is not; no antihypertensive in this library plots a pressure (see
# docs/antihypertensives.md).  No band: there is no established therapeutic
# plasma range for enalaprilat.
#
# References
# ----------
# Faisal M, Cawello W, Burckhardt BB, de Hoon J, Laer S. Simultaneous
#   semi-mechanistic population pharmacokinetic modeling analysis of enalapril
#   and enalaprilat serum and urine concentrations from child appropriate
#   orodispersible minitablets. Front Pediatr 2019;7:281.
#   https://doi.org/10.3389/fped.2019.00281
# -----------------------------------------------------------------------------

#' Enalaprilat central volume at the source's reference individual, litres
#' @keywords internal
ENALAPRILAT_VM <- 46.10

#' Enalaprilat distribution rate constant out of the central compartment, /h
#' @keywords internal
ENALAPRILAT_KQ1 <- 0.060

#' Enalaprilat distribution rate constant back from the peripheral one, /h
#' @keywords internal
ENALAPRILAT_KQ2 <- 0.054

#' Enalaprilat elimination rate constant, /h
#' @keywords internal
ENALAPRILAT_KME <- 0.184

#' Mean transit time of enalaprilat formation, hours
#' @keywords internal
ENALAPRILAT_MTT2 <- 0.910

#' Transit compartments delaying enalaprilat's formation
#' @keywords internal
ENALAPRILAT_TRANSITS <- 2L


#' Enalaprilat's disposition, as one PK set
#'
#' The single source of these numbers, read both by `enalaprilat()` for the
#' metabolite drug's own row and help page, and by `enalapril()` for the
#' `transitChain` block the engine advances, so that the curve plotted and the
#' parameters described can never drift apart.
#'
#' @inheritParams cefazolin
#' @returns a list of `v1`, `v2`, `v3`, `cl1`, `cl2`, `cl3`
#' @keywords internal
enalaprilatSet <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # The source fitted no size term for this moiety, so the published values
  # stand when the switch is off (legacy factors of 1).
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  v1 <- ENALAPRILAT_VM * size$volume
  list(
    v1  = v1,
    # Not fitted: implied by the rate constants, VM x KQ1 / KQ2.
    v2  = v1 * ENALAPRILAT_KQ1 / ENALAPRILAT_KQ2,
    v3  = 1,                                        # two compartments
    # The rate constants are weight-invariant in the source, so the
    # clearances carry the volume's factor and no second one of their own.
    cl1 = ENALAPRILAT_KME / 60 * v1,
    cl2 = ENALAPRILAT_KQ1 / 60 * v1,
    cl3 = 0
  )
}


#' Enalaprilat pharmacokinetics (formed from enalapril)
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
enalaprilat <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  default <- enalaprilatSet(weight, height, age, sex, adjustToFFM)

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Faisal M, Cawello W, Burckhardt BB, de Hoon J, Laer S. Simultaneous ",
    "semi-mechanistic population pharmacokinetic modeling analysis of ",
    "enalapril and enalaprilat serum and urine concentrations from child ",
    "appropriate orodispersible minitablets. Front Pediatr 2019;7:281. ",
    "THE ACTIVE MOIETY of enalapril, formed from it and not dosed directly. ",
    "Two compartments from Table 1: VM 46.10 L, KQ1 0.060 /h, KQ2 0.054 /h, ",
    "KME 0.184 /h, so CL 8.48 L/h, Q 2.77 L/h and an implied peripheral ",
    "volume of 51.2 L, with half-lives of 2.7 and 18.1 h. Fitted to serum ",
    "AND urine for both moieties in 24 healthy adults given 10 mg of ",
    "enalapril maleate, which closes the mass balance and makes these ",
    "parameters absolute rather than apparent. No size, renal or other ",
    "covariate was retained for this moiety: the source's weight term sits ",
    "on the parent's central volume alone. Not offered intravenously, ",
    "although the product exists, because these parameters were fitted to ",
    "metabolite formed from an oral dose. ",
    "https://doi.org/10.3389/fped.2019.00281"
  )

  list(
    PK = PK,
    tPeak = 0,          # converting-enzyme inhibition is not plotted
    MEAC = 0,
    typical = 0,        # no established therapeutic plasma range
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference
  )
}
