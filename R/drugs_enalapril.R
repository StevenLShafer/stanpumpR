# -----------------------------------------------------------------------------
# Enalapril (Renitec, Vasotec) -- the prodrug, plotted with its active
# metabolite enalaprilat
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-20, at the request of Steven L. Shafer, from
# the full text of Faisal M, Cawello W, Burckhardt BB, de Hoon J, Laer S,
# Front Pediatr 2019;7:281 (PMC6629781), read through PubMed Central.  Tests:
# tests/testthat/test-drugs-enalapril.R and test-transit-chain.R.
#
# WHAT THE SOURCE IS
# ==================
# A simultaneous semi-mechanistic population analysis of 2208 serum AND urine
# concentrations of enalapril and enalaprilat from 24 healthy adults (mean age
# 28 y, mean weight 69.76 kg) given a single 10 mg oral dose of enalapril
# maleate, in a two-period crossover of the reference tablet (Renitec) against
# child-appropriate orodispersible mini-tablets, sampled to 48 hours.  NONMEM,
# ADVAN6, FOCE with interaction, validated by visual predictive check,
# bootstrap and sampling-importance-resampling.  Part of the LENA project
# (labelling of enalapril from neonate to adolescence).
#
# Fitting serum and urine together for BOTH moieties is what makes this model
# unusual and what makes it usable: it closes the mass balance, so the
# bioavailability is a separately estimated parameter (F1 = 0.606, against the
# literature's 60%) rather than a scale hidden inside the volumes, and VC, VM
# and every clearance are therefore ABSOLUTE rather than apparent.  Almost no
# oral antihypertensive model in this library can say that.
#
# THE STRUCTURE, AND WHY IT NEEDED A NEW ENGINE
# =============================================
# Enalapril: ONE compartment (the source tested two and rejected them for
# higher standard errors with no better fit), reached through a chain of EIGHT
# Erlang transit compartments, eliminated along two paths -- renally at KREN
# and by conversion to enalaprilat at KM.  Enalaprilat: TWO compartments,
# reached through TWO more transit compartments that delay its formation,
# eliminated renally at KME.  Thirteen states in all.
#
# A chain of N identical transit compartments has one eigenvalue repeated N
# times, and every closed-form coefficient in this library divides by a
# difference of two rate constants.  separateEigenvalues() nudges accidental
# ties apart by a relative 1e-8, but for a structural tie eight deep the
# partial-fraction coefficients would scale as 1/nudge^7, which is the
# catastrophic cancellation that docs/antihypertensives.md section 3.3
# rejected esmolol's decomposition for.  The system is nonetheless linear, so
# it needs no numerical solver -- only exact handling of a repeated
# eigenvalue, which is a matrix exponential.  See R/advanceTransitChain.R;
# this drug is why that engine exists.
#
# Discretising the parent's input into lagged fractional doses, the answer for
# hydrochlorothiazide's Weibull, is closed off here: convolution commutes, so
# it would be exact in the limit for the parent, but the metabolite has its
# own transit delay and the two moieties would need DIFFERENT input kernels
# from one dose table, which the architecture does not allow.
#
# WHERE KTR AND KA EACH ACT
# =========================
# The source is explicit, and this matters because getting it wrong moves the
# peak: "the rate constant of absorption (KA) was the transfer rate of the
# drug from the LAST transit into enalapril central compartment", while KTR is
# "the rate transfer THROUGH these transits", computed from the printed mean
# transit time by MTT1 = (N+1)/KTR with N = 8.  So seven transfers run at
# KTR = 9/0.558 = 16.13 /h and the eighth, out of the last transit, at
# KA = 6.010 /h.  Both printed parameters are used exactly as the source
# defines them.
#
# The one casualty is a derived summary the source's own two statements cannot
# both satisfy: its formula presumes nine steps at KTR, while the structure it
# describes has seven at KTR and one at KA, so the model's true mean input
# time is 7/KTR + 1/KA = 0.60 h against the printed MTT1 of 0.558 h.  That is
# an inconsistency in the source's shorthand, not in its parameters, and three
# other readings were integrated numerically before this one was chosen: all
# four reproduce the source's own reported observations (parent peak 0.82 to
# 0.95 h against its "almost 1 h"; metabolite peak 3.20 to 3.32 h against its
# "around 3 to 4 h") with peak concentrations varying under 6%.  This reading
# is the one the text states, and that is the whole reason for it.  It is NOT
# the closest of the four to the reported peak: reading 1, nine compartments
# with a depot emptying at KA, is nearer at 0.95 h against this one's 0.88 h.
# Worth saying rather than glossing, because it means agreement with a
# reported summary was never what chose the reading -- the text was.
# test-drugs-enalapril.R integrates all four and pins them.
#
# The peak this predicts at the reference individual is 75.8 ng/mL for the
# parent and 55.9 for the metabolite, evaluated on a 0.01 minute grid.  What
# the PLOT shows is a hair lower on a long window, because simulationTimeGrid()
# samples a 14-day plot more coarsely through the peak than a 2-day one (75.3
# against 75.8); the curve is the same, the sampled maximum is not.
#
# THE DOSE IS ENTERED AS PRESCRIBED
# =================================
# The source gave "two 5 mg strength of enalapril maleate" tablets, so its
# doses are the labelled strength of the maleate salt, which is also what a
# prescription says.  F1 = 0.606 is a fitted scalar on that labelled strength,
# so whether it also absorbs the maleate-to-base mass fraction (0.764) is not
# resolvable from the text -- and does not need to be, because the model is
# used on the same basis it was fitted on.  There is no `molar` block and no
# salt factor; a 10 mg tablet entered as 10 mg reproduces the paper.
#
# There is likewise NO molecular-weight ratio between the moieties.  KM moves
# AMOUNT out of enalapril's compartment into the metabolite's transit chain,
# and VM was fitted against measured enalaprilat concentrations driven by that
# same flux, so the 376.45-to-348.4 ratio is already inside the fitted VM.
# Applying it again would count it twice.
#
# COVARIATES: ONE, AND IT IS WEIGHT ON THE PARENT'S VOLUME
# ========================================================
# Age, weight, height, sex and total body water were all available and tested
# stepwise, the exponent fixed at 1 for a volume and 0.75 for a clearance.
# Only weight on the PARENT's central volume survived (objective function down
# 18.2, and backward elimination put it back up), normalised to the study's
# mean weight of 69.76 kg.  Nothing was retained on any clearance, nor on the
# metabolite.  The authors add that base and full models differ by under 25%
# and call the covariates clinically unimportant.
#
# So VC scales with weight to the first power and the rate constants do not,
# which makes the parent's clearance scale with weight too -- it is KREN + KM
# times a volume.  The model is written that way rather than by scaling a
# clearance separately, so that the published rate constants stay exactly
# weight-invariant.  No renal covariate exists for either moiety although both
# are cleared renally and nothing else: the population was 24 healthy young
# adults, so a serum creatinine entered in the Patient Profile does not reach
# this drug, and the source's own assumption that the two renal paths account
# for the whole dose is what made its mass balance close.
#
# THE FORMULATION COVARIATE IS NOT APPLIED, AND NEED NOT BE
# =========================================================
# THETA(X) = 0.730 is the one other term in Table 1: by the source's Equation
# 7 it multiplies MTT1 when FORM = 1, the orodispersible mini-tablet.  The
# reference tablet is FORM = 0, so the factor is 1 and the printed MTT1 is
# already the reference formulation's -- which is the product this library has
# reason to model.  (Read against the ODMT it predicts absorption 9 minutes
# earlier, where the source's separate per-formulation fits found 5 minutes by
# Wilcoxon test; both numbers are in the paper, from different analyses.)
#
# WHAT IS NOT MODELLED
# ====================
# No effect site for either moiety, no band, and no blood pressure: see
# docs/antihypertensives.md for why no antihypertensive in this library plots
# a pressure.  A time until threshold is not reported either, and that is
# structural rather than pending -- it is solved from the exponential
# amplitudes the closed-form engines leave in hand, and a defective system has
# none to leave.
#
# EXTERNAL VALIDATION, AND IT SETTLES THE DOSE BASIS
# ==================================================
# Arafat T, Awad R, Hamad M, Azzam R, Al-Nasan A, Jehanli A, Matalka K,
# J Clin Pharm Ther 2005;30:319-328 gave two brands of 10 mg enalapril maleate
# to healthy volunteers and measured BOTH moieties with specific enzyme
# immunoassays rather than the ACE-inhibition assay most earlier work used.
# Nothing of it was an input to Faisal's fit.  Its metabolite peaks were 54.8
# and 57.2 ng/mL; this model gives 55.9, within 3% of each.
#
# That is what decides the dose-basis question the section above has to leave
# open: a factor of 1.308 in either direction puts the prediction outside the
# measured range, so entering the dose as the labelled strength is right and
# not merely self-consistent.  The two studies DISAGREE about the terminal
# phase -- Arafat's metabolite area is 267 and 256 against this model's 495 --
# and the disagreement is theirs to report, which they do: their enalaprilat
# concentrations at 12 to 24 h ran "lower than those reported in the
# literature".  Recorded, not reconciled.  The PARENT is no check at all,
# their enalapril peaks being "about 10 times those published in the
# literature" by their own account.
#
# AN ADULT MODEL FROM A PAEDIATRIC PROGRAMME
# ==========================================
# The paper is in a paediatric journal, its title is about child-appropriate
# mini-tablets, and the project is named "labelling of enalapril from neonate
# to adolescence".  Its DATA are 24 healthy adults, 51.8 to 95.6 kg; not one
# child was studied.  The paediatric columns of the generated help table are
# therefore this library's own allometric extrapolation and not anything the
# source measured -- and the conversion to enalaprilat is by carboxylesterase
# 1, whose activity changes with age, which nothing here represents.
#
# Enalapril is a PREGNANCY CONTRAINDICATION rather than an unstudied drug: the
# ACE inhibitors carry a boxed warning for fetal toxicity, so no pregnancy
# pharmacokinetic study exists and none should be expected.
#
# References
# ----------
# Faisal M, Cawello W, Burckhardt BB, de Hoon J, Laer S. Simultaneous
#   semi-mechanistic population pharmacokinetic modeling analysis of enalapril
#   and enalaprilat serum and urine concentrations from child appropriate
#   orodispersible minitablets. Front Pediatr 2019;7:281.
#   https://doi.org/10.3389/fped.2019.00281
# -----------------------------------------------------------------------------

#' Enalapril central volume at the source's reference individual, litres
#' @keywords internal
ENALAPRIL_VC <- 51.10

#' Enalapril bioavailability, a separately estimated parameter
#' @keywords internal
ENALAPRIL_F1 <- 0.606

#' Enalapril mean transit time of absorption, hours
#' @keywords internal
ENALAPRIL_MTT1 <- 0.558

#' Transit compartments in enalapril's absorption chain
#'
#' Eight, for the reference tablet and the pooled analysis.  (Six were needed
#' for the orodispersible mini-tablet modelled on its own, a formulation this
#' library does not offer.)
#' @keywords internal
ENALAPRIL_TRANSITS <- 8L

#' Enalapril transfer rate out of the last transit compartment, /h
#' @keywords internal
ENALAPRIL_KA <- 6.010

#' Enalapril renal elimination rate constant, /h
#' @keywords internal
ENALAPRIL_KREN <- 0.305

#' Enalapril elimination rate constant by conversion to enalaprilat, /h
#' @keywords internal
ENALAPRIL_KM <- 0.688

#' Reference weight of the source's population, kg
#'
#' Its mean body weight, which is what the source normalised to (its Equation
#' 6 divides by "body weight normalized by the mean body weight of the present
#' study"), not the library's 70 kg standard.
#' @keywords internal
ENALAPRIL_WT_REF <- 69.76


#' Transfer rate constant through a chain of transit compartments, /h
#'
#' The source's own expression, `MTT = (N + 1) / KTR`.  Its inconsistency with
#' the separate rate out of the last transit is discussed in the file header;
#' this is how the source states KTR was obtained, so it is how KTR is
#' obtained here.
#'
#' @param mtt the mean transit time, hours
#' @param n the number of transit compartments
#' @returns the transfer rate constant, per hour
#' @keywords internal
transitRateConstant <- function(mtt, n) (n + 1) / mtt


#' Enalapril pharmacokinetics
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
enalapril <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Weight to the first power on the central volume, normalised to the
  # source's mean weight, and nothing on the rate constants.  The clearances
  # below are rate constants times this volume, so they carry its factor and
  # no second one: that is the source's model, in which only VC moves.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume    = weight / ENALAPRIL_WT_REF,
                        legacyClearance = weight / ENALAPRIL_WT_REF)
  v1 <- ENALAPRIL_VC * size$volume

  default <- list(
    v1  = v1,
    v2  = 1,                                        # one compartment
    v3  = 1,
    # The TOTAL elimination, renal plus conversion, so that the clearance the
    # help tables show is the one the drug has.  The engine splits it again
    # with the block's kMetabolic below.
    cl1 = (ENALAPRIL_KREN + ENALAPRIL_KM) / 60 * v1,
    cl2 = 0,
    cl3 = 0,
    # The last transit compartment empties into the central one at KA; the
    # seven before it empty at KTR, carried in the transitChain block.
    ka_PO = ENALAPRIL_KA / 60,
    bioavailability_PO = ENALAPRIL_F1,
    # None was fitted: the source reports that a lag-time model failed to
    # describe the absorption phase and that the transit chain is what
    # replaced it.
    tlag_PO = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  # The metabolite's disposition, from the one place it is defined
  # (R/drugs_enalaprilat.R), on the same covariates, so that the curve plotted
  # here and the parameters that drug's own page describes cannot drift apart.
  metabolite <- enalaprilatSet(weight, height, age, sex, adjustToFFM)

  reference <- paste0(
    "Faisal M, Cawello W, Burckhardt BB, de Hoon J, Laer S. Simultaneous ",
    "semi-mechanistic population pharmacokinetic modeling analysis of ",
    "enalapril and enalaprilat serum and urine concentrations from child ",
    "appropriate orodispersible minitablets. Front Pediatr 2019;7:281. ",
    "A PRODRUG, PLOTTED WITH ITS ACTIVE METABOLITE ENALAPRILAT, which is the ",
    "moiety that inhibits converting enzyme. 2208 serum and urine ",
    "concentrations of both moieties from 24 healthy adults given 10 mg of ",
    "enalapril maleate. One compartment for enalapril (VC 51.10 L) behind ",
    "EIGHT Erlang transit compartments, eliminated renally at KREN 0.305 /h ",
    "and by conversion at KM 0.688 /h; two compartments for enalaprilat ",
    "behind two more transits. Fitting serum WITH urine closes the mass ",
    "balance, so F1 was estimated separately at 0.606 and the volumes and ",
    "clearances are absolute, not apparent. The dose is the labelled ",
    "strength of enalapril maleate, as prescribed. The only covariate ",
    "retained was weight to the first power on enalapril's central volume, ",
    "normalised to the study's mean 69.76 kg; there is NO renal term for ",
    "either moiety although both are cleared renally and nothing else, the ",
    "population being 24 healthy young adults. The formulation term ",
    "THETA(X) 0.730 applies to the orodispersible mini-tablet and not to the ",
    "reference tablet modelled here. Simulated by the exact ",
    "matrix-exponential engine, because a chain of eight identical ",
    "compartments has an eigenvalue repeated eight times. ",
    "https://doi.org/10.3389/fped.2019.00281"
  )

  return(
    list(
      PK = PK,
      tPeak = 0,    # converting-enzyme inhibition and blood pressure are not
      MEAC  = 0,    # plotted; see the header
      typical      = 0,   # no established therapeutic plasma range
      upperTypical = 0,
      lowerTypical = 0,
      # The chain, and the metabolite formed behind its own; see
      # R/advanceTransitChain.R.  n counts every compartment of the chain,
      # the last of which empties at ka_PO rather than ktr.
      transitChain = list(
        n   = ENALAPRIL_TRANSITS,
        ktr = transitRateConstant(ENALAPRIL_MTT1, ENALAPRIL_TRANSITS) / 60,
        # The share of the parent's elimination that forms the metabolite
        # rather than leaving the body.  cl1 above covers KREN + KM.
        kMetabolic = ENALAPRIL_KM / 60,
        metabolite = list(
          name = "enalaprilat",
          # Every compartment of this chain empties at ktr: the source names
          # no separate rate out of its last one, as it does for the parent.
          n   = ENALAPRILAT_TRANSITS,
          ktr = transitRateConstant(ENALAPRILAT_MTT2, ENALAPRILAT_TRANSITS) / 60,
          v   = metabolite$v1,
          kel = ENALAPRILAT_KME / 60,
          k12 = ENALAPRILAT_KQ1 / 60,
          k21 = ENALAPRILAT_KQ2 / 60
        )
      ),
      reference = reference
    )
  )
}
