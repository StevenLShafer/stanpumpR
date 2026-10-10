# -----------------------------------------------------------------------------
# Aripiprazole, oral only, with dehydroaripiprazole as its active metabolite
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-10-09, at the request of Steven L. Shafer, from
# the antipsychotic implementation brief.  Only the abstracts of Kim 2008 and
# Kim 2012 could be read.  Checked there: one compartment each, ka 1.06 /h
# fixed, plasma, 80 patients on 10 to 30 mg/day, intermediate metabolisers'
# CL/F about 60% of normal, and Kim 2012's EC50 of 8.63 ng/mL.  NOT checked:
# the genotype-class clearances, the metabolite's CLm/fm and Vm/fm, the
# formation convention, and Kim 2012's ke0 of 0.725 /h and Emax of 100%.
#
# V/F: the brief gives 193 L from Table 3; the abstract says 192 L.  The
# abstract is the text that could be read, so 192 L is used.
#
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL (plasma).
#
# THE SOURCE
# ==========
# Kim J-R et al., Br J Clin Pharmacol 2008;66:802-810: 141 steady-state plasma
# concentrations from 80 Korean psychiatric patients.  One compartment each for
# parent and metabolite, APPARENT oral parameters:
#
#     ka 1.06 /h (fixed), V/F 192 L
#     CL/F by CYP2D6 genotype class: 3.15 L/h (two functional alleles), 2.66
#       (functional + partially deficient), 2.27 (functional + null), 1.83
#       (intermediate metabolisers)
#     CLm/fm 8.02 L/h, Vm/fm 587 L
#
# CYP2D6 PHENOTYPE
# ================
# normal = 3.15 L/h and intermediate = 1.83 L/h, the source's two named
# groups.  Poor and ultrarapid metabolisers were NOT studied; they take the
# nearest studied value (1.83 and 3.15).  For a poor metaboliser that
# overstates clearance and understates exposure.
#
# FORMATION
# =========
# The metabolite parameters are divided by fm, and the whole of the parent's
# apparent clearance forms the scaled metabolite, kFormation = (CL/F)/(V/F).
# That convention is assumed, not read from the paper; it is supported by the
# abstract's finding that the dehydroaripiprazole concentration-to-dose ratio
# did not differ between genotypes, which is what it predicts (the metabolite's
# steady state is dose rate / (CLm/fm) whatever the parent clearance).  It
# predicts a steady-state metabolite-to-parent ratio of (CL/F)/(CLm/fm), 0.39
# in normal and 0.23 in intermediate metabolisers, against 0.20 to 0.34
# observed.  The scaled parameters live in "dehydroaripiprazole", which is
# never dosed directly.  Amounts in aripiprazole mass (mwRatio = 1).
#
# Body size: no weight covariate, so the published values are the 70 kg
# reference adult's and scale to fat-free mass (legacyVolume = 1).
#
# EFFECT SITE
# ===========
# Kim E 2012: [11C]raclopride PET with serial plasma aripiprazole in 18 healthy
# men, fitted with an effect compartment driven by PARENT plasma: ke0 0.725 /h,
# EC50 8.63 ng/mL, Emax 100%.  ke0 is supplied directly, so the plotted
# effect-site concentration is that model's Ce, and D2 occupancy is
# 100 x Ce / (8.63 + Ce) -- of parent only.  Adding dehydroaripiprazole to the
# 8.63 ng/mL denominator would be wrong.  The PK and PD come from different
# populations, which antipsychoticProfiles() records.  Occupancy is a receptor
# biomarker, not a target: MEAC and the band are zero.
# -----------------------------------------------------------------------------

ARIPIPRAZOLE_V   <- 192     # L, apparent (abstract; the brief says 193)
ARIPIPRAZOLE_KA  <- 1.06    # 1/h, fixed in the source
ARIPIPRAZOLE_KE0 <- 0.725   # 1/h, Kim 2012

ARIPIPRAZOLE_CL_CYP2D6 <- c(   # L/h, apparent; see the header
  poor         = 1.83,
  intermediate = 1.83,
  normal       = 3.15,
  ultrarapid   = 3.15
)

#' Aripiprazole pharmacokinetics (oral)
#'
#' Kim et al. (2008): one compartment, apparent oral parameters, CYP2D6 on
#' clearance, with dehydroaripiprazole formed as an active metabolite; effect
#' site from Kim et al. (2012).
#'
#' @inheritParams cefazolin
#' @param cyp2d6 CYP2D6 metaboliser phenotype, one of \code{CYP2D6_VALUES}
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
aripiprazole <- function(weight, height, age, sex, cyp2d6 = CYP2D6_DEFAULT,
                         adjustToFFM = TRUE)
{
  if (length(cyp2d6) != 1 || !cyp2d6 %in% CYP2D6_VALUES) {
    stop("Invalid cyp2d6: ", paste(cyp2d6, collapse = ", "),
         ". Must be one of: ", paste(CYP2D6_VALUES, collapse = ", "))
  }

  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  v1  <- ARIPIPRAZOLE_V * size$volume
  cl1 <- unname(ARIPIPRAZOLE_CL_CYP2D6[[cyp2d6]]) / 60 * size$clearance   # L/min

  default <- list(
    v1 = v1,
    v2 = 1,                                        # one compartment
    v3 = 1,
    cl1 = cl1,
    cl2 = 0,
    cl3 = 0,
    ka_PO = ARIPIPRAZOLE_KA / 60,                  # 1/min
    bioavailability_PO = 1,                        # apparent (/F)
    tlag_PO = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Kim JR et al., Br J Clin Pharmacol 2008;66:802-810. ",
    "https://doi.org/10.1111/j.1365-2125.2008.03223.x (apparent oral ",
    "parameters); effect site: Kim E et al., J Cereb Blood Flow Metab ",
    "2012;32:759-768. https://doi.org/10.1038/jcbfm.2011.180"
  )

  list(
    PK = PK,
    tPeak = 0,                        # ke0 is supplied, not solved
    ke0 = ARIPIPRAZOLE_KE0 / 60,      # 1/min
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference,
    metabolite = list(
      name              = "dehydroaripiprazole",
      kFormation        = cl1 / v1,   # all apparent clearance, fm-scaled
      firstPassFraction = 0,
      mwRatio           = 1           # scaled amounts in aripiprazole mass
    )
  )
}
