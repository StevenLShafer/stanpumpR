# -----------------------------------------------------------------------------
# Cyclosporine (Fanta 2007): three compartments, whole blood, intravenous
# (Sandimmune) and oral microemulsion (Neoral)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg, so the plotted concentration is ng/mL (whole blood).
#
# Drafted by Claude Code, 2026-11-13, at the request of Steven L. Shafer, who
# asked for the common drugs with narrow therapeutic windows that the library
# lacked, and on 2026-11-13 chose this source, with its covariates held at
# their references and one clearance with F 0.47, over the adult alternative.
# The numbers below were read from the full text of the source (the PubMed
# Central page of PMC2198781: Methods, Results, Tables 1-3 and the footnote
# giving the covariate model) and from the Sandimmune (Reference ID 5758649)
# and Neoral (Reference ID 3722656) labels, read in full.
#
# THE SOURCE
# ==========
# Fanta S, Jonsson S, Backman JT, Karlsson MO, Hoppu K.  Developmental
# pharmacokinetics of ciclosporin -- a population pharmacokinetic study in
# paediatric renal transplant candidates.  Br J Clin Pharmacol
# 2007;64:772-784 (doi 10.1111/j.1365-2125.2007.03003.x).  162 children
# (0.36-17.5 years, 6.9-64 kg) on peritoneal dialysis before transplantation,
# given cyclosporine 3 mg/kg intravenously over 4 h (Sandimmun) and 10 mg/kg
# orally (89 of them as the microemulsion, Neoral, whose data were used), each
# followed by whole-blood sampling to 24 h, specific monoclonal RIA; NONMEM.
# "A linear three-compartment model with first-order absorption without lag
# time".  Table 2, at 13 kg, cholesterol 5.4 mmol/L, serum creatinine 524
# umol/L and haematocrit 31%:
#
#     CL 6.1 L/h    V2 (central) 5.3 L    Q3 1.5 L/h    V3 19.6 L
#     Q4 3.0 L/h    V4 4.4 L              Ka 0.68 /h    F 0.36
#
# with every clearance x (BW/13)^0.75 and every volume x (BW/13), and all of
# them x [1 - 0.0542 (cholesterol - 5.4)] x [1 - 0.00732 (haematocrit - 31)]
# x [1 + 0.000214 (creatinine - 524)].  The fixed allometric exponents were
# kept because NONMEM's estimates (0.787 and 0.951) were close; "Normalization
# of CL for allometric BW (BW3/4) removed its relationship to age".
#
# ONE CLEARANCE, F 0.47
# =====================
# "The final model was assigned a 23% smaller typical value of clearance
# after oral administration than after i.v. administration", which the
# authors attribute to their assay.  The library has one clearance per drug,
# so the intravenous clearance is used for both routes and F is the value
# that keeps the fitted oral exposure, 0.36 / 0.77 = 0.47.
#
# HELD AT THE REFERENCE
# =====================
# Cholesterol (5.4 mmol/L), haematocrit (31%) and serum creatinine (524
# umol/L, a dialysis patient's): the Patient Profile has no cholesterol or
# haematocrit, and a normal creatinine lies below the studied range (165-1669
# umol/L), so all three factors are 1.  An adult haematocrit of 40% would
# lower every parameter by 6.6%.  CYP3A4 and P-glycoprotein inhibitors and
# inducers, and CYP3A5 genotype, are not modelled.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source's own allometric weight terms, evaluated at the pharmacokinetic
# weight with the fat-free-mass switch on and at total body weight with it
# off, which reproduces the published equations.  The source studied
# children up to 64 kg, so an adult is a mild extrapolation of an allometric
# law the source found to remove age from clearance.
#
# CHECKED AGAINST THE LABELS (not used in the fit)
# ================================================
# At 70 kg the model's clearance is 21.5 L/h (5.1 mL/min/kg) and its terminal
# half-life 17.6 h; the Neoral label: blood clearance "approximately 5 to 7
# mL/min/kg in adult recipients of renal or liver allografts", and the
# Sandimmune label a terminal half-life of about 19 hours (10-27).  The oral
# apparent clearance, 46 L/h at 70 kg, is above the Neoral label's adult
# transplant recipients (CL/F 593 mL/min, 35.6 L/h, de novo renal; 492
# mL/min, 29.5 L/h, stable renal).  At the stable renal recipients' 344 mg/day
# (172 mg twice daily) the label gives AUC 6035 ng.h/mL, peak 1333 and trough
# 251 ng/mL; this model at the reference man, 3727, 817 and 80.  So in an
# adult transplant recipient the oral concentrations are likely 1.3 to 1.6
# times this model's, and the trough about three times: the children's oral
# clearance was higher, and their fast distribution leaves a lower trough.
# The intravenous clearance agrees with the label; the oral route is the
# model's weakness in adults.
#
# DOSE BASIS
# ==========
# Cyclosporine is not a salt: Sandimmune injection (50 mg/mL) and Neoral and
# Gengraf capsules (25 and 100 mg) are labelled in mg of cyclosporine.  The
# oral route is the microemulsion (Neoral, Gengraf, "cyclosporine
# modified"); Sandimmune capsules and oral solution are not bioequivalent to
# it and are not modelled.
#
# THE BAND
# ========
# The Sandimmune label: "in one series of 375 consecutive cadaveric renal
# transplant recipients, dosage was adjusted to achieve specific whole blood
# 24-hour trough concentrations of 100 to 200 ng/mL as determined by
# high-pressure liquid chromatography"; nonspecific assays read about twice
# as high.  A trough range, drawn against the whole curve.
#
# PREGNANCY
# =========
# Not applied (unquantified): trough concentrations fall in pregnancy
# (Kozlowska-Boszko 1998, abstract), part of which is the fall in
# haematocrit; no clearance was measured.
# -----------------------------------------------------------------------------

# Fanta 2007, Table 2, at 13 kg and the reference covariates
CYCLOSPORINE_WT_REF <- 13       # kg
CYCLOSPORINE_CL  <- 6.1         # L/h
CYCLOSPORINE_V1  <- 5.3         # L (the source's V2, central)
CYCLOSPORINE_Q2  <- 1.5         # L/h (Q3)
CYCLOSPORINE_V2  <- 19.6        # L (V3)
CYCLOSPORINE_Q3  <- 3.0         # L/h (Q4)
CYCLOSPORINE_V3  <- 4.4         # L (V4)
CYCLOSPORINE_KA  <- 0.68        # 1/h
CYCLOSPORINE_F   <- 0.36        # with an oral clearance 23% below CL
CYCLOSPORINE_ORAL_CL_RATIO <- 0.77

#' Cyclosporine (Fanta 2007)
#'
#' Three compartments, whole blood, intravenous and oral microemulsion; see
#' the header of \code{R/drugs_cyclosporine.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
cyclosporine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  w <- pkW / CYCLOSPORINE_WT_REF
  fcl <- w^0.75 / 60            # L/h to L/min
  default <- list(
    v1  = CYCLOSPORINE_V1 * w,
    v2  = CYCLOSPORINE_V2 * w,
    v3  = CYCLOSPORINE_V3 * w,
    cl1 = CYCLOSPORINE_CL * fcl,
    cl2 = CYCLOSPORINE_Q2 * fcl,
    cl3 = CYCLOSPORINE_Q3 * fcl,
    ka_PO = CYCLOSPORINE_KA / 60,
    bioavailability_PO = CYCLOSPORINE_F / CYCLOSPORINE_ORAL_CL_RATIO,
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Fanta S et al., Br J Clin Pharmacol 2007;64:772-784: three compartments, ",
    "whole blood, children before renal transplantation; at 13 kg CL 6.1 L/h, ",
    "V1 5.3 L, Q 1.5 and 3.0 L/h, V 19.6 and 4.4 L, x (BW/13)^0.75 or BW/13; ",
    "Neoral ka 0.68 /h, F 0.47 (0.36 with the oral clearance 23% lower); ",
    "cholesterol, haematocrit and creatinine at the source's references. ",
    "https://doi.org/10.1111/j.1365-2125.2007.03003.x"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 150,
    upperTypical = 200,
    lowerTypical = 100,
    reference = reference
  )
}
