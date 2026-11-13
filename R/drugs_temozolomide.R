# -----------------------------------------------------------------------------
# Temozolomide, oral (Ostermann 2004): plasma temozolomide in adults with
# malignant glioma
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg and concentrations in mcg/mL (= mg/L).
#
# Drafted by Claude Code, 2026-11-13, at the request of Steven L. Shafer, who
# asked for this paper (stanpumpR-papers-needed.csv, priority 2) and had it
# added to the papers archive (PMID_15173079).  Every number below was read on
# the pages of the PDF.
#
# THE SOURCE
# ==========
# Ostermann S, Csajka C, Buclin T, Leyvraz S, Lejeune F, Decosterd LA, Stupp
# R.  Plasma and cerebrospinal fluid population pharmacokinetics of
# temozolomide in malignant glioma patients.  Clin Cancer Res
# 2004;10:3728-3736 (doi 10.1158/1078-0432.CCR-03-0807).  35 adults (24 men,
# 11 women) from two trials, 30-79 years, 46-105 kg, 150-188 cm, BSA (Du
# Bois) 1.46-2.11 m2, mean 1.80 and 1.83 m2 in the two trials (Table 1):
# 75 mg/m2/day during radiotherapy (the concentrations normalised to 200
# mg/m2 by the authors, the kinetics being linear) or 200 mg/m2/day for 5 days
# every 4 weeks; 227 plasma and 47 CSF samples; HPLC-UV; NONMEM V, FO,
# ADVAN5.
#
# THE MODEL (Fig. 1, Tables 3 and 4)
# ==================================
# First-order absorption into a central compartment, with a CSF compartment:
#
#     CL = 10.0 x (1 + 0.19 SEX) + 4.2 x dBSA   L/h   (SEX 1 for a man)
#     VD = 30.3 + 19.9 x dBSA                     L
#     Ka 5.8 /h   K23 7.2e-4 /h   K32 0.76 /h   Vp (CSF) 0.14 L, fixed
#
# dBSA is the "relative deviation ... from the population mean" (Table 3,
# footnote d; Table 4, footnote g), (BSA - mean)/mean, which the text
# confirms: CL rises from 10.0 to 14.2 L/h and VD from 30.3 to 50.2 L "on
# doubling BSA" (dBSA = 1), and to 11.9 L/h in a man.  The pooled mean is not
# printed; Table 1's two means weighted by their 23 and 12 patients give 1.81
# m2, used here.  No lag; bioavailability 1, assumed by the authors (no
# intravenous arm; the literature's 97%).  Terminal half-life 2.1 h; the
# absorption half-life 7 min.
#
# The CSF compartment.  dA3/dt = K23 A2 - K32 A3 and C_CSF = A3 / 0.14 L.
# It is carried here as the PK set's peripheral compartment, exactly for the
# plasma (cl2 = K23 VD, v2 = cl2 / K32, 0.029 L at the reference), but the
# library's V2 concentration is A3 / v2, which is NOT the CSF concentration:
# at equilibrium the CSF holds K23 VD / (K32 Vp) = 0.21 of the plasma
# concentration (the paper's CSF AUC, 20% of plasma's), where A3 / v2 equals
# the plasma's.  The compartment changes plasma by about 0.1%.
#
# CHECKS (test-drugs-temozolomide.R): the reference woman's CL 10.0 L/h and
# VD 30.3 L; a man 11.9 L/h; dBSA = 1 gives 14.2 L/h and 50.2 L; terminal
# half-life 2.1 h.  The model peaks 30 min after a dose (absorption half-life
# 7 min); Fig. 2's mean profile peaks at 1.5 h, from sparse early samples, and
# the text says Ka was poorly estimated.
#
# NOT APPLIED: radiotherapy (a trend on K23, dOF -2.6, not retained); the
# anticonvulsants, corticosteroids and other comedications (no effect).  Age
# and creatinine clearance had no effect.
#
# WHAT IT DOES NOT COVER: children (fitted at 30-79 years; the additive BSA
# term is not a size model for a child, whose curve is an extrapolation); the
# intravenous product (no intravenous data here); MTIC and AIC, the products
# of temozolomide's spontaneous hydrolysis, which are not modelled.
#
# DOSE BASIS: temozolomide has no salt; doses in mg of temozolomide.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model's own covariate, body-surface area (Du Bois, as the source),
# from the pharmacokinetic weight with the switch on and total weight with it
# off.  The additive terms stay positive for any body size (dBSA > -1: CL at
# least 5.8 L/h, VD at least 10.4 L).
#
# PREGNANCY: see its registry row.  NO EFFECT SITE, NO BAND.
# -----------------------------------------------------------------------------

# Ostermann 2004, Table 4 (final model)
TEMOZOLOMIDE_OSTERMANN <- list(
  cl = 10.0, clMale = 0.19, clBsa = 4.2,   # L/h
  v = 30.3, vBsa = 19.9,                    # L
  ka = 5.8,                                 # /h
  k23 = 7.2e-4, k32 = 0.76,                 # /h, plasma to CSF and back
  bsaMean = (23 * 1.80 + 12 * 1.83) / 35    # m2, Table 1's two means pooled
)

#' Temozolomide, oral (Ostermann 2004)
#'
#' First-order absorption, one central compartment and a CSF compartment;
#' sex and body-surface area on clearance, body-surface area on volume.  See
#' the header of \code{R/drugs_temozolomide.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
temozolomide <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  p <- TEMOZOLOMIDE_OSTERMANN
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  dBsa <- bsaDuBois(w, height) / p$bsaMean - 1
  male <- if (identical(sex, "male")) 1 else 0

  cl <- p$cl * (1 + p$clMale * male) + p$clBsa * dBsa   # L/h
  v <- p$v + p$vBsa * dBsa                              # L
  q <- p$k23 * v                                        # L/h, into the CSF

  default <- list(
    v1  = v,
    v2  = q / p$k32,
    v3  = 1,                                 # placeholder: no third compartment
    cl1 = cl / 60,
    cl2 = q / 60,
    cl3 = 0,
    ka_PO = p$ka / 60,
    bioavailability_PO = 1,
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Ostermann S, Csajka C, Buclin T, et al. Plasma and cerebrospinal fluid population ",
    "pharmacokinetics of temozolomide in malignant glioma patients. Clin Cancer Res ",
    "2004;10:3728-3736. 35 adults: oral, Ka 5.8 /h, CL 10.0 x (1 + 0.19 if male) + 4.2 x ",
    "dBSA L/h, V 30.3 + 19.9 x dBSA L, dBSA the relative deviation of Du Bois BSA from ",
    "1.81 m2; a CSF compartment (K23 0.00072, K32 0.76 /h). ",
    "https://doi.org/10.1158/1078-0432.CCR-03-0807"
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
