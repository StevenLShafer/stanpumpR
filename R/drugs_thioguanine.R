# -----------------------------------------------------------------------------
# Thioguanine, oral (Tabloid): Kitchen 1997's intravenous model with an oral
# absorption derived from Lowe 2001
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg and concentrations in mcg/mL (= mg/L).
#
# Drafted by Claude Code, 2026-11-16.  Steven L. Shafer decided (2026-11-16)
# that oral thioguanine, the approved product, be derived from Kitchen 1997's
# intravenous model (R/drugs_thioguanineIV.R) and Lowe 2001's oral data, no
# oral population model having been published.  Every number below was read
# on the pages of the PDFs.  A DERIVED model: say so wherever it is used.
#
# THE SOURCES
# ===========
# Disposition: Kitchen BJ et al. Clin Cancer Res 1997;3:713-717
# (thioguanineIV: two compartments, Michaelis-Menten elimination, per m2;
# 17 patients 4-25 years by continuous infusion).
#
# Absorption: Lowe ES, Kitchen BJ, Erdmann G, et al.  Plasma
# pharmacokinetics and cerebrospinal fluid penetration of thioguanine in
# children with acute lymphoblastic leukemia.  Cancer Chemother Pharmacol
# 2001;47:199-205 (doi 10.1007/s002800000229).  41 children 1-9 years
# (median 3) with standard-risk ALL; nine samples after the first 60 mg/m2
# oral dose of maintenance (Table 2, n = 35: Cmax median 0.29 uM, mean 0.52;
# time to peak median 2.0 h; AUC median 1.0, mean 1.5 uM h, 0.18-9.5;
# harmonic-mean half-life 1.6 h); the same children's continuous infusions
# (20 mg/m2/h for 24 h, n = 33: steady state 2.7 +/- 1.4 uM, clearance 935 +/-
# 463 mL/min/m2).  No model was fitted; noncompartmental.
#
# THE DERIVATION
# ==============
# 1. Bioavailability, read from Lowe's own data as the library's rule asks
#    (F is clearance-independent within one population, not fitted to
#    another's exposure): mean oral AUC x mean intravenous clearance / dose =
#    1.5 uM h x 0.16719 mg/umol x 56.1 L/h/m2 / 60 mg/m2 = 0.234.  The two
#    means are of different courses in the same children (oral at the start
#    of maintenance, the infusions in consolidation), and the clearance is
#    the infusion's, at 2.7 uM; at an oral dose's lower concentrations
#    Kitchen's elimination is faster, so 0.234 is if anything an
#    underestimate.  It is not a measured bioavailability: Lowe printed no
#    paired or model-based F, so 0.234 is the oral input fraction that, with
#    Kitchen's nonlinear disposition, approximates Lowe's oral exposure,
#    derived with the infusion's clearance (ChatGPT chemotherapy audit F19,
#    2026-11-24).
# 2. Absorption rate: Kitchen's disposition is fast (an initial half-life of
#    about 20 min below Km), so a first-order input alone cannot both peak at
#    Lowe's 2.0 h and reach his median Cmax: a ka slow enough to peak at 2 h
#    gives a third of the Cmax.  ka is solved so that the peak height after
#    60 mg/m2 equals the median Cmax, 0.29 uM: 0.62 /h (at 0.6 m2, Lowe's
#    children's size).
# 3. Lag: the time to peak at that ka is 1.0 h, so a lag of 1.0 h moves it
#    to Lowe's median 2.0 h.
#
# CHECKS, against numbers the derivation did not use: the model's AUC after
# 60 mg/m2 is 1.03 uM h against Lowe's median 1.0 (mean 1.5, range
# 0.18-9.5), and its half-life between 4 and 8 h is 1.75 h against Lowe's
# harmonic-mean 1.6 h.
#
# WHAT IT DOES NOT COVER: oral absorption of thioguanine is erratic (Lowe's
# AUC spans 50-fold, coefficient of variation 115%), and food was not
# controlled; the curve is a median child's.  The nucleotides (TGN), the
# active species, are not modelled, nor TPMT or NUDT15 genotype, which govern
# them.  Lowe's children were 1-9 years and Kitchen's patients 4-25: the
# derivation joins two pediatric populations of the same group (NCI
# Pediatric Oncology Branch).
#
# DOSE BASIS: mg of thioguanine, as labelled.
#
# BODY SIZE: Kitchen's, per m2 of BSA (see R/drugs_thioguanineIV.R).
#
# PREGNANCY: see its registry row.  NO EFFECT SITE, NO BAND.
# -----------------------------------------------------------------------------

# The derivation above; Lowe 2001, Table 2 and Results
THIOGUANINE_ORAL <- list(
  f = 1.5 * 167.19 / 1000 * (935 * 60 / 1000) / 60,   # 0.234 (MW 167.19, as thioguanineIV)
  ka = 0.62,                                                    # /h
  lag = 60                                                      # min
)

#' Thioguanine, oral (Kitchen 1997 and Lowe 2001)
#'
#' Kitchen's two-compartment Michaelis-Menten disposition per m2 with an
#' oral absorption derived from Lowe 2001; see the header of
#' \code{R/drugs_thioguanine.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects, with a
#'   \code{michaelisMenten} block
#' @export
thioguanine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  X <- thioguanineIV(weight, height, age, sex, adjustToFFM)
  o <- THIOGUANINE_ORAL
  X$PK$default$ka_PO <- o$ka / 60
  X$PK$default$bioavailability_PO <- o$f
  X$PK$default$tlag_PO <- o$lag
  X$reference <- paste0(
    "Oral thioguanine, DERIVED: Kitchen BJ et al. Clin Cancer Res 1997;3:713-717 (two ",
    "compartments, Michaelis-Menten elimination, per m2) with absorption from Lowe ES et al. ",
    "Cancer Chemother Pharmacol 2001;47:199-205 (children 1-9 years): F 0.234 (Lowe's oral AUC x ",
    "his intravenous clearance / dose), ka 0.62 /h and a 1.0-h lag (Lowe's median Cmax and time ",
    "to peak). https://doi.org/10.1007/s002800000229"
  )
  X
}
