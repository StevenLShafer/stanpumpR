# -----------------------------------------------------------------------------
# Naproxen: two-compartment apparent oral model (Valitalo 2012), re-centred
# allometrically, with the analgesic EC50 of Bjornsson 2011 as the threshold
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), total plasma naproxen.  Doses are mg of
# naproxen: 550 mg of naproxen sodium is 500 mg of naproxen, and a 220 mg
# over-the-counter tablet is 200 mg.
#
# SOURCE
# ======
# Valitalo et al. gave 53 healthy children (3 months to 12 years; median
# weight 20 kg in the 39 boys, 24 kg in the 14 girls) one 10 mg/kg dose of
# naproxen suspension before surgery under spinal anaesthesia and sampled
# plasma to 51 h (270 concentrations).  NONMEM VI, FOCE-I.  Their final
# ("empirical") model, Table II, outlier excluded:
#
#     CL/F = 0.62 x (WT/70)   L/h       V1/F = 8.2 x (WT/70)   L
#     Q/F  = 0.14             L/h       V2/F = 4.3 x (WT/70)   L
#     ka   = 1.1 /h, no lag             fraction unbound 0.0014
#
# There is no intravenous naproxen, so every parameter is apparent (/F) and
# bioavailability_PO is 1: the apparent scale already contains F (oral
# absorption is rapid and complete, Davies 1997).  The CSF compartment of the
# source is not modelled.
#
# RE-CENTRING FOR ADULTS (decision of Steven L. Shafer, 2026-10-11)
# =================================================================
# Linear weight scaling, fitted in children of median 20 kg, gives a 70 kg
# adult a clearance of 0.62 L/h, above what adults are measured to have
# (below).  The model is therefore re-expressed about its median child:
# Valitalo's typical values at 20 kg, scaled to other sizes allometrically,
# weight^0.75 for clearances and weight^1 for volumes.  The authors' own
# "mechanistic" model scaled CL/F and Q/F the same way and fitted the data as
# well (OFV 0.8 lower, one parameter more).  A 20 kg child receives Table II
# exactly; the 70 kg reference man:
#
#     CL/F = 0.62 x (20/70)^0.25 = 0.453 L/h   V1/F = 8.2 L
#     Q/F  = 0.14 x (70/20)^0.75 = 0.358 L/h   V2/F = 4.3 L
#
# Centring on the girls' median, 24 kg, would raise the adult clearance 5%.
# The source did not find Q/F to change with weight (exponent -0.16, RSE
# 240%), so scaling it is theory, not data; left unscaled, Q/F 0.14 L/h
# would give an adult a terminal half-life of 32 h.
#
# The re-centred model's half-lives in the reference man are 4.6 h and 23 h.
# In the 20 kg child they are 3.3 and 16.7 h.
#
# CHECKS AGAINST ADULT DATA
# =========================
# 500 mg once, reference man: AUC 1103 mg.h/L (CL/F 0.453 L/h).  Healthy
# adults: 1206 mg.h/L to 72 h for enteric-coated naproxen 500 mg (Choi 2015,
# n = 66); CL/F 0.416 L/h in young men after 375 mg (Upton 1984).  Peak 48
# mg/L at 2.5 h; the enteric-coated tablet peaks at 62 mg/L (Choi 2015), so
# the model is about 20% low at the peak in adults, plausibly because an
# adult's central volume per kg is smaller than a child's.  Terminal half-life 23 h; the label gives 12-17 h,
# Vree 1993 24.7 +/- 6.4 h (range 7-36 h) in 10 adults given 500 mg.  Steady
# state: 90% reached in 3 days of twice-daily dosing (the label: 4 to 5
# days).
#
# SATURABLE BINDING (NOT REPRESENTED)
# ===================================
# Naproxen is more than 99% bound to albumin, and above about 500 mg a day
# the unbound fraction rises, so the total clearance rises and total
# concentrations increase less than in proportion to the dose, while unbound
# clearance stays constant (Davies 1997, Runkel 1974).  Valitalo found the
# fraction unbound constant (0.14%) over 3 to 147 mg/L in children; Bjornsson
# fitted saturable binding in adults.  The model is linear in total drug, so
# it overpredicts total concentrations at the higher doses: at 375 mg twice
# daily the steady-state mean is 69 mg/L against 58 measured in young men
# (Upton 1984), and at 500 mg twice daily 92 against 75 (AUC 896 mg.h/L over
# 12 h, van den Ouweland 1987).
#
# EFFECT AND THRESHOLD
# ====================
# No effect site: tPeak = 0, and the plotted concentration is plasma.
# Bjornsson and Simonsson related pain intensity after wisdom-tooth removal
# (242 adults; naproxen 500 mg, naproxcinod or placebo) directly to the
# UNBOUND plasma naproxen concentration, with no delay: a sigmoid Emax model,
# Emax fixed at 1, EC50 0.135 umol/L (RSE 10%, between-subject variability
# 120%), shape 1.61.  The paper's text renders micro as "m" in some copies;
# its assay ranges (total 1.5-400, unbound 12.5-3200 nmol/L) show the units
# are umol/L.  Through their own binding model (Bmax 643 umol/L, Km 0.549
# umol/L unbound) the EC50 is 127 umol/L, 29 mg/L, of total naproxen, the
# concentration that halves the drug-attributable pain.  That is the
# time-until-threshold level (endCe in the CSV), timed on the plasma.  No
# band is drawn.  Their own kinetic model is not used: fitted to 8 h of data,
# it gives total naproxen a half-life of 6 to 8 h, which the authors caution
# against extrapolating.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The library's fat-free-mass scaling on the 70 kg values with the switch
# on; with it off, the allometry on total weight about the median child
# (legacyVolume = W / 70, legacyClearance = (W / 70)^0.75), which gives a
# 20 kg child Table II exactly.  Valitalo's published linear weight scaling
# of clearance is not reproduced in either position: see RE-CENTRING.
#
# NOT MODELLED
# ============
# Saturable binding (above), age and hypoalbuminaemia (which raise the unbound
# fraction; the unbound clearance of the elderly is half that of the young,
# Upton 1984), renal and hepatic impairment, CYP2C9 and CYP1A2 genotype,
# formulations (the source used a suspension; enteric-coated and
# delayed-release tablets peak later), food, and infants under 3 months,
# whose clearance is still maturing.
#
# References
# ----------
# Valitalo P et al., J Clin Pharmacol 2012;52:1516-1526.
#   https://doi.org/10.1177/0091270011418658
# Bjornsson MA, Simonsson USH, Br J Clin Pharmacol 2011;71:899-906.
#   https://doi.org/10.1111/j.1365-2125.2011.03924.x
# Choi Y et al., Drug Des Devel Ther 2015;9:4127-4135.
#   https://doi.org/10.2147/DDDT.S86725
# Upton RA et al., Br J Clin Pharmacol 1984;18:207-214.
#   https://doi.org/10.1111/j.1365-2125.1984.tb02454.x
# van den Ouweland FA et al., Br J Clin Pharmacol 1987;23:189-193.
#   https://doi.org/10.1111/j.1365-2125.1987.tb03028.x
# Vree TB et al., Br J Clin Pharmacol 1993;35:467-472.
#   https://doi.org/10.1111/j.1365-2125.1993.tb04171.x
# Davies NM, Anderson KE, Clin Pharmacokinet 1997;32:268-293.
#   https://doi.org/10.2165/00003088-199732040-00002
# -----------------------------------------------------------------------------

NAPROXEN_CENTRE_WEIGHT <- 20    # kg, median weight of Valitalo's boys

#' Naproxen pharmacokinetics (oral)
#'
#' Valitalo et al. (2012): two compartments with first-order absorption,
#' apparent oral parameters, re-centred allometrically on the source's median
#' child.  See the header of \code{R/drugs_naproxen.R}.
#'
#' @inheritParams cefazolin
#' @param adjustToFFM when \code{TRUE}, scale the 70 kg values to fat-free
#'   mass; when \code{FALSE}, allometrically on total body weight
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
naproxen <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Size scaling (see the header)
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyClearance = (weight / 70)^0.75)

  # Valitalo Table II at the median child, scaled allometrically to 70 kg
  centre <- NAPROXEN_CENTRE_WEIGHT
  cl70 <- 0.62 * (centre / 70) * (70 / centre)^0.75   # L/h, 0.453
  q70  <- 0.14 * (70 / centre)^0.75                   # L/h, 0.358

  v1  <- 8.2 * size$volume
  v2  <- 4.3 * size$volume
  cl1 <- cl70 / 60 * size$clearance                   # L/min
  cl2 <- q70 / 60 * size$clearance
  v3  <- 1                                            # two compartments
  cl3 <- 0

  ka_PO              <- 1.1 / 60   # 1/min, no lag
  bioavailability_PO <- 1          # apparent (/F) parameters
  tlag_PO            <- 0

  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3,
    ka_PO = ka_PO,
    bioavailability_PO = bioavailability_PO,
    tlag_PO = tlag_PO
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Valitalo P et al., J Clin Pharmacol 2012;52:1516-1526 (two-compartment ",
    "apparent oral model, fitted in children, re-centred allometrically on ",
    "its median child); analgesic EC50 from Bjornsson MA, Simonsson USH, ",
    "Br J Clin Pharmacol 2011;71:899-906; oral only. ",
    "https://doi.org/10.1177/0091270011418658"
  )

  return(
    list(
      PK = PK,
      # No effect site: the effect follows the plasma (see the header)
      tPeak = 0,
      # Not an opioid, so not on the MEAC panel
      MEAC = 0,
      # No band; the threshold is endCe in the CSV, 29 mg/L
      typical = 0,
      upperTypical = 0,
      lowerTypical = 0,
      reference = reference
    )
  )
}
