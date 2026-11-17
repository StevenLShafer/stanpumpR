# -----------------------------------------------------------------------------
# Paclitaxel, intravenous, solvent-based (Joerger 2006): total plasma
# paclitaxel, three compartments with saturable elimination and saturable
# distribution
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg and concentrations in mcg/mL (= mg/L), so the maximal rates are in
# mg/min and the Michaelis constants in mg/L.  The source works in umol and
# hours; every value is converted exactly at the molecular weight, 853.9
# g/mol (1 umol = 0.8539 mg), as GHB's source values are rescaled.
#
# Drafted by Claude Code, 2026-11-06, at the request of Steven L. Shafer,
# who decided that day to extend the Michaelis-Menten engine for it
# (docs/chemotherapy.md, inst/extdata/chemotherapyRegistry.csv, source P17).
# Every number below was read on the page images of the PDF (pp. 2151-2155).
#
# THE SOURCE
# ==========
# Joerger M, Huitema ADR, van den Bongard DHJG, Schellens JHM, Beijnen JH.
# Quantitative effect of gender, age, liver function, and body size on the
# population pharmacokinetics of paclitaxel in patients with solid tumors.
# Clin Cancer Res 2006;12:2150-2157 (doi 10.1158/1078-0432.CCR-05-2069).
# 168 adults (82 women) with solid tumours (84 NSCLC, 40 ovarian), 280
# courses; age 33-86 (median 56) years, BSA 1.35-2.25 (median 1.80) m2,
# total bilirubin 2-24 (median 7) umol/L.  Taxol (Cremophor EL), 100-250
# mg/m2 over 3 h (163 patients) or 135-175 mg/m2 over 24 h (5), PVC-free
# sets; total paclitaxel in heparin plasma by HPLC (10-10,000 ng/mL), sampled
# to 48 h after a 3-h infusion.  NONMEM V, FO, ADVAN6.
#
# THE MODEL (Table 2, Equations A-F)
# ==================================
# "Three-compartment model with saturable elimination from the central
# compartment, saturable transport to the first peripheral compartment, and
# linear distribution to the second peripheral compartment."  The ODEs are not
# printed; with amounts A in umol, C1 = A1/V1 and R the infusion rate:
#
#   dA1/dt = R - VMEL C1/(KMEL + C1) - VMTR C1/(KMTR + C1) + K21 A2
#              - Q C1 + Q A3/V3
#   dA2/dt = VMTR C1/(KMTR + C1) - K21 A2
#   dA3/dt = Q C1 - Q A3/V3
#
# Compartment 2 has no volume: it is an amount, filled saturably and emptied
# at first order (the structure of Henningsson 2001, the paper's ref 31).
# The typical woman at BSA 1.8 m2, bilirubin 7 umol/L and age 56:
#
#   V1 = 12.8 L                          (no covariate)
#   V3 = 252 L x (BSA/1.8)^1.17
#   VMEL = 37.4 umol/h x 1.2^G x (BSA/1.8)^0.842 x (bili/7)^-0.167
#          x (age/56)^-0.352
#   KMEL = 0.53 umol/L                   (no covariate)
#   VMTR = 169 umol/h x 1.2^G x (BSA/1.8)^0.911
#   KMTR = 0.826 umol/L x 2.11^G
#   K21  = 1.15 /h x 0.893^G
#   Q    = 20.1 L/h x (BSA/1.8)^0.724
#
# with G = 1 for a man.  The engine runs it as advanceMichaelisMenten()'s
# saturable elimination (vmax, km), its saturableDistribution block (VMTR,
# KMTR, K21; added for this drug) and the PK set's linear peripheral
# compartment (cl2 = Q, v2 = V3; the library's "V2" is the paper's V3).
#
# CHECKS
# ======
# 175 mg/m2 over 3 h at BSA 1.8 m2, age 56, bilirubin 7, against an
# independent integration (the spec's, and the test's own Runge-Kutta):
# woman Cmax 3.83 umol/L (ln 1.343; Fig. 3's median about 1.32), time above
# 0.1 umol/L 17.7 h (Fig. 3 about 16.5), AUC 13.5 umol h/L; man 3.29 (ln
# 1.192; Fig. 3 about 1.17), 13.0 h (about 12.7), 11.4.  The time above
# 0.1 umol/L is flat across BSA 1.35-2.25, as Fig. 3 shows, and the
# bilirubin-24 and age-86 women give 23.7 and 22.0 h (Fig. 4 about 22.4 and
# 21).  The typical value lies a little above the simulated population
# medians, as it should with interindividual variability.  The text's
# covariate statements all reproduce (a man +20% VMEL; +0.2 m2 BSA +9%; +10
# years -5%; +10 umol/L bilirubin -14%).
#
# MISPRINTS
# =========
# 1. The Results (p. 2153) swap the labels: "K_EL = 0.83 umol/L for females
#    and 1.74 for males ... nonlinear elimination (K_TR = 0.53)".  By Table 2
#    and Equation D the sex-dependent 0.83/1.74 is KMTR, the distribution
#    constant, and 0.53 is KMEL; the Discussion has them right, and with the
#    labels swapped the time above 0.1 umol/L comes out 27-43 h, against
#    Figs. 3-4's 13-22 h.
# 2. The Methods give Q "in L/min"; Table 2 and Equation F say L/h, and only
#    L/h reproduces Figs. 3-4.
# 3. The Methods' generic covariate form "(WT - 68)^theta" is a misprint for
#    (WT/68)^theta; weight is not in the final model.
# 4. Table 2's KMTR 0.83 is Equation D's 0.826 rounded; the equation is used.
# 5. Fig. 3's Cmax axis (about 1.0-1.45 "umol/L") is ln(Cmax).
#
# HELD AT THE REFERENCE
# =====================
# Total bilirubin, which the Patient Profile does not have, is held at the
# study median, 7 umol/L (0.41 mg/dL).  A patient with bilirubin 24 umol/L
# has a VMEL 19% lower and a longer time above 0.1 umol/L (23.7 h for the
# typical woman) than the curve shows.  Concurrent carboplatin (some NSCLC
# patients) was not a covariate.
#
# AGE: the age term is a negative power, floored at 18 (ADULT_AGE_TERM_FLOOR;
# the source's youngest patient was 33).
#
# DOSE BASIS: paclitaxel has no salt; doses in mg of paclitaxel.  The model is
# for solvent-based (Cremophor) paclitaxel and does not transfer to nab-
# paclitaxel, whose disposition differs; Cremophor's own nonlinearity is part
# of what the saturable terms describe.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model carries its own size covariate, BSA, on V3, VMEL, VMTR and Q.
# The source does not name its BSA formula; Mosteller's is used, as for the
# library's other chemotherapy models, at the pharmacokinetic weight with the
# switch on and at total body weight with it off.  V1, which carries no
# covariate, is FIXED at 12.8 L in both switch positions, on Shafer's
# decision (2026-11-06): the source found that neither sex nor size changes
# it, and scaling it by the library's volume factor gave the reference woman
# 10.5 L, against the source's own finding.  KMEL, a concentration, does not
# scale.
#
# PREGNANCY (R/pregnancy.R)
# =========================
# Janssen JM, Van Calsteren K, Dorlo TPC, et al.  Population pharmacokinetics
# of docetaxel, paclitaxel, doxorubicin and epirubicin in pregnant women.  Clin
# Pharmacokinet 2021;60:775-784 (doi 10.1007/s40262-020-00961-4), PDF read
# 2026-11-10: 20 pregnant women (25 cycles) and 689 nonpregnant patients, refitted on
# a model of this one's structure (Crombag 2019), one discrete pregnancy
# factor per parameter (Table 2): KMEL x 1.32 (95% CI 1.03-1.69), V1 x 2.27
# (1.57-3.10) and V3 x 2.28 (1.54-3.63).  Its text calls the linear
# compartment V2, but Table 2 and its footnote give the saturable compartment
# as the first peripheral (VMTR, KMTR, K21, no volume) and the LINEAR one as V3
# (267 L, Q2 16.9 L/h), which is this model's linear compartment (V3 252 L, Q
# 20.1), carried here as v2: so KMEL goes on km, V1 on v1 and V3 on v2.
# Applied under `pregnant` on Shafer's decision (2026-11-10).  V1 is fixed in
# both switch positions; the factor multiplies it.
# NO EFFECT SITE, NO BAND: the time above 0.05 or 0.1 umol/L, the exposure
# measure of neutropenia, is not a therapeutic range and is not drawn.
# -----------------------------------------------------------------------------

# Joerger 2006, Table 2 and Equations A-F: the typical woman, BSA 1.8 m2,
# bilirubin 7 umol/L, age 56; amounts in umol, time in hours
PACLITAXEL_JOERGER <- list(
  v1 = 12.8,                                    # L
  v3 = 252,  v3Bsa = 1.17,                      # L
  vmel = 37.4, vmelMale = 1.2, vmelBsa = 0.842, # umol/h
  vmelBili = -0.167, vmelAge = -0.352,
  kmel = 0.53,                                  # umol/L
  vmtr = 169, vmtrMale = 1.2, vmtrBsa = 0.911,  # umol/h
  kmtr = 0.826, kmtrMale = 2.11,                # umol/L
  k21 = 1.15, k21Male = 0.893,                  # /h
  q = 20.1, qBsa = 0.724,                       # L/h
  bsaReference = 1.8,                           # m2
  ageReference = 56,                            # years
  bilirubinReference = 7,                       # umol/L
  bilirubin = 7                                 # held: no Patient Profile field
)
# Janssen 2021, Table 2: paclitaxel's pregnancy factors on KMEL, V1 and the
# linear peripheral volume (Janssen's V3, this model's v2)
PACLITAXEL_PREGNANCY <- list(km = 1.32, v1 = 2.27, vLinear = 2.28)
PACLITAXEL_MW <- 853.9   # g/mol: 1 umol = 0.8539 mg

#' Paclitaxel, intravenous, solvent-based (Joerger 2006)
#'
#' Three compartments with saturable elimination, saturable distribution to
#' a first peripheral compartment and linear distribution to a second; sex,
#' body surface area and age as covariates, run by
#' \code{advanceMichaelisMenten()}.  See the header of
#' \code{R/drugs_paclitaxelJoerger.R}.
#'
#' @inheritParams cefazolin
#' @param pregnant TRUE in late pregnancy: Janssen 2021's factors
#' @returns a list in the shape \code{getDrugPK()} expects, with a
#'   \code{michaelisMenten} block
#' @export
paclitaxelJoerger <- function(weight, height, age, sex, adjustToFFM = TRUE, pregnant = FALSE)
{
  p <- PACLITAXEL_JOERGER
  preg <- if (isTRUE(pregnant)) PACLITAXEL_PREGNANCY else list(km = 1, v1 = 1, vLinear = 1)
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  bsa <- bsaMosteller(w, height) / p$bsaReference
  male <- if (identical(sex, "male")) 1 else 0
  ageTerm <- max(age, ADULT_AGE_TERM_FLOOR) / p$ageReference
  mg <- PACLITAXEL_MW / 1000                       # mg per umol

  vmel <- p$vmel * p$vmelMale^male * bsa^p$vmelBsa *
    (p$bilirubin / p$bilirubinReference)^p$vmelBili * ageTerm^p$vmelAge  # umol/h
  vmtr <- p$vmtr * p$vmtrMale^male * bsa^p$vmtrBsa                       # umol/h
  kmtr <- p$kmtr * p$kmtrMale^male                                       # umol/L
  k21 <- p$k21 * p$k21Male^male                                          # /h
  v3 <- p$v3 * bsa^p$v3Bsa                                               # L
  q <- p$q * bsa^p$qBsa                                                  # L/h

  vmax <- vmel * mg / 60                           # mg/min
  km <- p$kmel * mg * preg$km                      # mg/L

  default <- list(
    v1 = p$v1 * preg$v1,                        # fixed (Shafer, 2026-11-06)
    v2 = v3 * preg$vLinear,                  # the source's V3, linear
    v3 = 1,                                  # placeholder: see the header
    # The low-concentration limit of the saturable elimination, Vmax / Km,
    # for the help tables only: the simulation uses the michaelisMenten block.
    cl1 = vmax / km,
    cl2 = q / 60,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Joerger M, Huitema ADR, van den Bongard DHJG, Schellens JHM, Beijnen JH. ",
    "Quantitative effect of gender, age, liver function, and body size on the ",
    "population pharmacokinetics of paclitaxel in patients with solid tumors. ",
    "Clin Cancer Res 2006;12:2150-2157. 168 adults: three compartments, saturable ",
    "elimination (VMEL 37.4 umol/h x 1.2^male x (BSA/1.8)^0.842 x (bilirubin/7)^-0.167 ",
    "x (age/56)^-0.352, KMEL 0.53 umol/L), saturable distribution to a first ",
    "peripheral compartment (VMTR 169 umol/h x 1.2^male x (BSA/1.8)^0.911, KMTR ",
    "0.826 umol/L x 2.11^male, K21 1.15 /h x 0.893^male) and linear distribution ",
    "to a second (Q 20.1 L/h x (BSA/1.8)^0.724, V3 252 L x (BSA/1.8)^1.17), V1 12.8 L ",
    "(Mosteller BSA; bilirubin held at 7 umol/L; age floored at 18). Solvent-based ",
    "paclitaxel only. Total plasma paclitaxel. ",
    "https://doi.org/10.1158/1078-0432.CCR-05-2069"
  )
  if (isTRUE(pregnant))
    reference <- paste(reference, pregnancyReference("KMEL x 1.32, V1 x 2.27, linear V x 2.28",
      "Janssen 2021, Clin Pharmacokinet 60:775-784, https://doi.org/10.1007/s40262-020-00961-4"))

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference,
    michaelisMenten = list(
      vmax = vmax, km = km,
      saturableDistribution = list(
        vmax = vmtr * mg / 60,                   # mg/min
        km = kmtr * mg,                          # mg/L
        k21 = k21 / 60                           # /min
      )
    )
  )
}
