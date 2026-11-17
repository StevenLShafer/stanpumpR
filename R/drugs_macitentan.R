# -----------------------------------------------------------------------------
# Macitentan (Opsumit): one compartment, oral by three parallel absorption
# paths, forming its active metabolite aprocitentan, from Bartolucci 2021
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg, so the plotted concentrations are ng/mL (= mcg/L), total plasma.  The
# metabolite is its own drug, aprocitentan (R/drugs_aprocitentan.R).
#
# Drafted by Claude Code, 2026-11-16, at the request of Steven L. Shafer, who
# supplied the paper that day.  Source, read in full: Bartolucci R, Dosne AG,
# Csonka D, Perez-Ruixo JJ, Magni P, Poggesi I.  A population pharmacokinetic
# model of macitentan and its active metabolite aprocitentan in healthy
# volunteers and patients with pulmonary arterial hypertension.  Clin
# Pharmacokinet 2021;60:1605-1619.  doi 10.1007/s40262-021-01049-3.
#
# THE SOURCE
# ==========
# 9,388 concentrations from 452 subjects in nine studies (166 healthy
# volunteers, 286 patients with pulmonary arterial hypertension; median 68.8
# kg, 40 years, 57% women; 0.2-600 mg), NONMEM.  One compartment for each
# moiety; macitentan's apparent clearance CL1 is its total, of which 62% forms
# aprocitentan ("assumed to be 62%, based on in vitro findings and a
# physiologically based PK analysis", p.1607).  Three parallel absorption
# paths (Figure 1):
#
#   path 1, (1 - FPM) FMAC of the dose: zero-order into a depot over D1, then
#           ka1 into macitentan's compartment;
#   path 2, (1 - FPM)(1 - FMAC): after a lag Tlag2, ka2 into macitentan's;
#   path 3, FPM: zero-order into a depot over D3, then ka3 into
#           APROCITENTAN's compartment ("a fraction of the macitentan dose to
#           be directly absorbed as aprocitentan"),
#
#   logit(FMAC) = gamma - beta log(1 + Dose).
#
# Table 3, final model (FOCEI):
#
#     ka1 0.312 /h   ka2 0.0502 /h   ka3 0.381 /h
#     D1 2.77 h      Tlag2 11.6 h    D3 3.46 h      FPM 0.0628
#     gamma 3.73     beta 0.866      alpha 0.00168
#     V1 34.4 L x (BW/68.8)^0.81       CL1 1.39 L/h x (BW/68.8)^0.54
#     V2 23.5 L x (BW/68.8)^0.86       CL2 0.321 L/h x (BW/68.8)^0.70
#     CL1 x (1 + 0.27) in healthy volunteers
#     CL2 x (1 - 0.131) in moderate or severe renal impairment, x (1 + 0.157)
#         in men, x (1 - 0.158) in Asians; V1 x (1 - 0.219), V2 x (1 - 0.163)
#         in Asians
#
# Categorical covariates are fractional changes from "the most frequent"
# category (footnote c), which is the clinical case: a patient with PAH
# (63%), on the tablet (81%), female (57%), not Asian, with normal or mildly
# impaired renal and hepatic function.  Held: PAH (not the healthy
# volunteer's +27%), the tablet, not Asian (the Patient Profile has no race).
# Applied: sex; weight; and renal impairment from the Patient Profile's
# creatinine by the paper's own MDRD equation (egfrMDRD(), Table 2 footnote
# c), moderate or severe below 60 mL/min/1.73 m2, a step as published.
#
# Three readings, recorded as decisions in the PR (2026-11-16):
#   - Figure 1 and Table 3 give path 2 a lag and no zero-order duration; the
#     text speaks of "a duration D2" that no table prints.  Path 2 is a
#     lagged first-order depot.
#   - The footnote prints Ftotal = exp(alpha (Dose - 10)) and calls it a
#     "decrease in total bioavailability", where alpha is positive; the text
#     says exposure above 25 mg was "slightly lower than expected".  The
#     decrease is applied, Ftotal = exp(-alpha (Dose - 10)), as oralSaturation's
#     exponential form (Fmin 0, Fmax exp(10 alpha), D50 ln 2 / alpha).  It is
#     1 at the labelled 10 mg.
#   - The footnote's FMAC "0.81 for a dose of 10 mg" is the BASE model's (2.251
#     and 0.335); the final model's gamma and beta give 0.84, which is used.
#
# The amounts are mass: no molar conversion appears in the source, so the 62%
# and FPM carry mass (mwRatio 1).
#
# THE ENGINE
# ==========
# Linear, but no closed-form route carries three absorption paths of
# different shapes with one emptying into the metabolite, so the parent and
# aprocitentan are advanced together by the saturable engine
# (R/advanceMichaelisMenten.R) with vmax 0, clLinear CL1, its in-engine
# metabolite and its oralPaths (ORAL PATHS in its header).
#
# CHECKS
# ======
# The abstract's description of the model's typical woman with PAH (10 mg,
# the same model, so an implementation check): macitentan's peak 9 h after a
# dose and two-fold accumulation; aprocitentan's peak at 51 h and 12.5-fold
# accumulation; this model, a 68.8-kg woman: 9.1 h and 2.13; 47.7 h and 12.4.
# The Opsumit label (2025, not fitted): macitentan's peak "about 8 hours",
# half-lives "approximately 16 hours and 48 hours" (here 17.2 and 50.7 h), and
# at steady state the metabolite's exposure "3-times the exposure to
# macitentan" (here 2.98).
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source's own weight terms, at the pharmacokinetic weight with the switch
# on and total weight with it off.
#
# DOSE
# ====
# Opsumit tablets, 10 mg of macitentan (no salt), once daily.  Strong CYP3A4
# inducers and inhibitors are not modelled.
#
# PREGNANCY AND THE BAND
# ======================
# Contraindicated in pregnancy; no pregnancy pharmacokinetics (registry row).
# No therapeutic range.
# -----------------------------------------------------------------------------

# Bartolucci 2021, Table 3 (final model)
MACITENTAN_KA     <- c(0.312, 0.0502, 0.381)   # /h, paths 1-3
MACITENTAN_D1     <- 2.77    # h
MACITENTAN_TLAG2  <- 11.6    # h
MACITENTAN_D3     <- 3.46    # h
MACITENTAN_FPM    <- 0.0628
MACITENTAN_GAMMA  <- 3.73
MACITENTAN_BETA   <- 0.866
MACITENTAN_ALPHA  <- 0.00168 # /mg
MACITENTAN_V1     <- 34.4    # L
MACITENTAN_CL1    <- 1.39    # L/h
MACITENTAN_V1_WT  <- 0.81
MACITENTAN_CL1_WT <- 0.54
MACITENTAN_FORMED <- 0.62    # fraction of CL1 forming aprocitentan
MACITENTAN_WT_REF <- 68.8    # kg, the median

#' Macitentan (Bartolucci 2021)
#'
#' One compartment, oral by three parallel paths, forming aprocitentan; see the
#' header of \code{R/drugs_macitentan.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine, mg/dL, or NULL for an assumed normal
#'   value
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
macitentan <- function(weight, height, age, sex, adjustToFFM = TRUE,
                       creatinine = NULL)
{
  wt <- macitentanWeight(weight, height, age, sex, adjustToFFM) / MACITENTAN_WT_REF
  v1 <- MACITENTAN_V1 * wt^MACITENTAN_V1_WT
  cl1 <- MACITENTAN_CL1 * wt^MACITENTAN_CL1_WT / 60
  apro <- aprocitentan(weight, height, age, sex, adjustToFFM, creatinine)$PK$default

  default <- list(
    v1  = v1,
    v2  = 1,
    v3  = 1,
    cl1 = cl1,
    cl2 = 0,
    cl3 = 0,
    ka_PO = MACITENTAN_KA[1] / 60,  # the first path's, for the help tables
    bioavailability_PO = 1,         # apparent parameters
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  fmac <- c(MACITENTAN_GAMMA, MACITENTAN_BETA)
  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    # Ftotal = exp(-alpha (Dose - 10)): 1 at 10 mg (see the header)
    oralSaturation = list(form = ORAL_SATURATION_EXPONENTIAL, Fmin = 0,
                          Fmax = exp(10 * MACITENTAN_ALPHA),
                          D50 = log(2) / MACITENTAN_ALPHA),
    michaelisMenten = list(
      vmax = 0,
      km = 1,                      # unused: no saturable elimination
      clLinear = cl1,
      oralPaths = list(
        list(share = list(constant = 1 - MACITENTAN_FPM, logit = fmac),
             zeroOrder = MACITENTAN_D1 * 60, ka = MACITENTAN_KA[1] / 60),
        list(share = list(constant = 1 - MACITENTAN_FPM, logit = fmac,
                          complement = TRUE),
             lag = MACITENTAN_TLAG2 * 60, ka = MACITENTAN_KA[2] / 60),
        list(share = list(constant = MACITENTAN_FPM),
             zeroOrder = MACITENTAN_D3 * 60, ka = MACITENTAN_KA[3] / 60,
             into = "metabolite")
      ),
      # aprocitentan's disposition is its own model's (R/drugs_aprocitentan.R)
      metabolite = list(
        name = "aprocitentan",
        fraction = MACITENTAN_FORMED,
        v1 = apro$v1,
        cl = apro$cl1,
        mwRatio = 1                # the source's amounts are mass
      )
    ),
    reference = MACITENTAN_REFERENCE
  )
}

#' The weight macitentan's and aprocitentan's terms read: the pharmacokinetic
#' weight with the switch on, total weight with it off
#' @keywords internal
macitentanWeight <- function(weight, height, age, sex, adjustToFFM)
{
  if (isTRUE(adjustToFFM)) pkSizeFactors(weight, height, age, sex, TRUE)$pkWeight
  else weight
}

MACITENTAN_REFERENCE <- paste0(
  "Bartolucci R, Dosne AG, Csonka D, Perez-Ruixo JJ, Magni P, Poggesi I, Clin ",
  "Pharmacokinet 2021;60:1605-1619, Table 3: macitentan one compartment (V1 34.4 L, ",
  "CL1 1.39 L/h, 62% forming aprocitentan) absorbed by three paths (zero-order over ",
  "2.77 h then ka 0.312 /h; after 11.6 h, ka 0.0502 /h; 6.28% zero-order over 3.46 h ",
  "then ka 0.381 /h into aprocitentan; logit FMAC 3.73 - 0.866 log(1 + dose)); ",
  "aprocitentan one compartment (V2 23.5 L, CL2 0.321 L/h); weight about 68.8 kg; ",
  "a woman with PAH on the tablet. https://doi.org/10.1007/s40262-021-01049-3")
