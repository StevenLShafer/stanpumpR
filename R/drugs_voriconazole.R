# -----------------------------------------------------------------------------
# Voriconazole (Friberg 2012): two compartments, linear plus Michaelis-Menten
# elimination with a Vmax that declines over the first days, CYP2C19
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg, so the plotted concentration is mg/L (mcg/mL).
#
# Drafted by Claude Code, 2026-11-12, at the request of Steven L. Shafer, who
# asked for the common drugs with narrow therapeutic windows that the library
# lacked.  The numbers below were read from the full text of the source (the
# PubMed Central page of PMC3370730: the model equations, Table 3 and its
# footnotes) and from the VFEND label (NDA 021266, 2026).
#
# THE SOURCE
# ==========
# Friberg LE, Ravva P, Karlsson MO, Liu P.  Integrated population
# pharmacokinetic analysis of voriconazole in children, adolescents, and
# adults.  Antimicrob Agents Chemother 2012;56:3032-3042 (doi
# 10.1128/AAC.05761-11).  112 immunocompromised children (2 to <12 years), 26
# adolescents (12 to <17) and 35 healthy adults (22-55 years, 49-97 kg; 6
# mg/kg IV q12h on day 1, 4 mg/kg IV q12h to day 7, then 200 mg orally q12h),
# rich plasma sampling, NONMEM.  "A two-compartment model with first-order
# absorption and mixed linear (first-order) and nonlinear (Michaelis-Menten;
# time-dependent Vmax) elimination":
#
#     Vmax = Vmax,1 (1 - Vmax,inh (T - 1) / ((T - 1) + (T50 - 1)))
#
# with T "the time in hours after initiation of dosing" and Vmax,1 the
# value at 1 h.  Table 3 (estimate, %RSE), all scaled to 70 kg, clearances to
# the power 0.75 and volumes to the power 1:
#
#     Km        1.15 ug/mL           Vmax,1   114 mg/h
#     logit(Vmax,inh)  1.50, and -0.390 more under 12 years
#     T50       2.41 h               CL (linear)  6.16 L/h
#     V2 (central)  79.0 L           V3 (peripheral) 103 L
#     Q         15.5 L/h, x (1 + 0.637) in children and adolescents
#     logit(F1) 0.585                ka  100 /h (fixed) in adults; 1.19 /h in
#                                    children, x (1 - 0.615) in adolescents
#     Alag      0.949 h in adults, x (1 - 0.874) in children and adolescents
#
# So Vmax,inh is 81.8% in an adult (75.2% under 12) and F1 is 64.2%.  The
# children of the source's first study had a lower Km and Vmax,1; that study
# effect is not applied.  Footnote: "Vmax,inh = 100% if the adult is a
# CYP2C19 HEM or PM".  The Patient Profile's CYP 2C19 field is read for adults
# only, as the source did: poor and intermediate metabolisers (the source's
# heterozygous extensive and poor metabolisers, pooled) lose all of Vmax with
# time, so at steady state their elimination is the linear clearance alone;
# normal, rapid and ultrarapid metabolisers keep 18.2% of it.  The source did
# not test for *17 in adults, so rapid and ultrarapid take the normal
# metaboliser's value.
#
# THE TIME-DEPENDENT VMAX
# =======================
# The engine's michaelisMenten block carries it as vmaxDecline (fraction
# Vmax,inh, t50 T50, onset 1 h; R/advanceMichaelisMenten.R), measured from the
# first dose in the table.  The source estimated Vmax at 1 h "since there were
# no PK samples providing information on Vmax at time zero": before 1 h the
# printed formula would raise Vmax above Vmax,1 (threefold at T = 0), so the
# engine holds it at Vmax,1 for the first hour.  The decline never reverses
# after a pause in dosing, as in the source.
#
# Checked against the label (NDA 021266, 2026, Table 12, geometric means of
# five studies): AUC0-12 13.9 mg.h/L after the 6 mg/kg IV loading dose and
# 33.9 at 4 mg/kg IV q12h.  The label also says "poor metabolizers have, on
# average, 4-fold higher voriconazole exposure" than homozygous extensive
# metabolisers; this model gives about 2 to 2.7-fold.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source's own allometric weight terms, evaluated at the pharmacokinetic
# weight with the fat-free-mass switch on and at total body weight with it
# off, which reproduces the published equations.
#
# NOT MODELLED
# ============
# Sex and age in adults (the label reports higher exposure in women and in
# elderly men), liver disease, and the many interactions (voriconazole is
# cleared by CYP2C19, CYP2C9 and CYP3A4).  The source's adults were healthy.
#
# THE BAND
# ========
# Abdul-Aziz 2020 (Intensive Care Med 46:1127-1153, the ESICM/ESCMID position
# paper, read in full): "A Cmin target between 2 and 6 mg/L is recommended to
# guide voriconazole dosing"; efficacy at Cmin 1-2 mg/L and above, toxicity at
# Cmin 4.5-6 mg/L and above.  A trough range, drawn against the whole curve.
# -----------------------------------------------------------------------------

# Friberg 2012, Table 3
VORICONAZOLE_KM         <- 1.15     # mg/L
VORICONAZOLE_VMAX1      <- 114      # mg/h at 70 kg, at 1 h
VORICONAZOLE_LOGIT_INH  <- 1.50
VORICONAZOLE_LOGIT_INH_CHILD <- -0.390   # added under 12 years
VORICONAZOLE_T50        <- 2.41     # h after the first dose
VORICONAZOLE_CL         <- 6.16     # L/h at 70 kg
VORICONAZOLE_V2         <- 79.0     # L, central, at 70 kg
VORICONAZOLE_V3         <- 103      # L, peripheral, at 70 kg
VORICONAZOLE_Q          <- 15.5     # L/h at 70 kg
VORICONAZOLE_Q_YOUNG    <- 0.637    # Q x (1 + 0.637) under 17 years
VORICONAZOLE_LOGIT_F1   <- 0.585
VORICONAZOLE_KA_ADULT   <- 100      # 1/h, fixed
VORICONAZOLE_KA_CHILD   <- 1.19     # 1/h
VORICONAZOLE_KA_ADOL    <- -0.615   # ka x (1 - 0.615) for 12 to <17
VORICONAZOLE_ALAG       <- 0.949    # h, adults
VORICONAZOLE_ALAG_YOUNG <- -0.874   # Alag x (1 - 0.874) under 17 years

#' Voriconazole (Friberg 2012)
#'
#' Two compartments, linear plus Michaelis-Menten elimination whose Vmax
#' declines over the first days, CYP2C19 in adults; see the header of
#' \code{R/drugs_voriconazole.R}.
#'
#' @inheritParams cefazolin
#' @param cyp2c19 CYP2C19 phenotype, one of \code{CYP2C19_VALUES}
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
voriconazole <- function(weight, height, age, sex, adjustToFFM = TRUE,
                         cyp2c19 = CYP2C19_DEFAULT)
{
  if (!(length(cyp2c19) == 1 && cyp2c19 %in% CYP2C19_VALUES))
    stop("Invalid cyp2c19 for voriconazole: ", paste(cyp2c19, collapse = ", "))
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  w <- pkW / 70
  adult <- age >= 17
  child <- age < 12

  inverseLogit <- function(x) exp(x) / (1 + exp(x))
  inhibition <- if (adult && cyp2c19 %in% c("poor", "intermediate")) 1 else
    inverseLogit(VORICONAZOLE_LOGIT_INH + if (child) VORICONAZOLE_LOGIT_INH_CHILD else 0)

  vmax <- VORICONAZOLE_VMAX1 / 60 * w^0.75                     # mg/min
  km   <- VORICONAZOLE_KM
  clLinear <- VORICONAZOLE_CL / 60 * w^0.75                     # L/min
  q  <- VORICONAZOLE_Q / 60 * w^0.75 * (if (adult) 1 else 1 + VORICONAZOLE_Q_YOUNG)
  ka <- if (adult) VORICONAZOLE_KA_ADULT else
    VORICONAZOLE_KA_CHILD * (if (child) 1 else 1 + VORICONAZOLE_KA_ADOL)
  alag <- VORICONAZOLE_ALAG * (if (adult) 1 else 1 + VORICONAZOLE_ALAG_YOUNG)

  default <- list(
    v1 = VORICONAZOLE_V2 * w,
    v2 = VORICONAZOLE_V3 * w,
    v3 = 1,
    # The low-concentration limit at 1 h, linear plus Vmax / Km, for the help
    # tables only: the simulation uses the michaelisMenten block.
    cl1 = clLinear + vmax / km,
    cl2 = q,
    cl3 = 0,
    ka_PO = ka / 60,
    bioavailability_PO = inverseLogit(VORICONAZOLE_LOGIT_F1),
    tlag_PO = alag * 60
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Friberg LE et al., Antimicrob Agents Chemother 2012;56:3032-3042: two ",
    "compartments, linear CL 6.16 L/h plus Michaelis-Menten (Vmax 114 mg/h at ",
    "1 h, declining by ", round(100 * inhibition, 1), "% with T50 2.41 h; Km ",
    "1.15 mg/L), V 79.0 and 103 L, Q 15.5 L/h at 70 kg; oral F 0.64, lag ",
    round(alag, 3), " h; CYP2C19 ", cyp2c19, ". https://doi.org/10.1128/AAC.05761-11"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 4,
    upperTypical = 6,
    lowerTypical = 2,
    reference = reference,
    michaelisMenten = list(
      vmax = vmax,
      km = km,
      clLinear = clLinear,
      vmaxDecline = list(fraction = inhibition, t50 = VORICONAZOLE_T50 * 60,
                         onset = 60)
    )
  )
}
