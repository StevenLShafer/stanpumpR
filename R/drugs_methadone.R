# -----------------------------------------------------------------------------
# Methadone: Henthorn and Kharasch 2025, both enantiomers, summed to racemate
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.  Doses are
# methadone hydrochloride as labelled; concentrations are racemic methadone
# base, R(-) + S(+), in mcg/mL.
#
# SOURCE
# ======
# Henthorn TK, Kharasch ED.  Population pharmacokinetics of intravenous
# methadone enantiomers in adults: a comprehensive model.  Clin Pharmacol Ther
# 2026;119(3):739-750 (online 2025).  64 healthy adults, 29 +/- 8 years,
# 74 +/- 13 kg, given 6.0 mg methadone hydrochloride (5.4 mg base) as a
# 10-second intravenous bolus and sampled for 96 hours.  R(-) and S(+)
# methadone were each fitted with a three-compartment model; S(+) is R(-)
# times a fitted S/R ratio for the volumes, the intercompartmental clearances,
# renal clearance and metabolic clearance to EDDP.  Table 2:
#
#   R(-)-methadone       Vc 41.0 L, V2 155.3 L, V3 208.1 L x weight term
#                        Q2 13.3 L/min, Q3 0.50 L/min
#                        CL = 0.024 (to EDDP, CYP2B6 *1/*1)
#                           + 0.026 (renal) + 0.053 (other) = 0.103 L/min
#   S(+)-methadone       volumes x 0.60, Q2 and Q3 x 0.77
#                        CL = 0.024 x 1.30 + 0.026 x 0.57 + 0.045 (other)
#                           = 0.09102 L/min
#
# These reproduce the paper's own clearance shares (R: 25% renal, 23% EDDP,
# 51% other; S: 16%, 34%, 50%).  The typical-value terminal half-lives are
# 48 h for R(-) and 33 h for S(+).  The paper quotes 2.5 and 1.7 days; their
# ratio, 0.69, is reproduced exactly, so the quoted absolute values are taken
# to be summaries of individual estimates rather than of the typical values.
#
# COVARIATES
# ==========
# Weight is the only covariate in the final model, on V3 with exponent 1.23
# (Table 2), for both enantiomers.  The paper writes the term as WT^1.23
# without stating the weight it was centred on; 208.1 L is read as the value
# at 74 kg, the study mean.  Sex, race and CYP2C19 had no effect.  Weight was
# tested on every parameter and kept only on V3, and the authors conclude
# that their data "do not support dose adjustments based on weight for
# chronic methadone administration", so the other parameters are NOT size
# scaled with either position of the fat-free-mass switch.  This model
# therefore carries its own size covariate (docs/weight-adjustment.md,
# option 1): V3 is evaluated at the pharmacokinetic weight with the switch on
# and at total body weight with it off, as alprazolam does.  The study's
# weight range was roughly 50 to 100 kg; children are extrapolation.
#
# CYP2B6 genotype changed clearance to EDDP, but the authors recommend the
# wild-type (*1/*1) parameters for practical use, which is what is here; the
# app has no CYP2B6 input.
#
# REDUCTION TO THE LINEAR ENGINE
# ==============================
# The engine runs one three-compartment mammillary model per drug.  Racemic
# methadone is two of them in parallel, each receiving half the dose, so its
# unit disposition function has six exponentials.  It is reduced to three,
# per patient, by methadoneRacemicPK():
#
#   1. Build the R(-) and S(+) unit disposition functions at this patient's V3
#      and average them (each enantiomer gets half of every dose).
#   2. Fit three exponentials to that curve, least squares on log
#      concentration over 0.5 min to 7 days, with the total area under the
#      curve held EXACTLY equal to the six-exponential area.  Holding the area
#      makes racemic clearance, and so every steady-state average
#      concentration, identical to the enantiomer model's.
#   3. Convert the three exponentials to V1, V2, V3, CL1, CL2, CL3.
#
# At 74 kg the reduced model is within 2.5% of the enantiomer sum from 1 min
# to 7 days after a bolus, and within 1% throughout 10 days of 8-hourly
# dosing.  It drifts low after a week (-26% at 14 days after a single dose,
# when the concentration is under 2% of its 1-day value), because three
# exponentials cannot carry the R and S terminal slopes at once.
# Absorption, oral or IV infusion, is linear and enters both enantiomers
# identically, so the same reduction serves every route.
#
# SALT
# ====
# Doses are entered as methadone hydrochloride, as dispensed and as Henthorn
# gave it; concentrations are base.  Volumes and clearances are divided by
# the base fraction 309.45 / 345.91 = 0.8946 so that the plotted
# concentrations are base, as glycopyrrolate does for its salt.
#
# ORAL
# ====
# Henthorn studied intravenous methadone only.  Bioavailability is 0.70, from
# Kharasch 2004, the same laboratory, which measured it directly with
# simultaneous oral deuterium-labelled and intravenous doses in healthy
# volunteers.  Henthorn's introduction quotes "approximately 85%" without
# data of its own; Eap 2002 reviews a mean of about 0.75 with a range of 0.36
# to 1.  The absorption rate constant puts the oral plasma peak
# at 3 hours in the reference man, the middle of the 2.5 to 4 hours Eap 2002
# reports.
#
# EFFECT SITE AND MEAC
# ====================
# Henthorn has no pharmacodynamics.  The 11.3 min time to peak effect and
# the racemic MEAC of 60 ng/mL are kept from the previous (Inturrisi 1987)
# model; ke0 is solved against this disposition.
#
# References
# ----------
# Henthorn TK, Kharasch ED, Clin Pharmacol Ther 2026;119(3):739-750.
#   https://doi.org/10.1002/cpt.70147
# Kharasch ED et al., Clin Pharmacol Ther 2004;76:250-269.
#   https://doi.org/10.1016/j.clpt.2004.05.003
# Eap CB, Buclin T, Baumann P, Clin Pharmacokinet 2002;41:1153-1193.
#   https://doi.org/10.2165/00003088-200241140-00003
# Inturrisi CE et al., Clin Pharmacol Ther 1987;41:392-401.
#   https://pubmed.ncbi.nlm.nih.gov/3829576/
#
# Revised with Claude Code at the request of Steven L. Shafer, 2026-10-10,
# replacing the Inturrisi 1987 disposition.
# -----------------------------------------------------------------------------

# Henthorn 2025, Table 2: R(-)-methadone, and the S(+)/R(-) ratios
METHADONE_R <- list(
  vc = 41.0, v2 = 155.3, v3 = 208.1,       # L; V3 at METHADONE_WT_REF
  q2 = 13.3, q3 = 0.50,                    # L/min
  clEddp = 0.024, clRenal = 0.026, clOther = 0.053   # L/min
)
METHADONE_S_OVER_R <- list(v = 0.60, q = 0.77, clRenal = 0.57, clEddp = 1.30)
METHADONE_S_CL_OTHER <- 0.045              # L/min, Table 2
METHADONE_V3_WT_EXPONENT <- 1.23
METHADONE_WT_REF <- 74                     # kg, the study mean (see header)
METHADONE_BASE_FRACTION <- 309.45 / 345.91 # base / hydrochloride

# Three-compartment unit disposition function (per unit dose, per litre):
# coefficients and exponents.
methadoneUdf <- function(vc, v2, v3, cl, q2, q3)
{
  k10 <- cl / vc; k12 <- q2 / vc; k13 <- q3 / vc
  k21 <- q2 / v2; k31 <- q3 / v3
  lambda <- sort(cube(k10, k12, k13, k21, k31)[1:3], decreasing = TRUE)
  coef <- vapply(1:3, function(i) {
    l <- lambda[i]; o <- lambda[-i]
    (k21 - l) * (k31 - l) / ((o[1] - l) * (o[2] - l)) / vc
  }, numeric(1))
  list(coef = coef, lambda = lambda)
}

# R(-) and S(+) unit disposition functions with V3 at `v3Weight` kg.
methadoneEnantiomers <- function(v3Weight)
{
  r <- METHADONE_R
  s <- METHADONE_S_OVER_R
  v3 <- r$v3 * (v3Weight / METHADONE_WT_REF)^METHADONE_V3_WT_EXPONENT
  list(
    R = methadoneUdf(r$vc, r$v2, v3, r$clEddp + r$clRenal + r$clOther,
                     r$q2, r$q3),
    S = methadoneUdf(r$vc * s$v, r$v2 * s$v, v3 * s$v,
                     r$clEddp * s$clEddp + r$clRenal * s$clRenal +
                       METHADONE_S_CL_OTHER,
                     r$q2 * s$q, r$q3 * s$q)
  )
}

# The fit takes 30 to 150 ms, and the same patient is resolved repeatedly
# (every dose-table edit, the help pages), so results are kept per weight.
methadoneFitCache <- new.env(parent = emptyenv())

# Steps 1 to 3 of the header's reduction: the three-compartment model whose
# bolus response best matches the mean of the R(-) and S(+) responses, with
# equal area.  Returns v1, v2, v3, cl1, cl2, cl3 per unit of base.
methadoneRacemicPK <- function(v3Weight)
{
  key <- format(v3Weight, digits = 15)
  cached <- methadoneFitCache[[key]]
  if (!is.null(cached)) return(cached)

  e <- methadoneEnantiomers(v3Weight)
  udf <- function(coef, lambda, t) colSums(coef * exp(-outer(lambda, t)))
  t <- exp(seq(log(0.5), log(7 * 1440), length.out = 300))
  y <- 0.5 * (udf(e$R$coef, e$R$lambda, t) + udf(e$S$coef, e$S$lambda, t))
  auc <- 0.5 * (sum(e$R$coef / e$R$lambda) + sum(e$S$coef / e$S$lambda))

  # Free parameters: log of the first two coefficients and all three
  # exponents.  The third coefficient is whatever makes the area exact.
  unpack <- function(p)
  {
    p <- exp(p)
    c3 <- p[5] * (auc - p[1] / p[3] - p[2] / p[4])
    list(coef = c(p[1], p[2], c3), lambda = p[3:5])
  }
  objective <- function(p)
  {
    u <- unpack(p)
    if (u$coef[3] <= 0) return(1e10)
    sum((log(udf(u$coef, u$lambda, t)) - log(y))^2)
  }
  start <- log(c(0.5 * (e$R$coef[1:2] + e$S$coef[1:2]),
                 sqrt(e$R$lambda * e$S$lambda)))
  fit <- stats::optim(start, objective, method = "Nelder-Mead",
                      control = list(maxit = 20000, reltol = 1e-14))
  fit <- stats::optim(fit$par, objective, method = "BFGS",
                      control = list(maxit = 5000, reltol = 1e-15))
  u <- unpack(fit$par)
  ord <- order(u$lambda, decreasing = TRUE)
  A <- u$coef[ord]
  l <- u$lambda[ord]

  # Exponentials to mammillary micro constants.  With a_i = A_i V1, the
  # numerator of the Laplace transform gives k21 + k31 and k21 k31; the
  # denominator gives k10 and then k12 and k13.
  v1 <- 1 / sum(A)
  a <- A * v1
  sumK <- sum(a * c(l[2] + l[3], l[1] + l[3], l[1] + l[2]))
  prodK <- sum(a * c(l[2] * l[3], l[1] * l[3], l[1] * l[2]))
  root <- sqrt(sumK^2 - 4 * prodK)
  k21 <- (sumK + root) / 2
  k31 <- (sumK - root) / 2
  k10 <- prod(l) / (k21 * k31)
  kOut <- sum(l) - k10 - k21 - k31            # k12 + k13
  pairs <- l[1] * l[2] + l[1] * l[3] + l[2] * l[3]
  k12 <- ((kOut + k10) * (k21 + k31) + k21 * k31 - pairs - kOut * k31) /
    (k21 - k31)
  k13 <- kOut - k12
  if (!all(is.finite(c(k10, k12, k13, k21, k31))) ||
      any(c(k10, k12, k13, k21, k31) <= 0))
    stop("methadone: racemic reduction did not give a mammillary model")

  pk <- list(
    v1 = v1, v2 = v1 * k12 / k21, v3 = v1 * k13 / k31,
    cl1 = v1 * k10, cl2 = v1 * k12, cl3 = v1 * k13
  )
  assign(key, pk, envir = methadoneFitCache)
  pk
}

#' Methadone pharmacokinetics
#'
#' Henthorn and Kharasch (2025): separate three-compartment models for R(-)
#' and S(+) methadone, summed to racemic methadone and reduced to one
#' three-compartment model for this patient.  Intravenous and oral.  See the
#' file's header.
#'
#' @param weight weight in kg
#' @param height height in cm (used only for the fat-free-mass weight)
#' @param age age in years (used only for the fat-free-mass weight)
#' @param sex sex as a string (used only for the fat-free-mass weight)
#' @param adjustToFFM \code{TRUE} (the default) evaluates Henthorn's weight
#'   term on V3 at the pharmacokinetic weight; \code{FALSE} at total body
#'   weight.  No other parameter is size scaled.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
methadone <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Size scaling (see the header): Henthorn's own weight term on V3 only, at
  # the pharmacokinetic weight with the switch on, total weight with it off.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  base <- methadoneRacemicPK(pkW)
  # Hydrochloride doses, base concentrations (see the header).
  f <- METHADONE_BASE_FRACTION

  default <- list(
    v1 = base$v1 / f,
    v2 = base$v2 / f,
    v3 = base$v3 / f,
    cl1 = base$cl1 / f,
    cl2 = base$cl2 / f,
    cl3 = base$cl3 / f,
    ka_PO = 0.0100236846,        # 1/min; plasma peak at 3 h, reference man
    bioavailability_PO = 0.70,     # Kharasch 2004
    tlag_PO = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 11.3
  MEAC <- 60/1000
  typical <- MEAC * 1.2
  upperTypical <- MEAC * 0.8
  lowerTypical <- MEAC * 2.0
  reference <- paste0(
    "Henthorn TK, Kharasch ED, Clin Pharmacol Ther 2026;119(3):739-750. ",
    "R(-) and S(+) methadone models summed to racemate; ",
    "oral bioavailability 0.70 from Kharasch ED et al., ",
    "Clin Pharmacol Ther 2004;76:250-269; absorption from ",
    "Eap CB et al., Clin Pharmacokinet 2002;41:1153-1193. ",
    "https://doi.org/10.1002/cpt.70147"
  )

  return(
    list(
      PK = PK,
      tPeak = tPeak,
      MEAC = MEAC,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference
    )
  )
}
