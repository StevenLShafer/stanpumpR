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
# 51% other; S: 16%, 34%, 50%).  At 70 kg the typical-value terminal
# half-lives are 48 h for R(-) and 33 h for S(+).  The paper quotes 2.5 and
# 1.7 days; their ratio, 0.69, is reproduced, so the quoted absolute values
# are taken to be summaries of individual estimates rather than of the
# typical values.
#
# COVARIATES
# ==========
# Weight is the only covariate in the final model, on V3 with exponent 1.23
# (Table 2), for both enantiomers.  The paper writes the term as WT^1.23
# without stating the weight it was centred on; 208.1 L is taken as the value
# at 70 kg, the library's reference weight (the study mean was 74 kg), at
# Steven L. Shafer's instruction.  Sex, race and CYP2C19 had no effect.  Weight was
# tested on every parameter and kept only on V3, and the authors conclude
# that their data "do not support dose adjustments based on weight for
# chronic methadone administration", so the other parameters are NOT size
# scaled with either position of the fat-free-mass switch.  This model
# therefore carries its own size covariate (docs/weight-adjustment.md,
# option 1): V3 is evaluated at the pharmacokinetic weight with the switch on
# and at total body weight with it off, as alprazolam does.  The study's
# weight range was roughly 50 to 100 kg; children are extrapolation.
#
# CYP2B6
# ======
# Methadone is cleared principally by hepatic CYP2B6-catalysed N-demethylation
# to EDDP, and the CYP2B6 gene is highly polymorphic.  Henthorn's final model
# keeps CYP2B6 genotype as a covariate but recommends the normal-metaboliser
# (*1/*1) values for practical use, which is what its typical values above
# are.  The genotype effect here is taken from the study that measured it
# directly in these same 64 subjects:
#
#   Kharasch ED, Regina KJ, Blood J, Friedel C.  Methadone pharmacogenetics:
#   CYP2B6 polymorphisms determine plasma concentrations, clearance, and
#   metabolism.  Anesthesiology 2015;123:1142-1153 (PMID 26389554), read in
#   full on 2026-10-13.  The hepatic clearances below are quoted from p. 1146,
#   where the sentence names the genotypes in the order it gives the values.
#
# Cohorts of CYP2B6 *1/*1 (n = 21), *1/*6 (n = 20) and *6/*6 (n = 17) healthy
# volunteers received simultaneous intravenous and oral methadone.  HEPATIC
# clearance (mL/kg/min, intravenous drug, Results) was:
#
#                      *1/*1      *6/*6     ratio
#     S(+)-methadone     1.3        0.8      0.615
#     R(-)-methadone     1.3        1.0      0.769
#
# Renal clearance was NOT affected by genotype ("Intravenous methadone renal
# clearance was not affected by genotype"), so the multiplier is applied to
# each enantiomer's NON-RENAL clearance -- Henthorn's clearance to EDDP plus
# his "other" clearance -- and his renal clearance is left alone.  It is not
# put on the EDDP branch alone, which is 23% (R) and 34% (S) of clearance:
# for S(+) that branch is too small to carry the measured 36% fall in
# systemic clearance at all, and Henthorn's "other" clearance is non-renal,
# so on the evidence available the hepatic share is where a CYP2B6 effect
# belongs.
#
# Kharasch reported no hepatic clearance for the *1/*6 heterozygotes.  Their
# effect is set to the fraction of the homozygote effect that his abstract's
# apparent ORAL clearances show (S 35% against 45% lower, R 25% against 35%
# lower), so
#
#     S: 1 - (35/45) x (1 - 0.615) = 0.701      R: 1 - (25/35) x (1 - 0.769) = 0.835
#
# That interpolation is an INFERENCE, not an estimate, and it is recorded as
# one here and on the drug's help page.  THE FRACTION DEPENDS ON WHICH MEASURE
# IT IS TAKEN FROM, which matters because it is applied to a systemic
# clearance (the paper was read in full on 2026-10-13):
#
#     abstract, apparent oral, % lower     0.78     the values above
#     Results, apparent oral clearances    0.64     S 2.3, 1.6, 1.2 mL/kg/min
#     Results, systemic clearances         0.56     S 1.5, 1.2, 0.96 mL/kg/min
#
# Apparent oral clearance amplifies the genotype effect, because CL/F falls
# both from the lower clearance and from the higher bioavailability that less
# first-pass metabolism gives; the systemic figure, 0.56, is almost exactly
# linear in allele count.  The oral-derived values above are kept: they are
# the most conservative of the three, and a single-dose study understates the
# steady state, because CYP2B6 is less inducible in *6 carriers and methadone
# autoinduces.  All three agree qualitatively, and the mechanism is the
# extraction ratio: Kharasch measured 0.08 in *1/*1 and 0.05 in *6/*6
# (Results), so methadone is restrictively cleared, its hepatic clearance
# tracks intrinsic clearance almost one for one, and there is no flow-limited
# reserve to absorb the loss of one allele.
#
# CROSS-CHECK.  Through Henthorn's clearance split these multipliers give
# total systemic clearances, relative to the normal metaboliser, of 0.75
# (*1/*6) and 0.68 (*6/*6) for S(+) and 0.88 and 0.83 for R(-).  Kharasch
# measured S(+) systemic clearances of 1.5, 1.2 and 0.96 mL/kg/min in the
# three cohorts, that is 0.80 and 0.64 of the normal metaboliser, and found
# R(-) systemic clearance not significantly changed.  The model is within
# 0.05 of the measured S(+) ratios and moves R(-) less than S(+), as every
# source does.
#
# PHENOTYPE, NOT DIPLOTYPE.  The app's field carries the five CPIC categories
# (Robinson et al., Clin Pharmacol Ther 2024;116:932-938), which is how
# genotyping laboratories report CYP2B6: *1/*1 is a normal metaboliser, *1/*6
# an intermediate, *6/*6 a poor one.  CPIC's rapid and ultrarapid categories
# (an increased-function *4 allele) are given the NORMAL metaboliser's
# clearance.  Kharasch does report figures for his *4 carriers -- systemic
# clearances of 2.4 (R) and 2.7 (S) mL/kg/min against 1.5 for S in *1/*1, and
# apparent oral clearances of 7.4 and 8.6 against 2.4 and 2.3 -- but they
# cannot set a multiplier for a CPIC phenotype.  The group is FOUR subjects,
# pooling one *1/*4 with three *4/*6, which CPIC classes as two different
# phenotypes: rapid, and intermediate for one increased-function allele with
# one decreased-function allele.  The paper calls these differences
# "numerically greater" and runs no significance test on them.  CPIC's own
# recommendation for rapid metabolisers is standard dosing on limited
# evidence.  (Before the paper was read in full, this file and its help pages
# said no systemic estimate was retrievable.  That was wrong, and was
# corrected on 2026-10-13: the conclusion is unchanged, the reason is not.)
#
# CPIC does not advise changing methadone dosing on CYP2B6 genotype, because
# the exposure difference has not been shown to change efficacy or QTc
# prolongation.  The covariate is here to show the pharmacokinetic effect,
# which is real and measured, not to prescribe with.
#
# Genotype is not the whole of CYP2B6 activity: the enzyme is induced by
# rifampin, phenobarbital, carbamazepine, efavirenz and by chronic methadone
# itself, and inhibited by others.  None of that is modelled.
#
# WITH PREGNANCY.  The two covariates multiply: pregnancy scales all three
# clearance components by 24/14 and the phenotype then scales the non-renal
# part.  Treat a pregnant reduced-function metaboliser as the softest corner
# of this model.  Ansari attributes pregnancy's higher methadone clearance
# partly to induction of CYP2B6 itself, and CYP2B6 activity is LESS
# inducible in *6 carriers (Kharasch's discussion), so the product of the
# two factors probably overstates her clearance.  No source quantifies the
# interaction, so none is applied.
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
# At 70 kg the reduced model is within 2.5% of the enantiomer sum from 1 min
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
# reports.  Oral solution uses the same absorption: Blume 2015 found
# levomethadone tablet and solution bioequivalent (Cmax 104%, AUC 99-101%).
#
# PREGNANCY
# =========
# Ansari, Carvalho, Shafer and Flood (Anesth Analg 2016;122:786-804, Table 3):
# "Increased clearance, largely due to induction of CYP3A4 and CYP2B6 ...
# Reduced elimination half-life of 8-20 h compared with the 24-h half-life in
# a nonpregnant patient", with a higher dose and a shorter interval by the
# third trimester.  The review's figure is that pair of half-lives, so with
# the Patient Profile's Pregnant field on every clearance component of both
# enantiomers (to EDDP, renal and other) is multiplied by
# METHADONE_PREGNANCY_CL_FACTOR = 24 / 14 = 1.71, the nonpregnant half-life
# over the midpoint of 8 to 20 h, before the racemic reduction, which is
# fitted again at the new clearances.  The volumes are unchanged, so the
# reduced model's half-lives fall by the same ratio.  The review attributes
# the change to hepatic induction, but assigns no share to the renal
# component, so all three are scaled alike.  See R/pregnancy.R.
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
# Kharasch ED, Regina KJ, Blood J, Friedel C, Anesthesiology 2015;123:1142-1153.
#   https://doi.org/10.1097/ALN.0000000000000867
# Robinson KM et al. (CPIC), Clin Pharmacol Ther 2024;116:932-938.
#   https://doi.org/10.1002/cpt.3338
# Eap CB, Buclin T, Baumann P, Clin Pharmacokinet 2002;41:1153-1193.
#   https://doi.org/10.2165/00003088-200241140-00003
# Inturrisi CE et al., Clin Pharmacol Ther 1987;41:392-401.
#   https://pubmed.ncbi.nlm.nih.gov/3829576/
# Ansari J, Carvalho B, Shafer SL, Flood P.  Anesth Analg 2016;122:786-804.
#   https://doi.org/10.1213/ANE.0000000000001143 (pregnancy)
#
# Revised with Claude Code at the request of Steven L. Shafer, 2026-10-10,
# replacing the Inturrisi 1987 disposition; pregnancy added 2026-10-13.
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

# Non-renal (hepatic) clearance multiplier by CYP2B6 phenotype, per enantiomer.
# Kharasch 2015 measured hepatic clearances for the poor metaboliser (*6/*6);
# the intermediate (*1/*6) is interpolated from his apparent oral clearances,
# and the rapid and ultrarapid categories take the normal metaboliser's
# values.  See the CYP2B6 section of the header.
METHADONE_CYP2B6_POOR <- c(R = 1.0 / 1.3, S = 0.8 / 1.3)
METHADONE_CYP2B6_HEPATIC <- list(
  poor         = METHADONE_CYP2B6_POOR,
  intermediate = c(R = 1 - (25 / 35) * (1 - METHADONE_CYP2B6_POOR[["R"]]),
                   S = 1 - (35 / 45) * (1 - METHADONE_CYP2B6_POOR[["S"]])),
  normal       = c(R = 1, S = 1),
  rapid        = c(R = 1, S = 1),
  ultrarapid   = c(R = 1, S = 1)
)
METHADONE_WT_REF <- 70                     # kg, see the header
METHADONE_BASE_FRACTION <- 309.45 / 345.91 # base / hydrochloride
# Clearance of both enantiomers in pregnancy relative to Henthorn's values:
# the nonpregnant 24-h half-life over the midpoint of the 8-20 h of Ansari
# 2016 (see the header).
METHADONE_PREGNANCY_CL_FACTOR <- 24 / 14

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

# R(-) and S(+) unit disposition functions with V3 at `v3Weight` kg, at the
# CYP2B6 phenotype's hepatic clearance (the header's CYP2B6 section: that
# multiplier scales the non-renal clearance only), with every elimination
# clearance then multiplied by `clFactor` (pregnancy, which the review
# assigns to no one component; see the PREGNANCY section).
methadoneEnantiomers <- function(v3Weight, clFactor = 1,
                                 cyp2b6 = CYP2B6_DEFAULT)
{
  r <- METHADONE_R
  s <- METHADONE_S_OVER_R
  g <- METHADONE_CYP2B6_HEPATIC[[cyp2b6]]
  if (is.null(g)) stop("Invalid cyp2b6 for methadone: ", paste(cyp2b6, collapse = ", "))
  v3 <- r$v3 * (v3Weight / METHADONE_WT_REF)^METHADONE_V3_WT_EXPONENT
  clR <- g[["R"]] * (r$clEddp + r$clOther) + r$clRenal
  clS <- g[["S"]] * (r$clEddp * s$clEddp + METHADONE_S_CL_OTHER) +
    r$clRenal * s$clRenal
  list(
    R = methadoneUdf(r$vc, r$v2, v3, clR * clFactor, r$q2, r$q3),
    S = methadoneUdf(r$vc * s$v, r$v2 * s$v, v3 * s$v, clS * clFactor,
                     r$q2 * s$q, r$q3 * s$q)
  )
}

# The fit takes 30 to 150 ms, and the same patient is resolved repeatedly
# (every dose-table edit, the help pages), so results are kept per weight
# and clearance factor.
methadoneFitCache <- new.env(parent = emptyenv())

# Steps 1 to 3 of the header's reduction: the three-compartment model whose
# bolus response best matches the mean of the R(-) and S(+) responses, with
# equal area.  Returns v1, v2, v3, cl1, cl2, cl3 per unit of base.
methadoneRacemicPK <- function(v3Weight, clFactor = 1,
                               cyp2b6 = CYP2B6_DEFAULT)
{
  key <- paste(format(v3Weight, digits = 15), format(clFactor, digits = 15),
               cyp2b6)
  cached <- methadoneFitCache[[key]]
  if (!is.null(cached)) return(cached)

  e <- methadoneEnantiomers(v3Weight, clFactor, cyp2b6)
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
#' @param pregnant is the patient pregnant?  \code{TRUE} multiplies the
#'   clearance of both enantiomers by 24/14 before the racemic reduction
#'   (Ansari 2016; see the file's header).
#' @param cyp2b6 CYP2B6 metaboliser phenotype, one of \code{CYP2B6_VALUES}.
#'   It scales the non-renal clearance of each enantiomer, from Kharasch et
#'   al. (2015); the rapid and ultrarapid categories take the normal
#'   metaboliser's values.  See the file's header.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
methadone <- function(weight, height, age, sex, adjustToFFM = TRUE,
                      pregnant = FALSE, cyp2b6 = CYP2B6_DEFAULT)
{
  if (length(cyp2b6) != 1 || !isTRUE(cyp2b6 %in% CYP2B6_VALUES))
    stop("Invalid cyp2b6: ", paste(cyp2b6, collapse = ", "),
         ". Must be one of: ", paste(CYP2B6_VALUES, collapse = ", "))
  # Size scaling (see the header): Henthorn's own weight term on V3 only, at
  # the pharmacokinetic weight with the switch on, total weight with it off.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  # Pregnancy and CYP2B6 (see the header): both act on the enantiomers'
  # clearances, then the racemic model is fitted at the result.
  clPregnancy <- if (isTRUE(pregnant)) METHADONE_PREGNANCY_CL_FACTOR else 1
  base <- methadoneRacemicPK(pkW, clPregnancy, cyp2b6)
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
    # Oral solution ("mg PO liquid") is not declared in oralFormulations: it
    # takes the tablet's absorption.  Blume 2015 (Int J Clin Pharmacol Ther
    # 53:335-44) found levomethadone tablet and oral solution bioequivalent
    # (Cmax ratio 104%, AUC 99-101%); the hydrochloride is freely soluble.
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 11.3
  MEAC <- 60/1000
  typical <- MEAC * 1.2
  lowerTypical <- MEAC * 0.8
  upperTypical <- MEAC * 2.0
  reference <- paste0(
    "Henthorn TK, Kharasch ED, Clin Pharmacol Ther 2026;119(3):739-750. ",
    "R(-) and S(+) methadone models summed to racemate; ",
    "oral bioavailability 0.70 from Kharasch ED et al., ",
    "Clin Pharmacol Ther 2004;76:250-269; absorption from ",
    "Eap CB et al., Clin Pharmacokinet 2002;41:1153-1193; ",
    "CYP2B6 phenotype on non-renal clearance from Kharasch ED et al., ",
    "Anesthesiology 2015;123:1142-1153 (normal metaboliser unless given). ",
    "https://doi.org/10.1002/cpt.70147"
  )
  if (isTRUE(pregnant)) {
    reference <- paste0(reference, "; ", pregnancyReference(
      "clearance x 24/14 (half-life 8-20 h against 24 h)"))
  }

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
