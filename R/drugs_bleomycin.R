# -----------------------------------------------------------------------------
# Bleomycin, intravenous (Yee 1983): plasma bleomycin by radioimmunoassay,
# two compartments, per square metre of body-surface area
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.  Doses in
# USP units (U), amounts in mU, concentrations in mU/L = mcU/mL, as for
# vasopressin (R/drugs_vasopressin.R): the label's unit "is equal to the
# formerly used milligram activity", so the paper's mg doses are units and
# its ng/mL are mcU/mL.
#
# Drafted by Claude Code, 2026-11-16, at the request of Steven L. Shafer, who
# supplied the PDF.  Every number below was read on the pages of the PDF.
#
# THE SOURCE
# ==========
# Yee GC, Crom WR, Lee FH, Smyth RD, Evans WE.  Bleomycin disposition in
# children with cancer.  Clin Pharmacol Ther 1983;33:668-673 (doi
# 10.1038/clpt.1983.91).  14 children (11 boys; median 14.5 years, 1-17), 14
# intravenous bolus courses (20 or 30 mg/m2) and 7 prolonged infusions;
# plasma at 5, 15 and 30 min and 1-24 h after a bolus; radioimmunoassay
# (limit 8 ng/mL); each course fitted by DRUGMODEL (1/Y weights), Vss and CL
# by model-independent methods.
#
# THE MODEL (Table II, "All patients", the 11 bolus courses with a cumulative
# cisplatin dose below 300 mg/m2)
# ==========================================================================
#     t1/2 alpha 0.3 +/- 0.1 h   t1/2 beta 3.2 +/- 0.7 h   Vc 4.3 +/- 0.5 L/m2
#     Vss 9.9 +/- 1.1 L/m2       CL 51.8 +/- 6.1 mL/min/m2   (mean +/- SEM)
#
# Five numbers for a four-parameter model, means of per-course estimates, so
# they do not close exactly.  [decision] Claude's choice of 2026-11-16, for
# Shafer: CL, Vc, Vss and the terminal half-life close it, and the fast
# half-life, printed to one figure, is the check:
#
#     k10 = CL/Vc = 0.723 /h; Vss/Vc = 1 + k12/k21; alpha beta = k10 k21
#     k12 0.637 /h, k21 0.489 /h, so Q 2.74 L/h/m2, V2 5.6 L/m2
#
# giving a fast half-life of 0.42 h against the printed 0.3 +/- 0.1 (1.2 SEM).
# The alternatives miss by as much or more: CL, Vc and both half-lives give
# Vss 11.2 L/m2 against 9.9 +/- 1.1 (1.2 SEM); CL, Vss and both half-lives
# give Vc 3.3 L/m2 against 4.3 +/- 0.5 (1.9 SEM).
#
# CHECKS (test-drugs-bleomycin.R) against ADULTS, whom the model was not
# fitted to: Alberts 1978 (Cancer Chemother Pharmacol 1:177-181, PDF read; 8
# adults, 13.7-19.9 U/m2 IV bolus) measured CL 50.6 mL/min/m2 and half-lives
# of 24.4 min and 241.9 min; this model gives 51.8, 25 min and 192 min.  The
# authors conclude that disposition in older children "is as in adults".  The
# label (Fresenius Kabi, 2024) quotes the same: 51 mL/min/m2 total clearance.
#
# NOT APPLIED: the higher clearance under 3 years (70.5 mL/min/m2 in three
# courses, P < 0.05; the authors could not place the transition, having one
# child between 3 and 14); and any renal term.  Two thirds of the clearance
# is renal (CLR 33.5 mL/min/m2), but the paper found no relation to creatinine
# clearance in children of normal renal function; with cisplatin above 300
# mg/m2 the clearance fell to 18.0 mL/min/m2, and a child with obstructed
# ureters had 4.8.  The label proposes dose reductions below a creatinine
# clearance of 50 mL/min.  The curve is that of normal renal function.
#
# CAUTION: Kramer 1978 (J Clin Pharmacol 18:346-352, PDF read) found a slow
# phase below 10 mcU/mL after 4-5-day infusions, which a 10-min infusion's
# parameters do not predict (an assay-reactive metabolite or tissue binding,
# the authors suggest); the model does not carry it.
#
# DOSE BASIS: bleomycin sulfate in USP units, as labelled.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Every volume and clearance per m2 of body-surface area (the paper names no
# formula; Mosteller's is used), from the pharmacokinetic weight with the
# switch on and total weight with it off.
#
# PREGNANCY: no human data; see its registry row.  NO EFFECT SITE, NO BAND.
# -----------------------------------------------------------------------------

# Yee 1983, Table II (all patients), and the closure above
BLEOMYCIN_YEE <- list(
  cl = 51.8 * 60 / 1000,        # L/h/m2
  vc = 4.3,                     # L/m2
  vss = 9.9,                    # L/m2
  tBeta = 3.2,                  # h
  tAlpha = 0.3                  # h, the check
)

#' Bleomycin's two-compartment model per m2 (Yee 1983)
#'
#' Closed from the clearance, central volume, steady-state volume and terminal
#' half-life; see the header of \code{R/drugs_bleomycin.R}.
#' @returns a list: v1, v2 (L/m2), cl, q (L/h/m2) and the fast half-life (h)
#' @keywords internal
bleomycinYee <- function()
{
  p <- BLEOMYCIN_YEE
  k10 <- p$cl / p$vc
  b <- log(2) / p$tBeta
  r <- p$vss / p$vc - 1                       # k12 / k21
  # alpha = k10 + k12 + k21 - beta and alpha beta = k10 k21, with k12 = r k21
  k21 <- (b * (k10 - b)) / (k10 - b * (1 + r))
  k12 <- r * k21
  a <- k10 + k12 + k21 - b
  list(v1 = p$vc, v2 = p$vc * r, cl = p$cl, q = p$vc * k12, tAlpha = log(2) / a)
}

#' Bleomycin, intravenous (Yee 1983)
#'
#' Two compartments per square metre of body-surface area, doses in USP
#' units; see the header of \code{R/drugs_bleomycin.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
bleomycin <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  m <- bleomycinYee()
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  bsa <- bsaMosteller(w, height)

  default <- list(
    v1  = m$v1 * bsa,
    v2  = m$v2 * bsa,
    v3  = 1,                                 # placeholder: two compartments
    cl1 = m$cl * bsa / 60,
    cl2 = m$q * bsa / 60,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Yee GC, Crom WR, Lee FH, Smyth RD, Evans WE. Bleomycin disposition in children with ",
    "cancer. Clin Pharmacol Ther 1983;33:668-673. 11 IV bolus courses in children 1-17 ",
    "years: two compartments closed from CL 51.8 mL/min/m2, Vc 4.3 and Vss 9.9 L/m2 and a ",
    "3.2-h terminal half-life (Mosteller BSA); normal renal function. Doses in USP units, ",
    "concentrations in mcU/mL. https://doi.org/10.1038/clpt.1983.91"
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
