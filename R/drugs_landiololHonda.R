# -----------------------------------------------------------------------------
# Landiolol (Honda 2008): whole-blood landiolol in healthy young men
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-11-25, at the request of Steven L. Shafer
# ("Hold on Landiolol for Honda 2008").  Every number below was read on the
# PDF of the source (stanpumpRPapers PMID_19122339.pdf; card
# papers/digests/PMID_19122339.md).
#
# THE SOURCE
# ==========
# Honda N, Nakade S, Kasai H, Hashimoto Y, Ohno T, Kitagawa J, Yamauchi A,
# Hasegawa C, Kikawa S, Kunisawa T, Tanigawara Y, Miyata Y.  Population
# pharmacokinetics of landiolol hydrochloride in healthy subjects.  Drug
# Metab Pharmacokinet 2008;23(6):447-455 (doi 10.2133/dmpk.23.447).  The
# pooled phase 1 studies of Ono Pharmaceutical: 47 healthy Japanese men aged
# 20-33 years, 50.7-77.2 kg (Methods), given landiolol hydrochloride as
# 60-minute infusions of 5-80 mcg/kg/min, a 1-minute load of 250 or 500 then
# 40 or 80 mcg/kg/min for 60 minutes, or escalating 1-minute loads and
# 10-minute infusions (Table 1); 420 VENOUS WHOLE-BLOOD concentrations (the
# blood extracted into ethanol, HPLC with UV, quantified from 50 ng/mL;
# the upper limit's exponent is lost in the PDF's text).  NONMEM FOCE-I;
# one, two and three compartments, two with a lag chosen by AIC.  Covariates were not explored: every parameter was
# estimated per kilogram and every laboratory value was normal.  Four
# outlying points (subject 10 at 30 and 60 min after the end of an infusion,
# subjects 25 and 35 at 30 min) were removed before the final model.
# Table 3 (final model, mean +/- SE):
#
#     CL    36.6 +/- 1.23 mL/min/kg      Q    16.1 +/- 3.70 mL/min/kg
#     V1     101 +/- 8.83 mL/kg          V2   55.6 +/- 6.05 mL/kg
#     ALAG  0.820 min                    IIV CL 21.8%, V1 46.3%
#
# Kunisawa 2015's two papers print the same values beside their own.
#
# WHOLE BLOOD AGAINST PLASMA.  Honda measured whole blood from a vein; both
# Kunisawa studies measured arterial plasma with the esterases stopped at the
# bedside.  The clearances agree within 20% (36.6, 34.0, 30.7); the central
# volumes do not (101, 74.9, 65.0), which venous sampling in volunteers
# explains in the direction the library's rule on comparing sources expects
# (AGENTS.md, Comparing sources).
#
# What the three landiolol models share (the lag, the dose basis, body size,
# the 2-minute tPeak, pregnancy) is in R/landiolol.R.  The group `landiolol`
# chooses this one for the young adults it studied, and, as the nearest
# source, for patients under 20 and up to 35 years (R/modelSelection.R).
# -----------------------------------------------------------------------------

#' Landiolol (Honda 2008, healthy men)
#'
#' Two compartments per kilogram from 47 healthy young Japanese men; a
#' candidate of the group \code{landiolol}.  See the header of
#' \code{R/drugs_landiololHonda.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
landiololHonda <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  landiololPerKg(
    LANDIOLOL_SOURCES$honda, weight, height, age, sex, adjustToFFM,
    paste0(
      "Honda N et al., Drug Metab Pharmacokinet 2008;23(6):447-455. ",
      "Two compartments in 47 healthy men aged 20-33: CL 36.6 mL/min/kg, ",
      "V1 101 mL/kg, Q 16.1 mL/min/kg, V2 55.6 mL/kg; venous whole blood. ",
      "https://doi.org/10.2133/dmpk.23.447."))
}
