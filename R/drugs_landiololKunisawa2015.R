# -----------------------------------------------------------------------------
# Landiolol (Kunisawa 2015): plasma landiolol in anaesthetised elderly patients
# with peripheral arterial disease
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-11-25, at the request of Steven L. Shafer
# (docs/library-expansion/plan.csv, Anesthesia).  Every number below was read
# on the PDF of the source (stanpumpRPapers PMID_25653534.pdf; card
# papers/digests/PMID_25653534.md).
#
# THE SOURCE
# ==========
# Kunisawa T, Yamagishi A, Suno M, Nakade S, Honda N, Kurosawa A, Sugawara
# A, Tasaki Y, Iwasaki H.  Target-controlled infusion and population
# pharmacokinetics of landiolol hydrochloride in patients with peripheral
# arterial disease.  Ther Clin Risk Manag 2015;11:107-114 (doi
# 10.2147/TCRM.S74867).  Eight patients
# (6 men, 2 women; Table 1: 73 +/- 7 years, 64-84; 58.9 +/- 10.9 kg; 159 +/-
# 7 cm) anaesthetised with propofol and remifentanil by TCI, on dopamine 3
# mcg/kg/min, given landiolol hydrochloride by STANPUMP TCI to plasma targets
# of 500 then 1,000 ng/mL for 30 minutes each; 112 radial arterial samples
# (into ethanol and neostigmine, so the plasma esterases were stopped), HPLC
# with fluorescence.  NONMEM, two compartments with a lag time; one and three
# compartments did not converge; weight, lean body mass and age were tested
# and none retained.  Table 2:
#
#     CL    30.7 mL/min/kg      Q    48.3 mL/min/kg
#     V1    65.0 mL/kg          V2   54.4 mL/kg
#     ALAG  0.633 min           IIV on CL only, 13.5%
#
# What all three landiolol models share (the lag, the dose basis, body size,
# the 2-minute tPeak, pregnancy) is in R/landiolol.R.  The group `landiolol`
# chooses this one from 72 years, which only it studied, and from 64 to 71
# years below 47.1 kg, which Kunisawa 2014 did not study (at a weight both
# studied, Kunisawa 2014's nine patients win over these eight;
# R/modelSelection.R).  Until 2026-11-25 it was the library's only
# landiolol model, used for every patient.
# -----------------------------------------------------------------------------

#' Landiolol (Kunisawa 2015, peripheral arterial disease)
#'
#' Two compartments per kilogram from eight anaesthetised elderly patients
#' with peripheral arterial disease; a candidate of the group
#' \code{landiolol}.  See the header of \code{R/drugs_landiololKunisawa2015.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
landiololKunisawa2015 <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  landiololPerKg(
    LANDIOLOL_SOURCES$kunisawa2015, weight, height, age, sex, adjustToFFM,
    paste0(
      "Kunisawa T et al., Ther Clin Risk Manag 2015;11:107-114. ",
      "Two compartments in 8 anaesthetised patients with peripheral arterial ",
      "disease aged 64-84: CL 30.7 mL/min/kg, V1 65.0 mL/kg, Q 48.3 ",
      "mL/min/kg, V2 54.4 mL/kg; arterial plasma. ",
      "https://doi.org/10.2147/TCRM.S74867."))
}
