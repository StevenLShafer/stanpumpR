# -----------------------------------------------------------------------------
# Landiolol (Kunisawa, gynaecologic patients): plasma landiolol in
# anaesthetised women
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-11-25, at the request of Steven L. Shafer.
# Every number below was read on the PDF of the source (stanpumpRPapers
# PMID_25186494.pdf; card papers/digests/PMID_25186494.md).
#
# THE SOURCE
# ==========
# Kunisawa T, Yamagishi A, Suno M, Nakade S, Higashi R, Kurosawa A, Sugawara
# A, Matsubara K, Iwasaki H.  Target-controlled infusion and population
# pharmacokinetics of landiolol hydrochloride in gynecologic patients.  J
# Anesth 2015;29:198-205 (doi 10.1007/s00540-014-1908-5; received and
# published online 2014, so named "2014" here to tell it from the same
# group's peripheral arterial disease paper of 2015).  Nine women (Table 1:
# median 55 years, 37-71; 62.8 kg, 47.1-73.0; ASA 1-2) having gynaecologic
# surgery under propofol and fentanyl TCI with an epidural catheter, given
# landiolol hydrochloride by STANPUMP TCI with Honda 2008's parameters to
# plasma targets of 500 then 1,000 ng/mL for 30 minutes each; 126 radial
# arterial samples (into chilled ethanol and neostigmine, so the plasma
# esterases were stopped), HPLC with fluorescence.  NONMEM, two compartments
# with a lag; three compartments did not converge; weight, lean body mass
# and age were tested and none retained.  Table 2 (mean +/- SE):
#
#     CL    34.0 +/- 1.96 mL/min/kg      Q    70.9 +/- 68.9 mL/min/kg
#     V1    74.9 +/- 13.2 mL/kg          V2   38.9 +/- 9.03 mL/kg
#     ALAG  0.634 min                    IIV CL 6.3%, V1 6.6%; residual 38.1%
#
# Q is barely identified (its standard error equals it): sampled at 1, 2, 5
# and 25 min after each change and for 20 min after the stop, nine patients
# cannot separate a fast distribution from the central volume.  The
# clearance, the parameter that matters most for a 30-minute infusion, is
# well estimated and lies between Honda's and Kunisawa 2015's.
#
# What the three landiolol models share (the lag, the dose basis, body size,
# the 2-minute tPeak, pregnancy) is in R/landiolol.R.  The group `landiolol`
# chooses this one for the 37-71 years it studied (nine patients, against
# Kunisawa 2015's eight, where the two overlap, except below 47.1 kg, its
# lightest), and as the nearest source from 36 years and for a 35-year-old
# below Honda's 50.7 kg (R/modelSelection.R).  It studied women only; the group's
# rule chooses by age and weight, not sex.
# -----------------------------------------------------------------------------

#' Landiolol (Kunisawa, gynaecologic patients)
#'
#' Two compartments per kilogram from nine anaesthetised women having
#' gynaecologic surgery; a candidate of the group \code{landiolol}.  See the
#' header of \code{R/drugs_landiololKunisawa2014.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
landiololKunisawa2014 <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  landiololPerKg(
    LANDIOLOL_SOURCES$kunisawa2014, weight, height, age, sex, adjustToFFM,
    paste0(
      "Kunisawa T et al., J Anesth 2015;29:198-205. ",
      "Two compartments in 9 anaesthetised women having gynaecologic surgery, ",
      "aged 37-71: CL 34.0 mL/min/kg, V1 74.9 mL/kg, Q 70.9 mL/min/kg, ",
      "V2 38.9 mL/kg; arterial plasma. ",
      "https://doi.org/10.1007/s00540-014-1908-5."))
}
