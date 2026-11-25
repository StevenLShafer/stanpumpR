# -----------------------------------------------------------------------------
# Efmoroctocog alfa (ELOCTATE, recombinant factor VIII Fc fusion protein):
# two compartments, intravenous
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, amounts in
# IU, activity in IU/dL (= % of normal).  Plotted as the patient's own factor
# VIII activity plus the infused activity (R/factorBaselines.R).  Drafted by
# Claude Code, 2026-11-25; a drug of its own on Steven L. Shafer's decision of
# that day (a clinician knows which product is given; card PMID_32326156).
#
# SOURCE: Allard 2020, the model of R/drugs_factorVIII.R (whose header gives
# the study, the parameters, the age hold, the size scaling and its anchor),
# with EHL = 1: Elocta (Eloctate in the US), 136 of the 258 patients, clears
# exp(-0.394) = 0.674 times as fast as the seven standard-half-life products
# (Table 2).  Read on the PDF.  Switching from a standard product to Elocta
# raised the individual half-lives 1.48-fold (Results 3.6).
#
# CHECKS at 64 kg and 30 years: CL 2.15 mL/kg/h, half-lives 1.5 h and 15.2 h.
# ELOCTATE's label (12.3, Table 5, adults, 50 IU/kg): CL 2.06 mL/h/kg, Vss
# 49.5 mL/kg, terminal half-life 19.7 h, recovery 2.26 IU/dL per IU/kg; the
# label's half-life is 1.5 times ADVATE's.  This model's recovery is 2.42
# IU/dL per IU/kg and its Vss 46.5 mL/kg.  Clearance agrees; the half-life is
# shorter, as factor VIII's is against ADVATE's label (the source sampled
# sparsely, mostly in the first two days).  Children 1 to 5 years: the label's
# clearance per kg is 75% above adults'.
#
# BASELINE: the Patient Profile's Factor VIII baseline, as for factorVIII;
# blank, severe haemophilia A, 0.5 IU/dL.
#
# BAND: ELOCTATE's perioperative target for major surgery, 80 to 120 IU/dL
# before and after the operation (label Table 2); minor surgery 50 to 80.
#
# PREGNANCY: no pharmacokinetic study in pregnancy (ELOCTATE label 8.1;
# inst/extdata/pregnancyRegistry.csv).
#
# References
# ----------
# Allard Q et al., Pharmaceutics 2020;12:380.
#   https://doi.org/10.3390/pharmaceutics12040380
# ELOCTATE prescribing information, BLA 125487, revised 5/2023.
# -----------------------------------------------------------------------------

#' Efmoroctocog alfa, ELOCTATE (Allard 2020)
#'
#' Allard 2020's factor VIII model for its extended-half-life product, plotted
#' as the patient's own factor VIII activity plus the infused activity, in
#' IU/dL; see the header of \code{R/drugs_efmoroctocogAlfa.R}.
#'
#' @inheritParams factorVIII
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
efmoroctocogAlfa <- function(weight, height, age, sex, adjustToFFM = TRUE,
                             factorVIIIBaseline = NULL)
{
  allardFactorVIII(weight, height, age, sex, adjustToFFM, factorVIIIBaseline,
                   ehl = TRUE)
}
