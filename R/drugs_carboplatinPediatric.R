# -----------------------------------------------------------------------------
# Carboplatin, intravenous, in children (Duong 2019): plasma carboplatin
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg and concentrations in mcg/mL (= mg/L).
#
# Drafted by Claude Code, 2026-11-06, at the request of Steven L. Shafer
# (docs/chemotherapy.md, inst/extdata/chemotherapyRegistry.csv, source C04).
# Every number below was read from the PDF, which Shafer supplied
# (2026-11-06): Tables 1-4 on the page images.  The same paper is the
# library's melphalanPediatric and etoposideDuong; duongModel() below is
# shared by the three.
#
# THE SOURCE
# ==========
# Duong JK, Veal GJ, Nath CE, et al.  Population pharmacokinetics of
# carboplatin, etoposide and melphalan in children: a re-evaluation of
# paediatric dosing formulas for carboplatin in patients with normal or mild
# impairment of renal function.  Br J Clin Pharmacol 2019;85:136-146 (doi
# 10.1111/bcp.13774).  51 children with high-risk neuroblastoma (31% girls),
# 1.7-8.3 years, 8.6-31 kg (median 14.5), given high-dose carboplatin,
# etoposide and melphalan before autologous transplant (UK SIOPEN HR-NBL1 and
# Sydney); carboplatin and etoposide as 96-h infusions, melphalan over 15-30
# min daily for 3 days; NONMEM 7.3, FOCE-I.
#
# THE MODEL (Table 3; Eqs. 4-6)
# =============================
# For each drug two compartments, linear, scaled by weight alone, the
# clearances (CL, Q) to the 3/4 power and the volumes in proportion:
#
#                  CL L/h    VC L    Q L/h    VP L
#   carboplatin    2.7       8.3     0.23     0.74
#   etoposide      0.59      2.7     0.07     3.8
#   melphalan      8.9       6.8     4.1      4.6
#
# all at the reference weight WT_STD, which the paper calls the population's
# median weight and never prints.  Table 2's median is 14.5 kg; 15.5 kg is
# used, on Shafer's decision (2026-11-06), because the paper's own Table 4
# carboplatin exposures reproduce with it (within 2%) and fall 2-7% short at
# 14.5.  Measured GFR (51Cr-EDTA, 99mTc-DTPA), serum creatinine, Schwartz
# clearance, age, sex and BSA were all tested and none survived: weight is
# the only covariate.
#
# CARBOPLATIN
# ===========
# The two sites measured different things (ultrafilterable platinum by
# atomic absorption in the UK; carboplatin by HPLC in Sydney, the matrix
# unstated), pooled with separate residual errors; the doses and the AUC
# target are in mg of carboplatin, so the curve is read as unbound
# carboplatin.  Q and VP are essentially unidentified (no sample before 6 h
# into a 96-h infusion; the text gives their RSEs as 143% and 69% where Table
# 3 prints 50% and 31%; VP's bootstrap median 2.8 L against 0.74), so the
# curve after a short infusion is an extrapolation.  At 15.5 kg: CL 45 mL/min,
# half-lives 1.6 and 2.9 h.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model's own weight covariate, at the pharmacokinetic weight with the
# switch on and total weight with it off.  Children 1.7-8.3 years; an adult
# is an extrapolation (use carboplatin, Duffull 1997).
#
# PREGNANCY: not applicable (a children's model).
# NO EFFECT SITE, NO BAND: the AUC target is an exposure.
# -----------------------------------------------------------------------------

# Duong 2019, Table 3, at WT_STD; Shafer chose WT_STD 15.5 kg (2026-11-06)
DUONG_WT_STD <- 15.5
DUONG_TABLE3 <- list(
  carboplatin = c(cl = 2.7, v1 = 8.3, q = 0.23, v2 = 0.74),
  etoposide   = c(cl = 0.59, v1 = 2.7, q = 0.07, v2 = 3.8),
  melphalan   = c(cl = 8.9, v1 = 6.8, q = 4.1, v2 = 4.6)
)

# The PK list shared by carboplatinPediatric, etoposideDuong and
# melphalanPediatric: two compartments, allometric in weight
duongModel <- function(drug, weight, height, age, sex, adjustToFFM, reference)
{
  p <- DUONG_TABLE3[[drug]]
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  fCl <- (w / DUONG_WT_STD)^0.75
  fV <- w / DUONG_WT_STD

  default <- list(
    v1  = p[["v1"]] * fV,
    v2  = p[["v2"]] * fV,
    v3  = 1,                                 # placeholder: two compartments
    cl1 = p[["cl"]] * fCl / 60,
    cl2 = p[["q"]] * fCl / 60,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = paste0(
      "Duong JK, Veal GJ, Nath CE, et al. Population pharmacokinetics of ",
      "carboplatin, etoposide and melphalan in children. Br J Clin Pharmacol ",
      "2019;85:136-146. 51 children with neuroblastoma: ", reference,
      " https://doi.org/10.1111/bcp.13774")
  )
}

#' Carboplatin, intravenous, in children (Duong 2019)
#'
#' Two compartments allometric in weight; see the header of
#' \code{R/drugs_carboplatinPediatric.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
carboplatinPediatric <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  duongModel("carboplatin", weight, height, age, sex, adjustToFFM, paste0(
    "carboplatin in two compartments, CL 2.7 L/h, VC 8.3 L, Q 0.23 L/h, VP 0.74 L ",
    "at 15.5 kg (the reference weight is not printed; Shafer's choice), clearances x ",
    "(WT/15.5)^0.75, volumes x WT/15.5. Unbound carboplatin."))
}
