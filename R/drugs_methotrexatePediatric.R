# -----------------------------------------------------------------------------
# Methotrexate, high-dose intravenous, in children and young adults
# (Taylor 2023, the MTXPK.org model): plasma methotrexate
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg and concentrations in mcg/mL (= mg/L).  The source works in umol/L:
# 1 umol/L = 0.4544 mg/L (methotrexate 454.44 g/mol).
#
# Drafted by Claude Code, 2026-11-06, at the request of Steven L. Shafer, as a
# first-wave chemotherapy drug (docs/chemotherapy.md,
# inst/extdata/chemotherapyRegistry.csv, source C13).  Named for its
# population, as milrinonePediatric is, so that it cannot be taken for an
# adult model.  Every number below was read from the PDF, which Shafer
# supplied (2026-11-06): Table 1, Table 2, equations 3-7 and the Methods;
# the supplement's Tables S1-S2 were read too.
#
# THE SOURCE
# ==========
# Taylor ZL, Mizuno T, Punt NC, et al.  Clinical covariates that improve the
# description of high dose methotrexate pharmacokinetics in a diverse
# population to inform MTXPK.org.  Clin Transl Sci 2023;16:2130-2143
# (doi 10.1111/cts.13600).  1,668 patients aged 0.1-32.3 years (median
# 10.9) at three US children's hospitals, 2010-2020: 7,506 courses of 0.5-18
# g/m2 for ALL, osteosarcoma and non-Hodgkin lymphoma, 30,250 plasma
# concentrations to 380 h.  BSA 0.24-2.8 m2, baseline creatinine 0.1-1.9
# mg/dL.  NONMEM 7.2, ADVAN11 TRANS4, FOCE-I; it extends the MTXPK.org model
# of Taylor 2020 (772 Nordic children with ALL).
#
# THE MODEL (Table 2, equations 3-7)
# ==================================
# Three compartments, all scaled linearly by BSA (Mosteller) over 1.73 m2:
#
#     CL = 8.24 L/h  x (BSA/1.73) x (SCR/40 uM)^-0.26
#                    x 1.05 for a short infusion (8 h or less)
#                    x 0.84 with Down syndrome
#     V1 = 22.1 L    x (BSA/1.73) x 1.06 with a pleural effusion
#     Q2 = 0.225 L/h x (BSA/1.73) x 1.17 with severe hypoalbuminaemia
#     V2 = 2.19 L    x (BSA/1.73) x 1.43 with a pleural effusion
#     Q3 = 0.0342 L/h x (BSA/1.73) x 1.34 with severe hypoalbuminaemia
#     V3 = 6.07 L    x (BSA/1.73)
#
# THE COMPARTMENTS ARE MAMMILLARY.  Table 2's footnote calls Q3 the
# clearance "between V2 and V3", a chain; the Methods state ADVAN11 TRANS4,
# NONMEM's three-compartment mammillary model (both peripherals on the
# central compartment), and the supplement's abbreviations agree ("between
# V1 and V3"), so the footnote is a misprint.  Equation 3 prints the Down
# syndrome factor 0.83 against Table 2's 0.84; it is not applied (below).
# Treatment site entered the model during selection (Table S1) but is in
# neither equation 3 nor Table 2, so the final model has none.
#
# SEVERE HYPOALBUMINAEMIA
# =======================
# The Patient Profile's Serum albumin field (g/dL; added 2026-11-06 on
# Steven L. Shafer's decision).  The source's covariate is categorical,
# severe hypoalbuminaemia, an albumin of 2.5 g/dL or less, so an entered
# albumin of 2.5 g/dL or less multiplies Q2 by 1.17 and Q3 by 1.34
# (Table 2), and anything above it, or a blank field (NULL), applies
# neither: the source's reference is a normal albumin.  The factors do not
# grade below the threshold; 1.0 and 2.5 g/dL give the same curve.
#
# WHAT IS HELD AT THE REFERENCE
# =============================
# The Patient Profile has no field for Down syndrome, a pleural effusion or
# the infusion's length, so each is held at the source's reference: no Down
# syndrome, no effusion, a long infusion.  For a short infusion (3-4 h, as
# in osteosarcoma) the true clearance is 5% higher than the curve's.  These
# are each a few per cent of courses (Table 1: 0.8% Down syndrome, 0.5%
# effusion; severe hypoalbuminaemia, now read from the field, 2.8%) but
# matter to those patients: the effusion is a third space that releases
# methotrexate slowly, and its V2 is 43% larger.
#
# RENAL FUNCTION
# ==============
# The serum creatinine, as each patient's own value (the source used
# children's measured creatinine, referenced to 40 uM, 0.45 mg/dL), so it is
# patientCreatinine(): the Patient Profile's creatinine, or the normal value
# for the age and sex when the field is blank.  The source's creatinine was
# time-varying; the curve holds the entered value through the course, so it
# cannot show the delayed elimination of a kidney injury that develops
# during the course, which is when methotrexate is most dangerous.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model carries its own size covariate, BSA, evaluated as for cefazolin
# at the pharmacokinetic weight with the switch on and at total body weight
# with it off.
#
# PREGNANCY (R/pregnancy.R)
# =========================
# See the registry row: methotrexate is contraindicated in pregnancy, and no
# change is applied.
#
# NO EFFECT SITE, NO BAND
# =======================
# Plasma only.  The thresholds that matter (glucarpidase and leucovorin
# rescue, discharge at 0.1 umol/L) are management criteria at stated times,
# not a therapeutic range, and are not drawn.
# -----------------------------------------------------------------------------

# Taylor 2023, Table 2: clearances in L/h and volumes in L per 1.73 m2
METHOTREXATE_TAYLOR <- list(
  cl = 8.24, v1 = 22.1, q2 = 0.225, v2 = 2.19, q3 = 0.0342, v3 = 6.07,
  scrExponent = -0.26,
  scrReference = 40,     # uM
  bsaReference = 1.73,   # m2
  # severe hypoalbuminaemia, albumin <= 2.5 g/dL (Table 2)
  albuminSevere = 2.5,   # g/dL
  q2Hypoalbuminaemia = 1.17,
  q3Hypoalbuminaemia = 1.34
)
METHOTREXATE_MW <- 454.44   # g/mol: 1 umol/L = 0.45444 mg/L

#' Methotrexate, high-dose intravenous, in children and young adults
#' (Taylor 2023)
#'
#' Three mammillary compartments scaled by body surface area, with the serum
#' creatinine on clearance and severe hypoalbuminaemia (albumin 2.5 g/dL or
#' less) on the distribution clearances; see the header of
#' \code{R/drugs_methotrexatePediatric.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine, mg/dL, or NULL for the normal value for
#'   the age and sex
#' @param albumin serum albumin, g/dL, or NULL for a normal albumin; 2.5 or
#'   less applies the severe-hypoalbuminaemia factors on Q2 and Q3
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
methotrexatePediatric <- function(weight, height, age, sex, adjustToFFM = TRUE,
                                  creatinine = NULL, albumin = NULL)
{
  p <- METHOTREXATE_TAYLOR
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  bsa <- bsaMosteller(w, height) / p$bsaReference
  scr <- patientCreatinine(creatinine, age, sex) * SCR_UMOL_PER_MG_DL     # uM
  severe <- isTRUE(albumin <= p$albuminSevere)
  fQ2 <- if (severe) p$q2Hypoalbuminaemia else 1
  fQ3 <- if (severe) p$q3Hypoalbuminaemia else 1

  default <- list(
    v1  = p$v1 * bsa,
    v2  = p$v2 * bsa,
    v3  = p$v3 * bsa,
    cl1 = p$cl * bsa * (scr / p$scrReference)^p$scrExponent / 60,
    cl2 = p$q2 * bsa * fQ2 / 60,
    cl3 = p$q3 * bsa * fQ3 / 60
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Taylor ZL, Mizuno T, Punt NC, et al. Clinical covariates that improve the ",
    "description of high dose methotrexate pharmacokinetics in a diverse population ",
    "to inform MTXPK.org. Clin Transl Sci 2023;16:2130-2143. 1,668 patients 0.1-32 ",
    "years: three compartments per 1.73 m2 (Mosteller), CL 8.24 L/h x (creatinine/40 ",
    "uM)^-0.26 from the entered creatinine or the normal value for age, V1 22.1 L, ",
    "Q2 0.225 L/h, V2 2.19 L, Q3 0.0342 L/h, V3 6.07 L; Q2 x1.17 and Q3 x1.34 for an ",
    "entered albumin of 2.5 g/dL or less (blank: normal albumin); long infusion, no ",
    "Down syndrome and no pleural effusion assumed. Plasma methotrexate ",
    "in mg/L (1 umol/L = 0.454 mg/L). https://doi.org/10.1111/cts.13600"
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
