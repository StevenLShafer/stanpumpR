# -----------------------------------------------------------------------------
# Daunorubicin, intravenous (Bogason 2011): plasma daunorubicin
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg and concentrations in ng/mL (doses entered in mg).
#
# Drafted by Claude Code, 2026-11-06, at the request of Steven L. Shafer
# (docs/chemotherapy.md, inst/extdata/chemotherapyRegistry.csv).  Every
# number below was read from the PDF, which Shafer supplied (2026-11-06):
# Tables 1 and 2 on the page images.
#
# THE SOURCE
# ==========
# Bogason A, Quartino AL, Lafolie P, Masquelier M, Karlsson MO, Paul C,
# Gruber A, Vitols S.  Inverse relationship between leukaemic cell burden and
# plasma concentrations of daunorubicin in patients with acute myeloid
# leukaemia.  Br J Clin Pharmacol 2011;71:514-521 (doi
# 10.1111/j.1365-2125.2010.03894.x).  40 adults with AML, 33-83 years;
# daunorubicin 60 mg/m2 over 1 h on days 1-3 after cytarabine, the first dose
# sampled at 1, 5 and 24 h; venous plasma, HPLC with fluorescence; NONMEM.
#
# THE MODEL (Table 2, the basic model)
# ====================================
# Two compartments, linear (reduced from Callies 2004's three for the sparse
# data):
#
#     CL 115 L/h    Vc 373 L    Q 252 L/h    Vp 1120 L
#
# Derived here: half-lives 0.60 and 11.5 h, Vss 1493 L.  For 108 mg over 1 h
# the model gives 358, 66 and 20 nmol/L at 1, 5 and 24 h against the observed
# mean 404 at 1 h and medians of about 50 and 20 at 5 and 24 h.  The basic
# model is used rather than the white-count model (Vc = 412 x [1 + 0.0138
# (WBC - 39)], the term's RSE 47%): the Patient Profile has no white count,
# and at the reference count the two differ only in Vc.  Krogh-Madsen 2012
# (adults with AML, in the registry) used this model as its NONMEM prior; its
# own estimates move Q and Vp, which nothing sampled between 1 and 5 h here
# identifies well, and agree on clearance within 12%.  Daunorubicinol was
# measured but is not in the final model.
#
# DOSE BASIS
# ==========
# Not named in the paper (doses in mg/m2, concentrations in nmol/L, no molar
# mass printed); doses are entered as labelled with no conversion (the
# hydrochloride is 6.9% heavier than the base).
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# No size covariate, and the paper prints no weight: its parameters are taken
# as the reference adult's, scaled by the library's factors with the switch
# on and fixed with it off.
#
# PREGNANCY (R/pregnancy.R): no daunorubicin comparison; nothing applied.
# NO EFFECT SITE, NO BAND
# -----------------------------------------------------------------------------

# Bogason 2011, Table 2 (basic model)
DAUNORUBICIN_BOGASON <- list(cl = 115, v1 = 373, q = 252, v2 = 1120)

#' Daunorubicin, intravenous (Bogason 2011)
#'
#' Two compartments; see the header of \code{R/drugs_daunorubicin.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
daunorubicin <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  p <- DAUNORUBICIN_BOGASON
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)

  default <- list(
    v1  = p$v1 * size$volume,
    v2  = p$v2 * size$volume,
    v3  = 1,                                 # placeholder: two compartments
    cl1 = p$cl / 60 * size$clearance,
    cl2 = p$q / 60 * size$clearance,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Bogason A, Quartino AL, Lafolie P, et al. Inverse relationship between ",
    "leukaemic cell burden and plasma concentrations of daunorubicin in patients ",
    "with acute myeloid leukaemia. Br J Clin Pharmacol 2011;71:514-521. 40 adults: ",
    "two compartments (basic model), CL 115 L/h, Vc 373 L, Q 252 L/h, Vp 1120 L. ",
    "Doses as labelled; plasma daunorubicin in ng/mL. ",
    "https://doi.org/10.1111/j.1365-2125.2010.03894.x"
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
