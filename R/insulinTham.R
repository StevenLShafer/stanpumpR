# -----------------------------------------------------------------------------
# Subcutaneous insulins: the Tham 2017 population model, shared by the
# insulin products in the library
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, doses in
# units (U), concentrations in mcU/mL (= mU/L; 1 mcU/mL of human insulin is
# about 6 pmol/L).  Concentration only: no glucose, glucose infusion rate or
# HbA1c is plotted, because the source is a pharmacokinetic model alone ("it
# incorporates only PK information").  Drafted by Claude Code, 2026-10-17, at
# the request of Steven L. Shafer; each product is its own drug,
# R/drugs_insulin<Product>.R, and the antidiabetic registry
# (inst/extdata/antidiabeticRegistry.csv) lists them.
#
# SOURCE
# ======
# Tham LS, Schneck K, Ertekin A, Reviriego J.  Modeling pharmacokinetic
# profiles of insulin regimens to enhance understanding of subcutaneous
# insulin regimens.  J Clin Pharmacol 2017;57:1126-1137 (PMC5573917).
# 16 PK and euglycaemic clamp studies, mostly in healthy volunteers.  "A
# 1-compartment model with first-order absorption and elimination", with an
# absorption rate constant for each formulation and an apparent volume and
# clearance for each insulin; glargine and lispro add a zero-order
# absorption to the first-order one.  Table 2, final estimates (per hour, L,
# L/h, h):
#
#     preparation          ka      lag    first-order  zero-order   V/F   CL/F
#                                         fraction     duration
#     regular U100         0.67    -      1            -            178   127
#     regular U500         0.185   -      1            -            178   127
#     NPH                  0.129   0.378  1            -            178   127
#     lispro               0.989   0.265  0.729        1.06         43.0  30.5
#     lispro protamine     0.0365  -      1            -            43.0  30.5
#     glargine (Basaglar)  0.0830  -      0.822        0.612        768   74.5
#
# "Model outputs were expressed in units (U) of regular insulin
# concentration": a dose in U over V/F in L is U/L, and x1000 is mcU/mL.
# Premixes are component sums ("a 10 U dose of Mix 70/30 was considered to
# contain 7 U of NPH and 3 U of regular insulin"; Mix 75/25 and 50/50 are
# lispro protamine and lispro).
#
# THE STRUCTURE, AND WHAT IS LEFT OUT
# ===================================
# * The zero-order input enters the central compartment directly, beside the
#   first-order depot: the paper's Figure 1 draws k0 from the subcutaneous
#   site into the central volume next to ka, for lispro and for glargine.
#   The supplement ("Supplement Methods: Pharmacokinetic Model Equations")
#   defines the lag as "lag time for insulin lispro first-order absorption"
#   (and NPH's likewise), and the zero-order input as running "when t <=
#   DUR" with t the time after the dose: the depot is lagged, the
#   zero-order input is not.  That is the structure implemented
#   (zeroOrderSC, into = "central", R/subcutaneous.R).
# * COVARIATES (Table 2, with the equations of the supplement's Word file,
#   whose equation objects the PDF had lost):
#
#     human insulins  V/F  = 178 x (BW / 80)^1.62
#                     CL/F = 127 x (Dose / 10)^-0.282
#     regular U100    ka   = 0.67 x (Dose / 10)^-0.275
#     lispro          V/F  = 43.0 x (BW / 80)^2.48
#     glargine        V    = 768 x (1 + 0.00728 (BW - 71.4))
#                     F    = (Dose / 35)^-0.300
#
#   Table 2 prints the weight power 1.62 in the regular U100 column only,
#   but the supplement's equation puts it on the volume all the human
#   insulins share, "Vd_Reg/NPH"; the equation is followed.  "Dose" is the
#   units of one injection: for regular U100's ka the regular share of it
#   (a 10 U dose of Mix 70/30 "contain[s] 7 U of NPH and 3 U of regular"),
#   and for the human insulins' CL/F the whole human-insulin dose.
#   Weight and dose are held within the ranges Table 1 studied for each
#   insulin (THAM_WEIGHT_RANGE, THAM_DOSE_RANGE): lispro's steep power, for
#   one, was fitted to 52.6 to 86 kg.  Weight is the pharmacokinetic weight
#   with the fat-free-mass switch on and the total weight with it off; no
#   other size scaling is applied, since the source's own terms are the
#   size model.
#
#   A dose covariate is a property of each injection: Tham fitted single
#   doses.  Glargine's F scales each dose by its own size
#   (scDoseBioavailability, applied in simCpCe()).  The human insulins' ka
#   and CL/F make each dose size its own linear system: simCpCe() simulates
#   the doses of each size with the parameters for that size and adds the
#   results (doseDependent), which reproduces Tham's single doses exactly and
#   superposes repeated ones.  With no dose given, the model is at the
#   reference dose (10 U), which is what the help tables show.
# * The endogenous-insulin baseline Tham estimated (regular and NPH) and the
#   C-peptide correction of the glargine data are not added: the curve is
#   the exogenous insulin alone.  The glargine row was fitted to Basaglar
#   (LY IGlar) on an immunoreactive assay; it is not the active metabolite M1
#   and not a fit to Lantus, Semglee or Rezvoglar, which Tham used only as an
#   external check.  Lispro is Humalog U100; Humalog U200 was a validation set
#   only and is not offered.
#
# PREGNANCY
# =========
# Insulin requirements rise in late pregnancy mostly through insulin
# resistance, a pharmacodynamic change that is not plotted.  The clearance
# of insulin rises too.  Catalano 1998 measured the metabolic clearance rate
# of infused insulin by hyperinsulinaemic-euglycaemic clamp in 16 women
# before conception, at 12-14 weeks and at 34-36 weeks: 442, 514 and 526
# mL/m2/min (p = 0.0005), the same in women with normal and abnormal glucose
# tolerance; late against before conception, 526 / 442 = 1.19.  For
# absorption, Goudie 2015 (Bayesian population model of insulin aspart by
# pump in 22 women with type 1 diabetes at 12-33 weeks) found the time to
# peak lengthening by 1.7% per week of gestation and the clearance per kg
# hardly changing (-0.33% per week).
#
# With the Patient Profile's Pregnant field on, the apparent clearance CL/F
# of every product (every insulin and every component of a premix, through
# this one builder) is multiplied by INSULIN_PREGNANCY_CL_FACTOR = 1.19
# (Catalano).  Absorption (ka, lag, the zero-order input), bioavailability
# and V/F are unchanged: Goudie's gestational slowing of absorption is in
# aspart, which the library does not model, and is not carried to these
# formulations.  Catalano's clearance is the systemic clearance of
# intravenous insulin at clamp concentrations, per m2 of body surface area
# (the ratio of absolute clearances is probably a little higher, since body
# surface area rises); it is applied to the apparent CL/F on the assumption
# that subcutaneous bioavailability is unchanged, for which there are no
# data.  Insulin is not bound, so total and free are one.
#
# NOTE ON WEIGHT.  Tham's V/F scales steeply with weight: (BW/80)^1.62 for
# the human insulins, (BW/80)^2.48 for lispro, linearly for glargine, held to
# the studied weights.  A term-pregnant weight entered in the Patient Profile
# therefore already inflates V/F, by weight alone (70 to 82 kg multiplies
# the human insulins' V/F by 1.29 and lispro's by 1.48), with no pregnancy
# evidence behind it: Tham's subjects were not pregnant, and the weight
# gained in pregnancy (fetus, placenta, amniotic fluid, water) is not the
# weight his covariate describes.  Entering the pre-pregnancy weight avoids
# it.  Claude Code, 2026-10-19.  See R/pregnancy.R.
#
# References
# ----------
# Tham LS et al., J Clin Pharmacol 2017;57:1126-1137.
#   https://doi.org/10.1002/jcph.899
# Catalano PM, Drago NM, Amini SB.  Longitudinal changes in pancreatic
#   beta-cell function and metabolic clearance rate of insulin in pregnant
#   women with normal and abnormal glucose tolerance.  Diabetes Care
#   1998;21:403-408.  https://doi.org/10.2337/diacare.21.3.403 (pregnancy)
# Goudie RJB, Hovorka R, Murphy HR, Lunn D.  Rapid model exploration for
#   complex hierarchical data: application to pharmacokinetics of insulin
#   aspart.  Stat Med 2015;34:3144-3158.  https://doi.org/10.1002/sim.6536
#   (pregnancy, aspart absorption; not applied)
# -----------------------------------------------------------------------------

THAM_INSULIN_REFERENCE <- "https://doi.org/10.1002/jcph.899"

# Pregnancy (see the header): CL/F of every product.  Catalano 1998, clamp
# metabolic clearance rate at 34-36 weeks against before conception, 526 /
# 442 mL/m2/min = 1.19.
INSULIN_PREGNANCY_CL_FACTOR <- 1.19

# Apparent disposition per insulin (Table 2): V/F in L at the reference
# weight, CL/F in L/h at the reference dose, and the weight covariate on V/F
# (a power of BW/reference, or glargine's linear slope per kg)
THAM_DISPOSITION <- list(
  human    = list(v = 178,  cl = 127,  weight = list(power = 1.62, reference = 80),
                  clDose = list(power = -0.282, reference = 10)),
  lispro   = list(v = 43.0, cl = 30.5, weight = list(power = 2.48, reference = 80)),
  glargine = list(v = 768,  cl = 74.5, weight = list(slope = 0.00728, reference = 71.4))
)

# The weights (kg) and single doses (U) Table 1 studied, pooled over each
# insulin's formulations; covariates are held within them
THAM_WEIGHT_RANGE <- list(human = c(53.1, 124), lispro = c(52.6, 86),
                          glargine = c(42.8, 110))
THAM_DOSE_RANGE <- list(human = c(3, 100), glargine = c(14, 58))

# Glargine's dose-dependent bioavailability, (Dose / 35)^-0.300
THAM_GLARGINE_F_DOSE <- list(reference = 35, power = -0.300,
                             range = THAM_DOSE_RANGE$glargine)

# Regular U100's dose-dependent ka, (Dose / 10)^-0.275
THAM_REGULAR_KA_DOSE <- list(reference = 10, power = -0.275)

# Absorption per formulation (Table 2): ka /h, lag h, the fraction absorbed
# first-order, and the duration (h) of the zero-order rest
THAM_ABSORPTION <- list(
  regularU100     = list(label = "regular U-100",    ka = 0.67,   tlag = 0,     firstOrder = 1),
  regularU500     = list(label = "regular U-500",    ka = 0.185,  tlag = 0,     firstOrder = 1),
  nph             = list(label = "NPH",              ka = 0.129,  tlag = 0.378, firstOrder = 1),
  lispro          = list(label = "lispro",           ka = 0.989,  tlag = 0.265, firstOrder = 0.729,
                         zeroOrder = 1.06),
  lisproProtamine = list(label = "lispro protamine", ka = 0.0365, tlag = 0,     firstOrder = 1),
  glargine        = list(label = "glargine",         ka = 0.0830, tlag = 0,     firstOrder = 0.822,
                         zeroOrder = 0.612)
)

#' A Tham 2017 insulin product as a drug model
#'
#' @inheritParams cefazolin
#' @param insulin which disposition, a name of `THAM_DISPOSITION`
#' @param components the product's formulations and their shares of each
#'   dose: a named numeric vector, names from `THAM_ABSORPTION`, summing to 1
#'   (`c(nph = 0.7, regularU100 = 0.3)` for human 70/30).  At most two, and at
#'   most one with a zero-order share.
#' @param product the product as the reference string names it
#' @param dose units of one injection, for the dose covariates, or NULL for
#'   the reference dose
#' @param pregnant when \code{TRUE}, CL/F x INSULIN_PREGNANCY_CL_FACTOR (see
#'   the file's header)
#' @returns a list in the shape \code{getDrugPK()} expects
#' @keywords internal
thamInsulin <- function(weight, height, age, sex, adjustToFFM, insulin,
                        components, product, dose = NULL, pregnant = FALSE)
{
  stopifnot(abs(sum(components) - 1) < 1e-12, length(components) <= 2)
  disposition <- THAM_DISPOSITION[[insulin]]
  absorption <- THAM_ABSORPTION[names(components)]
  clamp <- function(x, range) min(max(x, range[1]), range[2])

  # Body weight on V/F, held within the studied range
  pkW <- if (isTRUE(adjustToFFM))
    pkSizeFactors(weight, height, age, sex, TRUE)$pkWeight else weight
  w <- clamp(pkW, THAM_WEIGHT_RANGE[[insulin]])
  vw <- disposition$weight
  v <- disposition$v * if (is.null(vw$slope)) (w / vw$reference)^vw$power else
    1 + vw$slope * (w - vw$reference)

  # The dose of one injection on the human insulins' CL/F and regular U100's
  # ka, held within the studied range; NULL is the reference dose
  doseFactor <- function(effect, units) {
    if (is.null(effect) || is.null(dose)) return(1)
    (clamp(units, THAM_DOSE_RANGE[[insulin]]) / effect$reference)^effect$power
  }
  cl <- disposition$cl * doseFactor(disposition$clDose, dose)
  # Pregnancy (see the header): CL/F alone
  if (isTRUE(pregnant)) cl <- cl * INSULIN_PREGNANCY_CL_FACTOR
  regular <- names(components) == "regularU100"
  for (i in which(regular))
    absorption[[i]]$ka <- absorption[[i]]$ka *
      doseFactor(THAM_REGULAR_KA_DOSE, components[[i]] * dose)

  # The zero-order share of each dose, and the first-order depots, fastest
  # first
  zeroShare <- vapply(seq_along(components), function(i)
    components[[i]] * (1 - absorption[[i]]$firstOrder), numeric(1))
  zeroOrder <- vapply(absorption, function(a) if (is.null(a$zeroOrder)) 0 else a$zeroOrder,
                      numeric(1))
  stopifnot(sum(zeroShare > 0) <= 1)
  depotShare <- components - zeroShare
  ka <- unname(vapply(absorption, `[[`, numeric(1), "ka"))
  tlag <- unname(vapply(absorption, `[[`, numeric(1), "tlag"))
  byKa <- order(ka, decreasing = TRUE)

  default <- list(
    v1  = v, v2 = 1, v3 = 1,
    cl1 = cl / 60, cl2 = 0, cl3 = 0,
    ka_SC = ka[byKa[1]] / 60,
    bioavailability_SC = 1,   # apparent CL/F and V/F
    tlag_SC = tlag[byKa[1]] * 60
  )
  if (length(components) == 2)
  {
    slow <- byKa[2]
    default$ka_SC_slow <- ka[slow] / 60
    # the slow depot's share of what goes first-order
    default$fraction_SC_slow <- depotShare[[slow]] / sum(depotShare)
    default$tlag_SC_slow <- tlag[slow] * 60
  }
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  zero <- which(zeroShare > 0)
  zeroOrderSC <- if (length(zero) == 1)
    list(fraction = zeroShare[[zero]], duration = zeroOrder[[zero]] * 60, into = "central")

  absorbed <- paste(vapply(seq_along(components), function(i) {
    a <- absorption[[i]]
    sprintf("%s%s ka %s/h%s%s", if (length(components) > 1)
      paste0(round(100 * components[[i]]), "% ") else "",
      a$label, format(a$ka),
      if (a$tlag > 0) sprintf(" lag %s h", format(a$tlag)) else "",
      if (a$firstOrder < 1) sprintf(", %s%% zero-order over %s h",
                                    format(100 * (1 - a$firstOrder)), format(a$zeroOrder)) else "")
  }, character(1)), collapse = "; ")

  covariateText <- switch(insulin,
    human = "V/F x (weight/80)^1.62, CL/F x (dose/10)^-0.282",
    lispro = "V/F x (weight/80)^2.48",
    glargine = "V x (1 + 0.00728 (weight - 71.4)), F (dose/35)^-0.300")
  if (any(regular)) covariateText <- paste0(covariateText, ", regular ka x (dose/10)^-0.275")

  reference <- paste0(
    "Tham 2017, ", product, ": one compartment, V/F ", format(disposition$v),
    " L, CL/F ", format(disposition$cl), " L/h, ", absorbed, "; ",
    covariateText, " (weight and dose held to the studied ranges); ",
    "exogenous insulin, concentration only. ", THAM_INSULIN_REFERENCE
  )
  if (isTRUE(pregnant)) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("CL/F x ", INSULIN_PREGNANCY_CL_FACTOR),
      paste0("Catalano PM et al., Diabetes Care 1998;21:403-408. ",
             "https://doi.org/10.2337/diacare.21.3.403")))
  }

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    zeroOrderSC = zeroOrderSC,
    # Glargine: each injection's bioavailability by its own size
    scDoseBioavailability = if (insulin == "glargine") THAM_GLARGINE_F_DOSE,
    # The human insulins: each dose size is its own linear system
    doseDependent = insulin == "human",
    reference = reference
  )
}
