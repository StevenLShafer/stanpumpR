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
# WHAT IS ASSUMED, AND WHAT IS LEFT OUT
# =====================================
# * Where the zero-order input goes, and which branch the lispro lag delays,
#   are in the paper's supplement and Figure 1, which could not be read.  The
#   model takes the usual NONMEM form of parallel zero- and first-order
#   absorption: the first-order fraction into a depot, absorbed with the lag;
#   the rest straight into the central compartment at a constant rate over
#   the zero-order duration, from the moment of injection.  "The rapid onset
#   of insulin lispro's activity was best described by adding a 0-order
#   absorption model" supports an unlagged zero-order branch.  The registry
#   records the assumption (structure_verified FALSE).
# * Table 2 also has dose effects (on regular insulin's ka, -0.275, and on the
#   human insulins' CL/F, -0.282; on glargine's F, -0.300) and weight effects
#   on V/F (1.62 regular, 2.48 lispro, 0.00728 glargine), whose reference
#   dose, reference weight and functional form are in the same supplement.
#   They are NOT applied: every product uses Table 2's typical values, as the
#   specification asks until the equations are transcribed.  The typical
#   values are taken as the 70 kg reference adult's and scaled by the
#   library's fat-free-mass factors (legacyVolume = 1: fixed with the switch
#   off).
# * The endogenous-insulin baseline Tham estimated (regular and NPH) and the
#   C-peptide correction of the glargine data are not added: the curve is
#   the exogenous insulin alone.  The glargine row was fitted to Basaglar
#   (LY IGlar) on an immunoreactive assay; it is not the active metabolite M1
#   and not a fit to Lantus, Semglee or Rezvoglar, which Tham used only as an
#   external check.  Lispro is Humalog U100; Humalog U200 was a validation set
#   only and is not offered.
#
# References
# ----------
# Tham LS et al., J Clin Pharmacol 2017;57:1126-1137.
#   https://doi.org/10.1002/jcph.899
# -----------------------------------------------------------------------------

THAM_INSULIN_REFERENCE <- "https://doi.org/10.1002/jcph.899"

# Apparent disposition per insulin (Table 2): V/F in L, CL/F in L/h
THAM_DISPOSITION <- list(
  human    = list(v = 178,  cl = 127),
  lispro   = list(v = 43.0, cl = 30.5),
  glargine = list(v = 768,  cl = 74.5)
)

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
#' @returns a list in the shape \code{getDrugPK()} expects
#' @keywords internal
thamInsulin <- function(weight, height, age, sex, adjustToFFM, insulin,
                        components, product)
{
  stopifnot(abs(sum(components) - 1) < 1e-12, length(components) <= 2)
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  disposition <- THAM_DISPOSITION[[insulin]]
  absorption <- THAM_ABSORPTION[names(components)]

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
    v1  = disposition$v * size$volume, v2 = 1, v3 = 1,
    cl1 = disposition$cl * size$clearance / 60, cl2 = 0, cl3 = 0,
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

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    zeroOrderSC = zeroOrderSC,
    reference = paste0(
      "Tham 2017, ", product, ": one compartment, V/F ", format(disposition$v),
      " L, CL/F ", format(disposition$cl), " L/h, ", absorbed,
      "; Table 2 typical values (dose and weight effects not applied); ",
      "exogenous insulin, concentration only. ", THAM_INSULIN_REFERENCE
    )
  )
}
