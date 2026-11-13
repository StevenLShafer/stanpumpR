# -----------------------------------------------------------------------------
# Thioguanine, intravenous (Kitchen 1997): plasma 6-thioguanine in children
# and young adults, two compartments with saturable elimination
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg and concentrations in mcg/mL (= mg/L).
#
# Drafted by Claude Code, 2026-11-13, at the request of Steven L. Shafer, who
# asked for this paper (stanpumpR-papers-needed.csv, priority 6) and had it
# added to the papers archive (PMID_9815740).  Every number below was read on
# the pages of the PDF (pp. 714-715).
#
# THE SOURCE
# ==========
# Kitchen BJ, Balis FM, Poplack DG, O'Brien M, Jacobs RF, Adamson PC.  A
# pediatric phase I trial and pharmacokinetic study of thioguanine
# administered by continuous i.v. infusion.  Clin Cancer Res 1997;3:713-717.
# 18 patients 4-25 years (median 18; 12 male) with refractory solid tumours
# or ALL; 17 with pharmacokinetic sampling: 4 at 10 mg/m2/h for 24 h, 13 at
# 20 mg/m2/h (9 for 24 h, 4 for 36 h); plasma at 3 and 6 h, at the end of the
# infusion and 0.25-12 h after it; HPLC.  The 24-h infusions of both dose
# rates were fitted together (MLAB).
#
# THE MODEL (Fig. 1 and Results)
# ==============================
#     dCc/dt = k0/Vc - Vm Cc/(Km + Cc) - kcp Cc + kpc Xp/Vc
#     dXp/dt = kcp Vc Cc - kpc Xp
#
#     Vc 56 L/m2   Vm 3.7 uM/h   Km 2.5 uM   kcp 0.42 /h   kpc 0.29 /h
#
# Vm is a rate of change of CONCENTRATION, so the maximal elimination rate is
# Vm Vc (207 umol/h per m2).  Molecular weight 167.19 g/mol (not printed):
# Vmax = 3.7 x 56 x 0.16719 = 34.6 mg/h per m2, Km = 0.418 mg/L.
#
# CHECKS (test-drugs-thioguanineIV.R): the paper's clearances are its
# formula Vm Vc / (Km + Css), Css the concentration at the end of a 24-h
# infusion.  The model's 24-h concentrations, 1.01 uM at 10 mg/m2/h and 3.18
# uM at 20 mg/m2/h, put through that formula give 985 and 609 mL/min/m2
# against the paper's 987 and 608 (the two-figure Vm and Km).  (At 20 mg/m2/h the 24-h value is not yet the steady
# state, 3.41 uM; the measured mean was 4.1 uM, range 1.0-8.3.)
#
# WHAT IT DOES NOT COVER: the oral tablet, the only approved product (Tabloid);
# this is an intravenous research route, as pramlintideIV is.  The paper's
# thioguanine was reconstituted with sodium hydroxide to the sodium salt, the
# doses stated as thioguanine; doses in mg of thioguanine.  The thioguanine
# nucleotides, the active intracellular species, are not modelled.  TPMT and
# NUDT15 genotype govern the nucleotides, not plasma thioguanine, and are not
# covariates here.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model's own covariate, body-surface area (Vc and with it Vmax per m2;
# the paper names no formula, Mosteller's is used), from the pharmacokinetic
# weight with the switch on and total weight with it off.  Km and the rate
# constants do not scale.
#
# PREGNANCY: no human data; see its registry row.  NO EFFECT SITE, NO BAND.
# -----------------------------------------------------------------------------

# Kitchen 1997, Results
THIOGUANINE_KITCHEN <- list(
  vc = 56,       # L/m2
  vm = 3.7,      # umol/L/h
  km = 2.5,      # umol/L
  kcp = 0.42,    # /h
  kpc = 0.29     # /h
)
THIOGUANINE_MW <- 167.19   # g/mol (C5H5N5S), not printed in the paper

#' Thioguanine, intravenous (Kitchen 1997)
#'
#' Two compartments with Michaelis-Menten elimination, per square metre of
#' body-surface area, run by \code{advanceMichaelisMenten()}; see the header
#' of \code{R/drugs_thioguanineIV.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects, with a
#'   \code{michaelisMenten} block
#' @export
thioguanineIV <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  p <- THIOGUANINE_KITCHEN
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  bsa <- bsaMosteller(w, height)
  mg <- THIOGUANINE_MW / 1000                     # mg per umol

  vc <- p$vc * bsa                                # L
  vmax <- p$vm * vc * mg / 60                     # mg/min
  km <- p$km * mg                                 # mg/L

  default <- list(
    v1  = vc,
    v2  = 1,                                 # the peripheral compartment is in
    v3  = 1,                                 #   the michaelisMenten block
    # The low-concentration limit, Vmax / Km, for the help tables only: the
    # simulation uses the michaelisMenten block.
    cl1 = vmax / km,
    cl2 = 0,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Kitchen BJ, Balis FM, Poplack DG, et al. A pediatric phase I trial and ",
    "pharmacokinetic study of thioguanine administered by continuous i.v. infusion. Clin ",
    "Cancer Res 1997;3:713-717. 17 patients 4-25 years: two compartments, Vc 56 L/m2, ",
    "Michaelis-Menten elimination (Vm 3.7 uM/h, Km 2.5 uM), kcp 0.42 and kpc 0.29 /h; BSA ",
    "by Mosteller. An intravenous research route: the approved product is oral."
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference,
    michaelisMenten = list(
      vmax = vmax,
      km = km,
      k23 = p$kcp / 60,
      k32 = p$kpc / 60
    )
  )
}
