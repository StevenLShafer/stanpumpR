# -----------------------------------------------------------------------------
# Prednisone: an oral prodrug whose active species is prednisolone
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL of TOTAL prednisone (free x 4, Xu's constant free
# fraction of 0.25).
#
# Prednisone has little glucocorticoid activity of its own; it is reduced to
# prednisolone, and the reverse reaction also runs.  The model is the Xu
# 2007 reversible pair described in R/drugs_prednisolone.R, used here from
# the prednisone side:
#
#   - The prednisone row is the exact two-compartment mammillary equivalent
#     of the pair for a prednisone dose, with the prednisolone pool as the
#     peripheral compartment.  It is written on TOTAL prednisone, so its
#     volumes are the free-concentration volumes divided by four.
#   - Prednisolone is the metabolite row.  The engine's metabolite link is a
#     one-way first-order formation convolved through prednisolone's own
#     model, which cannot represent the back-reaction directly.  The
#     formation constant is therefore calibrated so that the free
#     prednisolone AUC after a prednisone dose equals the exact value from
#     the source's AUC identity (0.1754 /h against the biochemical
#     LNL/VN = 0.2281 /h; the difference is the recycling the prednisolone
#     model's peripheral pool already contains).  Shape is approximate,
#     exposure exact.
#   - Oral prednisone enters 0.105 as prednisone and 0.645 as prednisolone
#     formed before reaching the circulation; 0.25 is lost.  Those cannot be
#     used directly, because most of the prednisone in plasma after an oral
#     dose is REGENERATED from the prednisolone that entered first, and a
#     one-way parent row cannot receive it.  The row therefore uses two
#     effective coefficients, solved from the source's AUC identities so
#     that both exposures are exact: an oral bioavailability of 0.4203
#     (total prednisone AUC exact) and a first-pass fraction of 0.4959
#     (prednisolone AUC exact, given the systemic branch).  Shape is
#     approximate, exposure exact, in the same sense as above.
#
# Like codeine, prednisone carries no effect site: tPeak is zero and the row
# is plasma only.
#
# ORAL ONLY.  Intravenous prednisone has been given in research but no
# routine product was verified; intravenous steroid is methylprednisolone or
# prednisolone.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Fixed published parameters: volumes scale with fat-free mass relative to
# the reference male, clearances with that ratio ^ 0.75; the switch off uses
# them as published.  Prednisone and prednisolone must make this call
# identically, because the formation constant is a clearance over a volume.
#
# References
# ----------
# Xu J, Winkler J, Derendorf H. J Pharmacokinet Pharmacodyn 2007;34:355-372.
#   https://doi.org/10.1007/s10928-007-9050-8
# -----------------------------------------------------------------------------

#' Prednisone pharmacokinetics (total concentration), forming prednisolone
#'
#' @inheritParams cefazolin
#' @param adjustToFFM scale volumes to the patient's fat-free mass and
#'   clearances to that ratio to the 0.75 power; when \code{FALSE}, use the
#'   published fixed parameters unscaled.
#' @returns a list in the shape \code{getDrugPK()} expects, naming
#'   prednisolone as the formed active species
#' @export
prednisone <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  sys  <- prednisolonePairSystem()

  # Total-concentration form: amounts are unchanged, concentrations are
  # free / fu, so volumes and clearances are divided by 1/fu = 4.
  fu <- XU_FU_PREDNISONE
  v1  <- sys$PN$v1 * fu * size$volume
  v2  <- sys$PN$v2 * fu * size$volume
  v3  <- 1
  cl1 <- sys$PN$cl1 * fu / 60 * size$clearance   # L/min
  cl2 <- sys$PN$cl2 * fu / 60 * size$clearance
  cl3 <- 0

  ka_PO              <- XU_KA_PN / 60            # 1/min
  bioavailability_PO <- sys$oralF_PN             # 0.4203, AUC-exact; see the header
  tlag_PO            <- 0

  # Formation: a rate constant on the prednisone AMOUNT, so it scales as a
  # clearance over a volume, the ratio ^ -0.25, like every other k.
  kFormation <- sys$kFormation / 60 * size$clearance / size$volume

  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3,
    ka_PO = ka_PO,
    bioavailability_PO = bioavailability_PO,
    tlag_PO = tlag_PO
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  # Band, TOTAL prednisone ng/mL after 20-40 mg.  Orientation only.
  typical      <- 30
  upperTypical <- 80
  lowerTypical <- 10

  reference <- paste0(
    "Xu J, Winkler J, Derendorf H. J Pharmacokinet Pharmacodyn 2007;34:355-372. ",
    "Prodrug side of the reversible pair; total prednisone plotted, free ",
    "prednisolone appears on the prednisolone row. ",
    "https://doi.org/10.1007/s10928-007-9050-8"
  )

  return(
    list(
      PK = PK,
      tPeak = 0,      # prodrug: no effect site of its own
      MEAC = 0,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference,
      metabolite = list(
        name              = "prednisolone",
        kFormation        = kFormation,
        firstPassFraction = sys$firstPass_PN,     # 0.4959, AUC-exact
        # prednisolone 360.44, prednisone 358.43 g/mol
        mwRatio           = 360.44 / 358.43
      )
    )
  )
}
