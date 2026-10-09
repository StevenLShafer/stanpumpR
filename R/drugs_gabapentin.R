# -----------------------------------------------------------------------------
# Gabapentin: oral only, one compartment, absorption that saturates with dose
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), total plasma (gabapentin is under 3%
# bound, so total and free are the same thing to within the assay).
#
# SOURCE
# ======
# Tran P et al. fitted 173 healthy Korean men (19-30 y, 52-88 kg, Cockcroft-
# Gault CrCL 66-170 mL/min) given single doses of 300, 400 or 800 mg in seven
# bioequivalence studies, rich sampling to 24 h (NONMEM, ADVAN2 TRANS2):
#
#     CL = 11.1 x (CrCL / 106.3)^0.331   L/h
#     V  = 81.0                          L
#     ka = 0.860 h^-1   (ABCB1 2677 GG/GT/GA; x 1.339 for TT/AT)
#     lag = 0.311 h
#     F  = 1 - 0.906 x Dose / (571 + Dose),   Dose in mg per administration
#
# F is 0.688 at 300 mg, 0.627 at 400 and 0.471 at 800, which the paper
# reports.  Three things are changed here, and each is a decision recorded
# with the person who made it.
#
# 1. CREATININE CLEARANCE: PROPORTIONAL, NOT ^0.331
# ==================================================
# The 0.331 was estimated over CrCL 66-170 mL/min in young men with normal
# kidneys, where Cockcroft-Gault mostly tracks muscle mass rather than
# filtration; the paper's own Fig. 2 has R^2 = 0.022.  A noisy covariate over a
# narrow range attenuates a fitted exponent towards zero.  Gabapentin is
# eliminated unchanged by the kidney, and its clearance is proportional to
# CrCL across renal impairment (Blum 1994, the Neurontin label; Ouellet 2001
# found CL/F = 0.116 x CrCL).  Extrapolated, 0.331 would give a 9 h half-life
# at CrCL 20 mL/min against the label's 52 h at CrCL < 30, which is the error
# that matters most clinically.  The exponent is therefore 1, anchored at
# Tran's median CrCL so that Tran's typical subject is unchanged.  (Steven L.
# Shafer, 2026-10-07.)
#
# 2. ABSOLUTE SCALE: RE-ANCHORED TO THE INTRAVENOUS VOLUME, 58 L
# ===============================================================
# Without intravenous data, Tran's F can only be relative: the model assumes
# that a vanishingly small dose is completely absorbed, and the volume and
# clearance are scaled to that assumption.  Tran's subjects had lower exposure
# than Western ones -- their own NCA AUC was 20.1 mcg.h/mL after 300 mg, against
# 24.8 for 300 mg q8h at steady state in the label and 28.3 in a Thai
# bioequivalence study; 23.6 after 400 mg against Gidal 2000's 34.1 -- and
# their model, used as published, predicts about 30-35% below the Western
# single-dose studies at every dose.
#
# The one absolute measurement available is the volume after 150 mg
# intravenously, 58 +/- 6 L (Neurontin label).  Tran's 81 L against it implies
# Tran's subjects absorbed about 72% of a very small dose.  So V is set to
# 58 L and clearance is scaled by the same 58/81, which keeps Tran's half-life,
# absorption, lag and dose-bioavailability curve exactly.  At Tran's median
# CrCL that clearance is 7.95 L/h (132 mL/min), close to gabapentin's measured
# renal clearance (110-141 mL/min, Urban 2008), as it should be for a drug
# cleared only by the kidney.  The re-anchored model predicts AUC 26, 31 and
# 40 mcg.h/mL after 300, 400 and 600 mg (observed 25-28, 34, 44) and a peak of
# 3.9 mcg/mL after 600 mg (observed 3.9-4.2).  (Steven L. Shafer, 2026-10-07.)
#
# On this scale bioavailability_PO = 1 means the fraction absorbed in the
# limit of a small dose, and F(Dose) is then the absolute bioavailability:
# 0.69 at 300 mg, against 0.60 for 300 mg three times daily in the label.
#
# 3. SATURABLE ABSORPTION: APPLIED DOSE BY DOSE
# =============================================
# The engine is linear, and a dose-dependent F does not change that: each
# oral dose is scaled by its own fraction absorbed before it enters the engine
# (oralSaturation below; oralSaturationFraction() in R/routes.R), and is then
# an ordinary input.  What is NOT represented is saturation shared between
# doses: doses entered as separate rows at the same time are each scaled by
# their own size, not by their sum, so enter a 900 mg dose as one 900 mg row.
#
# ABSORPTION
# ==========
# ka 0.860 /h and lag 0.311 h as published, for the ABCB1 2677 GG/GT/GA
# genotypes (87% of Tran's subjects).  TT/AT carriers absorb 34% faster;
# stanpumpR has no ABCB1 input, so that is not offered.  The plasma peak
# falls at 3.0 h for the 70 kg reference patient, inside the 2-4 h of the
# label and the bioequivalence literature.
#
# The lag is kept, not folded into ka as hydromorphone's were: it is an
# estimated parameter (RSE 8%).  It makes gabapentin the one drug in the
# library with a live absorption lag, so if a threshold is set under Drug
# Thresholds, time until threshold reads blank for the 18.7 min after each
# oral dose, before its absorption starts (test-recovery-lag.R).  Immediate-release capsules and
# tablets only: gastroretentive (Gralise) and gabapentin enacarbil (Horizant)
# are different inputs.
#
# RENAL FUNCTION
# ==============
# CrCL is Cockcroft-Gault, as in the source, at the patient's serum creatinine
# from the Patient Profile (a child's read on the adult scale).  When none is
# entered it is an ASSUMED NORMAL creatinine (R/renalFunction.R), which
# captures the decline of renal function with age and the sex difference but
# not renal impairment.  Gabapentin
# accumulates in renal impairment, and this is the drug in the library where
# entering the creatinine matters most.  Haemodialysis is not represented.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Tran tested weight, BSA and BMI on V and retained none of them, so V is the
# library's size-free case: x FFM / FFM_ref with the switch on, 58 L for
# everyone with it off (legacyVolume = 1).  Clearance carries size through
# Cockcroft-Gault, at the pharmacokinetic weight with the switch on and at
# total body weight with it off, and takes no further size factor.
#
# NO EFFECT SITE (see the constant below)
# =======================================
# Plotted as plasma only.  See GABAPENTIN_TPEAK.
#
# BAND
# ====
# 4.1-9.4 mcg/mL: the steady-state average concentration corresponding to the
# 1200-3600 mg/day doses effective in postherpetic neuralgia, from the
# exposure-response analysis of 690 patients in the Neurontin NDA (AUC over an
# 8-hour interval 32.8-75.1 mg.h/L, Healy 2023).  Orientation only: it is a
# chronic neuropathic pain exposure, not a perioperative target, and a single
# preoperative dose sits below it.  The GAP trial (Baos 2025, 1196 patients,
# 600 mg before surgery and 300 mg twice daily for two days) found no
# clinically important effect on length of stay, opioid use or acute pain.
#
# NOT MODELLED
# ============
# Food (a high-protein meal raises Cmax by about a third, Gidal 1996), the
# ABCB1 absorption covariate, OCTN1 renal secretion (Urban 2008), dialysis,
# and saturation shared between overlapping doses.
#
# References
# ----------
# Tran P et al., J Pharmacokinet Pharmacodyn 2017;44:567-579.
#   https://doi.org/10.1007/s10928-017-9549-6
# Neurontin (gabapentin) US prescribing information, section 12.3.
# Blum RA et al., Clin Pharmacol Ther 1994;56:154-159.
#   https://doi.org/10.1038/clpt.1994.118
# Ouellet D et al., Epilepsy Res 2001;47:229-241.
#   https://doi.org/10.1016/s0920-1211(01)00311-4
# Urban TJ et al., Clin Pharmacol Ther 2008;83:416-421.
#   https://doi.org/10.1038/sj.clpt.6100271
# Gidal BE et al., Epilepsy Res 2000;40:123-127.
#   https://doi.org/10.1016/s0920-1211(00)00117-0
# Healy P et al., Pharmacol Res Perspect 2023;11:e01138.
#   https://doi.org/10.1002/prp2.1138
# Baos S et al., Anesthesiology 2025;143:851-861.
#   https://doi.org/10.1097/ALN.0000000000005655
# Zhou L et al., Front Pharmacol 2026;17:1760901 (effect compartment, below).
#   https://doi.org/10.3389/fphar.2026.1760901
# Ben-Menachem E et al., Epilepsy Res 1992;11:45-49 (CSF, below).
#   https://doi.org/10.1016/0920-1211(92)90020-t
# Ben-Menachem E et al., Epilepsy Res 1995;21:231-236 (CSF at steady state).
#   https://doi.org/10.1016/0920-1211(95)00026-7
# Buvanendran A et al., Reg Anesth Pain Med 2010;35:535-538 (pregabalin CSF).
#   https://doi.org/10.1097/AAP.0b013e3181fa6b7a
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# TO BE SUPPLIED
# -----------------------------------------------------------------------------
# No human equilibration delay between plasma and the site of gabapentin's
# effect has been estimated, so gabapentin is plotted as plasma only.  What
# exists:
#
#   - Zhou 2026 (Front Pharmacol, doi:10.3389/fphar.2026.1760901) linked pain
#     scores to an effect compartment, but FIXED its rate out at 1/h without
#     giving a reason, and that rate alone sets the time course of the delay.
#   - CSF/plasma is about 0.1 at 6 h after a single dose (Ben-Menachem 1992)
#     and 0.06-0.34 at steady state (Ben-Menachem 1995): slow, partial entry,
#     which a ke0 effect site, equilibrating to plasma, does not describe.
#   - Pregabalin, its nearest analogue, peaks in CSF about 8 h after a 300 mg
#     dose (Buvanendran 2010).
#
# Setting this to a time to peak effect after an ORAL dose, in minutes, turns
# the effect site on: the drug already declares tPeakRoute = ROUTE_PO, so ke0
# is solved against the oral plasma curve.  It must be later than the oral
# plasma peak (about 180 min for the reference patient).  MEAC stays 0:
# gabapentin is not an opioid and does not belong on the MEAC panel.
GABAPENTIN_TPEAK <- 0   # minutes after an ORAL dose; 0 = no effect site
# -----------------------------------------------------------------------------


#' Gabapentin pharmacokinetics (oral)
#'
#' Tran et al. (2017), with clearance proportional to creatinine clearance and
#' the disposition re-anchored to the intravenous volume (see the header).
#' Each oral dose is scaled by its own dose-dependent bioavailability through
#' the returned \code{oralSaturation} block.
#'
#' @inheritParams cefazolin
#' @param adjustToFFM \code{TRUE} (the default) scales the volume to the
#'   patient's fat-free mass and evaluates Cockcroft-Gault at the
#'   pharmacokinetic weight; \code{FALSE} uses 58 L for everyone and
#'   Cockcroft-Gault at total body weight.  See the file's header.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
gabapentin <- function(weight, height, age, sex, adjustToFFM = TRUE,
                       creatinine = NULL)
{
  # Size scaling (see the header): V carries no size term in the source, so it
  # is 58 L for everyone with the switch off.  pkW is the weight Cockcroft-
  # Gault sees: the pharmacokinetic weight with the switch on, total weight
  # with it off.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  crcl <- creatinineClearanceCG(pkW, age, sex,       # mL/min
                                adultEquivalentCreatinine(creatinine, age, sex))

  # Tran 2017, re-anchored to the intravenous volume (header, point 2):
  # V 81 -> 58 L, and clearance by the same 58/81 so the half-life is Tran's.
  # Clearance proportional to CrCL at Tran's median (header, point 1).
  anchor <- 58 / 81
  v1  <- 81.0 * anchor * size$volume
  cl1 <- 11.1 * anchor * (crcl / 106.3) / 60   # L/min, renal covariate carries size
  v2  <- 1                                     # one compartment
  v3  <- 1
  cl2 <- 0
  cl3 <- 0

  ka_PO              <- 0.860 / 60   # 1/min, ABCB1 2677 GG/GT/GA
  bioavailability_PO <- 1            # fraction absorbed as the dose -> 0
  tlag_PO            <- 0.311 * 60   # min

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

  # Band, mcg/mL: steady-state average for the postherpetic neuralgia doses,
  # see the header.  Orientation only.
  typical      <- 6
  upperTypical <- 9.4
  lowerTypical <- 4.1

  reference <- paste0(
    "Tran P et al., J Pharmacokinet Pharmacodyn 2017;44:567-579. ",
    "One compartment with saturable absorption; clearance proportional to ",
    "Cockcroft-Gault creatinine clearance (from the entered creatinine or an ",
    "assumed normal one) and the disposition re-anchored to the intravenous ",
    "volume of 58 L; oral only. https://doi.org/10.1007/s10928-017-9549-6"
  )

  return(
    list(
      PK = PK,
      tPeak = GABAPENTIN_TPEAK,
      # An effect site, once there is one, would be timed after an oral dose.
      tPeakRoute = ROUTE_PO,
      MEAC = 0,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference,
      # Tran 2017: F = 1 - 0.906 x Dose / (571 + Dose), Dose in mg per
      # administration.  Applied to each oral dose by simCpCe().
      oralSaturation = list(Imax = 0.906, ID50 = 571)
    )
  )
}
