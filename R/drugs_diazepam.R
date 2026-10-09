# -----------------------------------------------------------------------------
# Diazepam: three-compartment intravenous kinetics (Hung 1996) with the EEG
# effect site of Buhrer 1990, and oral and intramuscular routes
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL, total plasma.
#
# WHY NOT THE SPECIFICATION'S MODEL
# =================================
# The ChatGPT specification this file was built from proposed McCann 2025, a
# two-compartment model fitted to children (2.5-20.6 y, mostly obese) with the
# oral ka and F fixed by assumption.  At 70 kg it predicts a 10 mg oral peak
# of 90-145 ng/mL; adults reach about 300 (Hogan 2020, below).  Its prose and
# its Table 3 also disagree on V1 (52.8 against 58.2 L).  Mould 1995, the
# natural adult source, gives diazepam's effect site but no disposition: "the
# duration of sampling (3 hours after dosing) was not sufficient", so "values
# for t1/2, CL, and Vd-beta could not be calculated" (read in full).
#
# DISPOSITION
# ===========
# Hung OR, Dyck JB, Varvel J, Shafer SL, Stanski DR gave 4 healthy men (33.5 y,
# 80.5 kg, 171.3 cm) 30 mg of diazepam intravenously over 5 min, with arterial
# samples for 2 h and venous samples for 10 days, and fitted a three-
# compartment mammillary model to each.  Table I, means:
#
#     V1 3.43 L   V2 8.47 L   V3 87.51 L
#     CL1 0.027   CL2 1.103   CL3 0.335 L/min
#
# Half-lives 1.3 min, 24 min and 45 h; Vss 99 L (1.23 L/kg).  Checks:
#   Mould 1995 (12 men, venous): 0.1 and 0.2 mg/kg over 90 s gave 1120 and
#     2390 ng/mL at 3 min and AUC 0-3 h 33.9 and 66.6 ug.min/mL; for an 80 kg
#     subject this model gives 1030 and 2060, and 30.1 and 60.2 (about 10%
#     below, and arterial against venous).
#   Clearance 27 mL/min (0.34 mL/min/kg): "between 20 and 32 ml/min" (Klotz
#     1975, 33 volunteers); 26.6 mL/min (Greenblatt, Ther Drug Monit 1989, 48
#     men given 10 mg orally); 0.46 mL/min/kg in young men (Divoll 1983).
#     Terminal half-life 45 h: 44.2 h (the same 48 men), 33 h (Greenblatt,
#     Clin Pharmacol Ther 1989, 11 volunteers), about 20 h at 20 years (Klotz
#     1975).
# The study is small, and each parameter is a mean of four.  It is the only
# adult intravenous diazepam model with early arterial sampling whose
# parameters are published.
#
# EFFECT SITE
# ===========
# Buhrer M, Maitre PO, Crevoisier C, Stanski DR (1990) related arterial
# concentration to EEG voltage in volunteers given 15-50 mg: equilibration
# half-time 1.6 min (midazolam 4.8 min).  ke0 = ln 2 / 1.6 = 0.433 /min,
# carried as the time to peak effect it gives on this model's bolus curve,
# DIAZEPAM_TPEAK = 2.63 min.  Mould 1995 found 1.2 min (harmonic mean) for
# the DSST with venous samples, and Greenblatt 1989 that EEG changes "were
# maximal at the end of the diazepam infusion".  Diazepam reaches its peak
# effect about twice as fast as midazolam.
#
# ORAL AND INTRAMUSCULAR
# ======================
# Oral: bioavailability 0.94 (Divoll 1983, 5 mg against IV in 22 adults).
# Intramuscular: 1.0 (Hung 1996, by deconvolution and by dose-normalised AUC).
# Absorption rates are set so that the typical peak matches the observed
# mean peak, as for oxycodone (kept so by decision of Steven L. Shafer,
# 2026-10-09):
#   Oral, absorption half-life 20 min: 10 mg peaks at 302 ng/mL at 27 min.
#     Hogan 2020 (46 adults, 10 mg Valium fasting): geometric mean peaks 338
#     and 286 ng/mL (51-75 and 76-111 kg) at median 1.0 and 0.75 h; 406
#     ng/mL (arithmetic mean) in Greenblatt's 48 men; tmax 0.9 h (Divoll).
#   IM, absorption half-life 50 min: 10 mg peaks at 200 ng/mL at 53 min.
#     Hung 1996 (the same subjects as the disposition): 199 +/- 89 ng/mL at
#     34 +/- 8 min.
# Neither route's peak height and time can both be met with this disposition
# and a single first-order rate; the typical curves peak earlier (oral) or
# later (IM) than observed.  Hung's deconvolution showed why for IM: absorption
# peaks at 14 min and is still 20-50% of its peak rate at 1 h, as diazepam
# precipitates in muscle.  No lag.  Rectal gel and nasal spray are not
# offered.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Fixed published parameters (Hung's subjects averaged 80.5 kg), read as the
# reference adult's.  Volumes scale with fat-free mass relative to the 70 kg,
# 170 cm reference male and clearances with that ratio ^ 0.75;
# adjustToFFM = FALSE uses the published values for everyone.
#
# BAND AND THRESHOLD
# ==================
# Band 150-600 ng/mL, typical 300.  The bottom is a little above the
# effect-site concentration that halved DSST performance, 116-132 ng/mL
# (Mould 1995); 300 is cited as the effective therapeutic concentration
# (Chevassus 2004); 200-600 is the target in status epilepticus (Ku 2018).
# The time-until-threshold level (endCe in the CSV) is 150, the bottom of the
# band.  The EEG EC50s are higher: 269 (Greenblatt 1989), 958 at steady state
# (Buhrer 1990).
#
# NOT MODELLED
# ============
# Nordiazepam, by decision (Steven L. Shafer, 2026-10-09): about half of a
# diazepam dose reaches the circulation as nordiazepam (53%, Greenblatt
# 1988), which is active and has a half-life of days, so repeated dosing
# produces more effect than this curve shows.  Also
# not modelled: age (the half-life rises from about 20 h at 20 years to about
# 90 h at 80, from a larger volume with clearance unchanged; Klotz 1975), sex
# (the volume is larger in women; Divoll 1983), obesity, CYP2C19 and CYP3A4,
# hepatic disease (half-life more than doubled in cirrhosis; Klotz 1975), and
# the propylene glycol vehicle.
#
# References
# ----------
# Hung OR et al., Can J Anaesth 1996;43:450-455.
#   https://doi.org/10.1007/BF03018105
# Buhrer M et al., Clin Pharmacol Ther 1990;48:555-567.
#   https://doi.org/10.1038/clpt.1990.192
# Mould DR et al., Clin Pharmacol Ther 1995;58:35-43.
#   https://doi.org/10.1016/0009-9236(95)90070-5
# Divoll M et al., Anesth Analg 1983;62:1-8.
#   https://pubmed.ncbi.nlm.nih.gov/6849499/
# Hogan RE et al., Epilepsia 2020;61:455-464.
#   https://doi.org/10.1111/epi.16449
# Greenblatt DJ et al., Clin Pharmacol Ther 1989;45:356-365.
#   https://doi.org/10.1038/clpt.1989.41
# Greenblatt DJ et al., Ther Drug Monit 1989;11:652-657.
#   https://doi.org/10.1097/00007691-198911000-00007
# Klotz U et al., J Clin Invest 1975;55:347-359.
#   https://doi.org/10.1172/JCI107938
# Ku LC et al., CPT Pharmacometrics Syst Pharmacol 2018;7:718-727.
#   https://doi.org/10.1002/psp4.12349
# Chevassus H et al., BMC Clin Pharmacol 2004;4:3.
#   https://doi.org/10.1186/1472-6904-4-3
# Greenblatt DJ et al., J Clin Pharmacol 1988;28:853-859 (nordiazepam).
#   https://doi.org/10.1002/j.1552-4604.1988.tb03228.x
# McCann SM et al., J Clin Pharmacol 2025 (children; not used).
#   https://doi.org/10.1002/jcph.70027
#
# Drafted with Claude Code at the request of Steven L. Shafer, 2026-10-09,
# from a ChatGPT specification whose references and values were checked
# against the sources first (Hung and Mould in full).
# -----------------------------------------------------------------------------

# Buhrer 1990: equilibration half-time 1.6 min, carried as the time to peak
# effect it gives on the reference patient's bolus curve.
DIAZEPAM_TPEAK <- 2.63   # minutes after an IV bolus

#' Diazepam pharmacokinetics
#'
#' Three-compartment intravenous kinetics from Hung et al. (1996), the EEG
#' effect site of Buhrer et al. (1990), and oral and intramuscular absorption
#' set to the observed peaks.  See the file's header.
#'
#' @inheritParams cefazolin
#' @param adjustToFFM \code{TRUE} (the default) scales volumes to fat-free mass
#'   and clearances to its 0.75 power; \code{FALSE} uses the published values
#'   for everyone.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
diazepam <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Size scaling (see the header): fixed published values, read as the
  # reference adult's.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  # Hung 1996, Table I, means of four men
  default <- list(
    v1 = 3.43  * size$volume,
    v2 = 8.47  * size$volume,
    v3 = 87.51 * size$volume,
    cl1 = 0.027 * size$clearance,
    cl2 = 1.103 * size$clearance,
    cl3 = 0.335 * size$clearance,
    ka_PO = log(2) / 20,           # 1/min, set to the observed oral peak
    bioavailability_PO = 0.94,     # Divoll 1983
    tlag_PO = 0,
    ka_IM = log(2) / 50,           # 1/min, set to Hung's IM peak
    bioavailability_IM = 1,        # Hung 1996
    tlag_IM = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  # Band, ng/mL: seizure-rescue and status epilepticus targets (see header)
  typical      <- 300
  upperTypical <- 600
  lowerTypical <- 150

  reference <- paste0(
    "Hung OR et al., Can J Anaesth 1996;43:450-455 (three-compartment ",
    "intravenous kinetics, arterial sampling); effect site from Buhrer M et ",
    "al., Clin Pharmacol Ther 1990;48:555-567. ",
    "https://doi.org/10.1007/BF03018105"
  )

  return(
    list(
      PK = PK,
      tPeak = DIAZEPAM_TPEAK,
      MEAC = 0,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference
    )
  )
}
