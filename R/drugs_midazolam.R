# -----------------------------------------------------------------------------
# Midazolam: Buhrer's three-compartment intravenous kinetics (Zomorodi 1998)
# -----------------------------------------------------------------------------
# The source of the disposition and of the time to peak effect is written out
# in the function below.  Units: time in minutes, volumes in litres,
# clearances in L/min, concentrations in mcg/mL.
#
# PREGNANCY
# =========
# Hebert 2008 gave 13 women 2 mg of ORAL midazolam at 28-32 weeks of
# gestation and again 6-10 weeks after delivery (paired, noncompartmental):
# the unbound apparent oral clearance was 1.72 times higher in pregnancy
# (593 +/- 237 against 345 +/- 103), and Quinney 2012, from the same data,
# reports the total oral AUC 9.5 ng.h/mL in pregnancy against 17.4 after
# delivery, a total apparent clearance 17.4 / 9.5 = 1.83 times higher.
# Fitting a semi-mechanistic model of CYP3A metabolism to those data,
# Quinney attributes the change to hepatic CYP3A activity 1.6 times higher,
# with the unbound fraction 20% higher (albumin 38.8 against 47.7 g/L) and
# the extraction in the gut wall and the hepatic blood flow unchanged.
#
# This model is INTRAVENOUS only, and an oral ratio overstates the change in
# the clearance of an intravenous dose: the oral clearance contains no blood
# flow limit, and midazolam's hepatic extraction is intermediate.  So the
# oral data are carried to the intravenous clearance through Quinney's
# mechanism and the well-stirred liver, CL = QH fu CLint / (QH + fu CLint),
# with an assumed hepatic blood flow QH of 1.5 L/min:
#
#     fu CLint, nonpregnant = CL QH / (QH - CL)
#                           = 0.5352 x 1.5 / (1.5 - 0.5352) = 0.832 L/min
#     fu CLint, pregnant    = 0.832 x 1.6 (CYP3A) x 1.2 (fu) = 1.598 L/min
#     CL, pregnant          = 1.5 x 1.598 / (1.5 + 1.598) = 0.774 L/min
#     ratio                 = 0.774 / 0.5352 = 1.45
#
# (0.5352 L/min is this model's CL1 at the 70 kg reference.)  The same
# assumptions predict an ORAL ratio of 1.6 x 1.2 = 1.92, the gut wall being
# unchanged, against the 1.83 observed, so they are consistent with Hebert's
# data.  That derivation depends on the assumed hepatic blood flow (with QH
# 2.0 L/min the same arithmetic gives 1.54) and on Quinney's assumption that
# it is unchanged in pregnancy, so it is the corroboration, not the factor.
#
# The factor is from the one INTRAVENOUS comparison, Wilson 1987 (read in
# full, 2026-10-19): 5 mg intravenously, plasma sampled for 2 h,
# non-compartmental, in women in active labour (n = 12, 71 kg), awaiting
# caesarean section 3 h before surgery (n = 10, 73 kg), at the start of
# caesarean section (n = 8) and two gynaecological groups (n = 10 each, 60
# kg, one given oxytocin).  Clearance 6.5, 9.0, 13.9, 8.7 and 7.0 mL/min/kg.
# The women awaiting caesarean section, pregnant but neither labouring nor
# anaesthetised, are the state the flag describes.  This model scales
# clearance with size, so each group is set against the model's clearance
# for a nonpregnant woman of its age and weight: the awaiting group cleared
# 1.425 times the model's, the two gynaecological groups 1.238 and 0.996
# (mean 1.117), so the pregnancy ratio is 1.425 / 1.117 = 1.28 (1.15 against
# the group without oxytocin alone; oxytocin did not change the
# gynaecological levels significantly).  With the Patient Profile's Pregnant
# field on, CL1 is multiplied by MIDAZOLAM_PREGNANCY_CL_FACTOR = 1.28, below
# the oral derivation's 1.45 and in the same direction; the volumes and the
# distribution clearances are unchanged.  In ACTIVE LABOUR the same
# arithmetic gives 0.91: labour lowers the clearance and shrinks the volume,
# and the flag does not represent it.  The plotted concentration is
# total midazolam; with the unbound fraction 20% higher, unbound midazolam
# falls less than the total.  The time to peak effect, 3.0 min, is held, as
# it is for every covariate, so getDrugPK() solves a ke0 about 12% smaller
# on the faster-clearing disposition.
#
# Kanto 1984 (six women just after caesarean delivery, radioreceptor assay)
# found a clearance of 13.18 mL/min/kg, 1.72 times this model's 7.64
# mL/min/kg at 70 kg.  Ansari 2016 lists midazolam by direction only (a lower
# peak, the half-life unchanged); Hebert's paired data supersede that here.
# Late pregnancy (the third trimester) is the state described.  (Claude
# Code, 2026-10-19, at the request of Steven L. Shafer; see R/pregnancy.R and
# inst/extdata/pregnancyRegistry.csv.)
#
# References
# ----------
# Zomorodi K et al.  Anesthesiology 1998;89:1418-1429.
#   https://pubmed.ncbi.nlm.nih.gov/9856717/
# Buhrer M et al.  Clin Pharmacol Ther 1990;48:544-554 and 555-567.
# Hebert MF, Easterling TR, Kirby B et al.  Clin Pharmacol Ther
#   2008;84:248-253.  https://doi.org/10.1038/clpt.2008.1 (pregnancy)
# Quinney SK, Mohamed AN, Hebert MF et al.  CPT Pharmacometrics Syst
#   Pharmacol 2012;1:e2.  https://doi.org/10.1038/psp.2012.5 (pregnancy:
#   the mechanism carried to the intravenous clearance)
# Wilson CM, Dundee JW, Moore J, Howard PJ, Collier PS.  Anaesthesia
#   1987;42:1057-1062.  https://doi.org/10.1111/j.1365-2044.1987.tb05168.x
#   (pregnancy, intravenous: the factor)
# Kanto J, Aaltonen L, Erkkola R, Aarimaa L.  Acta Anaesthesiol Scand
#   1984;28:116-118.  https://doi.org/10.1111/j.1399-6576.1984.tb02024.x
#   (pregnancy, intravenous, after caesarean delivery)
# -----------------------------------------------------------------------------

# Elimination clearance in pregnancy relative to the published value: Wilson
# 1987's women awaiting caesarean section against its gynaecological women,
# each against this model at its own age and weight (1.425 / 1.117 = 1.28);
# Hebert 2008's oral data through a well-stirred liver give 1.45 (see the
# header).
MIDAZOLAM_PREGNANCY_CL_FACTOR <- 1.28

midazolam <- function(weight, height, age, sex, adjustToFFM = TRUE,
                      pregnant = FALSE)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters
  # Source: three-compartment kinetics fitted to the data of Buhrer et al.
  # (Clin Pharmacol Ther 1990;48:544-554), men aged 33 to 43 given 3.75 to
  # 25 mg at 5 mg/min.  Neither of Buhrer's 1990 papers prints the parameters
  # (part II, 48:555-567, fitted a three-compartment model to each experiment
  # but reports only the pharmacodynamics).  They are printed in Zomorodi et
  # al. (Anesthesiology 1998;89:1418-1429), Table 3, column "Buhrer": V1 3.3,
  # V2 17.56, V3 96.76 L; Cl1 0.54, Cl2 2.01, Cl3 0.83 L/min; exponents 1.097,
  # 0.047, 0.0031 /min.  Zomorodi drove the study's TCI with this set, which
  # STANPUMP used for midazolam until later versions took Zomorodi's own ICU
  # kinetics (Somma et al., Anesthesiology 1998;89:1430-1443, footnote).
  # The values below convert exactly from V1 = 3.3 L, k21 = 0.1147 and
  # k31 = 0.0086 /min and exponents 1.098, 0.047 and 0.0031 /min, the form in
  # which the set was entered.  Earlier versions of this file cited Mould et
  # al. (Clin Pharmacol Ther 1995;58:35-43), which reports only
  # noncompartmental kinetics.
  # tPeak: 3.0 min, from the same data as the kinetics.  Buhrer's parametric
  # t1/2 ke0 of 5.6 min (part II, text), fitted against his three-compartment
  # kinetics, peaks at 2.97 min with these kinetics; his nonparametric 4.8 min
  # (part II, Table III) peaks at 2.7 min.  His observed peak EEG effect after
  # 3.75 mg given over 45 s was at 2.9 and 2.2 min (part I, Table II).  Until
  # 2026 the library used 4 min, from no recorded source; Mould's t1/2 ke0 of
  # 3.2 min would give 2.2 min.
  # Size scaling (see docs/weight-adjustment.md): the published parameters
  # describe a 70 kg adult.  Volumes scale with fat-free mass relative to the
  # 70 kg, 170 cm reference male, clearances with that ratio ^ 0.75
  # (Al-Sallami 2015).  adjustToFFM = FALSE reproduces the former behaviour
  # exactly: no size scaling, the parameters were used as published.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  # Pregnancy (see the file's header): elimination clearance only.
  clPregnancy <- if (isTRUE(pregnant)) MIDAZOLAM_PREGNANCY_CL_FACTOR else 1

  default <- list(
    v1 = 3.3        * size$volume,
    v2 = 17.56348   * size$volume,
    v3 = 96.75715   * size$volume,
    cl1 = 0.5351973 * size$clearance * clPregnancy,
    cl2 = 2.014531  * size$clearance,
    cl3 = 0.8321115 * size$clearance
  )
  
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))
  
  reference <- paste0(
    "Zomorodi K et al., Anesthesiology 1998;89(6):1418-1429, Table 3: ",
    "kinetics fitted to the data of Buhrer M et al., Clin Pharmacol Ther ",
    "1990;48(5):544-554, used by STANPUMP for target-controlled infusion; ",
    "time to peak effect from Buhrer M et al., Clin Pharmacol Ther ",
    "1990;48(5):555-567. https://pubmed.ncbi.nlm.nih.gov/9856717/"
  )
  if (isTRUE(pregnant)) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("clearance x ", MIDAZOLAM_PREGNANCY_CL_FACTOR),
      paste0("Wilson CM et al., Anaesthesia 1987;42:1057-1062. ",
             "https://doi.org/10.1111/j.1365-2044.1987.tb05168.x; ",
             "Hebert MF et al., Clin Pharmacol Ther 2008;84:248-253. ",
             "https://doi.org/10.1038/clpt.2008.1; Quinney SK et al., CPT ",
             "Pharmacometrics Syst Pharmacol 2012;1:e2. ",
             "https://doi.org/10.1038/psp.2012.5")))
  }
  # Band, mcg/mL: moderate sedation (Ramsay 3 or 4), the end point the library
  # draws for the sedatives (Steven L. Shafer, 2026-10-27), from Barr J et al.,
  # Anesthesiology 2001;95:286-298 (full text read), the basis of lorazepam's
  # band: plasma midazolam with an even chance of a Ramsay score of at least 3,
  # 4 and 5 (Model H) is 101, 208 and 304 ng/mL, so the median patient scores 3
  # or 4 from 101 to 304.  It replaced 0.04-0.12, whose source was not recorded.  The AGNP 2026 range (Hart 2026, Table 5), 50-150 ng/mL,
  # is for oral premedication and is recorded in therapeuticRanges.csv, not
  # drawn: the guideline cites Matsuda 2022, who quote it from a textbook
  # (Cote, Lerman and Anderson 2013, p. 141), and whose own children had
  # median concentrations of 47.6 ng/mL sedated and 49.8 not sedated.
  typical <- .208
  lowerTypical <- .101
  upperTypical <- .304
  MEAC <- 0
  tPeak <- 3.0
  return(
    list(
      PK = PK, 
      tPeak = tPeak, 
      MEAC = MEAC, 
      typical = typical, 
      upperTypical = upperTypical, 
      lowerTypical = lowerTypical, 
      reference = reference
    )
  )
}
