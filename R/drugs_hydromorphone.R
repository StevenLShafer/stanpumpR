hydromorphone <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters
  
  v1Ref <- 0.16 * 70   # 0.16 L/kg at the 70 kg reference
  k10 <- 0.116
  k12 <- 0.3
  k13 <- 0.08
  k21 <- 0.03
  k31 <- 0.00095
  
  tPeak <- 19.6
  MEAC <- 1.5/1000
  typical <- MEAC * 1.2
  upperTypical <- MEAC * 0.8
  lowerTypical <- MEAC * 2.0
  reference <- paste0(
    "Drover DR et al., Anesthesiology 2002;97(4):827-836. ",
    "https://pubmed.ncbi.nlm.nih.gov/12357147/ (disposition); ",
    "Coda BA et al., Anesth Analg 2003;97(1):117-123. ",
    "https://pubmed.ncbi.nlm.nih.gov/12818953/ (intranasal); ",
    "Lohela TJ et al., Anesth Analg 2021;133(2):423-434. ",
    "https://pubmed.ncbi.nlm.nih.gov/33177323/ (oral: apparent oral ",
    "clearance and time course of immediate-release hydromorphone)"
  )
  
  # Size scaling (see docs/weight-adjustment.md): the published parameters
  # describe a 70 kg adult.  Volumes scale with fat-free mass relative to the
  # 70 kg, 170 cm reference male, clearances with that ratio ^ 0.75
  # (Al-Sallami 2015).  adjustToFFM = FALSE reproduces the former behaviour
  # exactly: V1 proportional to weight with fixed rate
  # constants, so volumes and clearances both scaled with weight/70.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  v1 <- v1Ref * size$volume
  v2 <- v1Ref * k12 / k21 * size$volume
  v3 <- v1Ref * k13 / k31 * size$volume
  cl1 <- v1Ref * k10 * size$clearance
  cl2 <- v1Ref * k12 * size$clearance
  cl3 <- v1Ref * k13 * size$clearance
  
  # Oral, recalibrated 2026-10-10 (Claude Code, at the request of Steven L.
  # Shafer; see docs/meac-exposure.md section 4.2.1).
  #
  # The bioavailability used to be 0.6, which had no source: the comment
  # beside it cited Lamminsalo and Mandema, OXYCODONE references copied from
  # drugs_oxycodone.R.  At 0.6 an oral dose ran two to three times above the
  # measured concentrations, and oral hydromorphone came out about nine times
  # as potent as oral morphine against the CDC's five.
  #
  # Calibrated to Lohela 2021: 12 healthy fasted volunteers, 2.6 mg
  # immediate-release hydromorphone hydrochloride (Palladon capsules), IV
  # 0.02 mg/kg in the same subjects; placebo phase, geometric means: Cmax
  # 1.48 ng/mL, median tmax 1.0 h (0.5-1.5), AUC0-last 6.60 ng.h/mL, CL/F
  # 5.78 L/min, IV CL 1.88 L/min, F 0.33.
  #
  # F is NOT Lohela's 0.33.  Oral exposure is set by F / CL, and Lohela's
  # subjects cleared hydromorphone faster (1.88 L/min) than Drover's, whose
  # 1.30 L/min this model uses.  Lohela's F with Drover's clearance would
  # overstate oral exposure by about 45%.  So, as for oral morphine, F is set
  # so that the model's apparent oral clearance matches the one observed:
  #     F = CL_model / (CL/F)_Lohela = 1.2992 / 5.78 = 0.225
  # which agrees with the DILAUDID label (about 24% for the 8 mg tablet) and
  # with Drover's own 0.19, measured in the subjects this disposition comes
  # from.  Ritschel 1987 (51.4%, SD 29.3, n = 8) and Parab 1988 (same group)
  # are at the high end of a range Lohela gives as 13% to 62%.
  #
  # ka was then fitted to Lohela's mean oral curve (Figure 1A, read from the
  # plot, 0.5 to 8 h) with F fixed at 0.225.  The best fit is 0.0100 /min,
  # the value the model already had, and it reproduces the curve to within
  # about 15% at every sample: peak 1.18 ng/mL at 46 min against a mean
  # curve of 1.18 to 1.30 ng/mL between the 0.5 and 1 h samples.  (The 1.48
  # ng/mL Cmax in Lohela's table is the mean of individual peaks, which
  # always exceeds the peak of the mean curve.)  No lag: the 0.5 h sample is
  # already near the peak.
  ka_PO <- 0.01                  # 1/min; plasma peak at 46 min
  bioavailability_PO <- 0.225
  #
  # Oral liquid ("mg PO liquid", "mg/kg PO liquid").  The DILAUDID label
  # states that "bioequivalence between the DILAUDID 8 mg Tablet and an
  # equivalent dose of DILAUDID Oral Solution has been demonstrated", i.e.
  # Cmax and AUC within the 80-125% limits.  No published human study gives
  # the solution's own Cmax or tmax, so there is nothing to fit a separate
  # absorption to, and the liquid is deliberately NOT listed in
  # oralFormulations: its doses take the oral parameters above, exactly as
  # "mg PO" does.  The liquid units exist so that a dose can be entered as
  # it is prescribed, including per kg for children.  (Claude Code,
  # 2026-10-10, at the request of Steven L. Shafer.)

  # ---------------------------------------------------------------------
  # Intramuscular and intranasal, corrected 2026-10-06
  # ---------------------------------------------------------------------
  # These two routes previously reused the ORAL absorption constant and
  # bioavailability and then added lags of 90 and 180 min.  That put the
  # intramuscular plasma peak at 136 min and the intranasal peak at 226 min.
  # Intranasal hydromorphone has been measured twice and peaks at 15 to 25
  # min, so the model was out by roughly tenfold on the route that is chosen
  # precisely because it works quickly.
  #
  # The lags were also doing the wrong job.  A measured time to peak is
  # measured from the dose, so it belongs in the absorption constant; a lag
  # on top of an absorption constant already slow enough to produce that peak
  # counts the delay twice.  Both lags are now zero and the timing is carried
  # by ka, which is also what keeps the time-until-threshold readout working:
  # during a lag the engine has no effect-site state at all, so recovery read
  # exactly zero for the first three hours after an intranasal dose while the
  # drug was very much in the patient.
  #
  # INTRANASAL is anchored on Coda 2003: 24 healthy volunteers, 1 and 2 mg
  # intranasal against 2 mg intravenous, absolute bioavailability 52.4% and
  # 57.5%, median time to peak 20 and 25 min.  Davis 2004 found 46.9% and a
  # 15 min peak in allergic rhinitis, and quotes 57% for healthy volunteers.
  # ka below reproduces a 20 min peak; with bioavailability 0.55 a 2 mg dose
  # then peaks at 2.93 ng/mL, against the 3.02 to 3.56 ng/mL Davis measured.
  #
  # INTRAMUSCULAR HAS NO DIRECT CITATION and needs one.  No human
  # intramuscular hydromorphone pharmacokinetic study reporting time to peak
  # or bioavailability was found.  Bioavailability is set to 1 because an
  # intramuscular dose bypasses first pass entirely, and because the product
  # labelling gives the same dose by either route while oral needs several
  # times more.  The 30 min peak is a judgement: slower than the nasal mucosa,
  # faster than oral.  Moulin 1991 measured 78% for a SUBCUTANEOUS infusion,
  # which is the closest measured comparator and suggests 1.0 may be slightly
  # generous.
  ka_IM              <- 0.0128241366   # 1/min; plasma peak at 30 min
  bioavailability_IM <- 1.0
  ka_IN              <- 0.0149015907   # 1/min; plasma peak at 20 min
  bioavailability_IN <- 0.55

  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3,
    ka_PO = ka_PO,
    bioavailability_PO = bioavailability_PO,
    tlag_PO = 0,
    ka_IM = ka_IM,
    bioavailability_IM = bioavailability_IM,
    # Zero: the measured time to peak is already carried by ka_IM.
    tlag_IM = 0,
    ka_IN = ka_IN,
    bioavailability_IN = bioavailability_IN,
    # Zero: the measured time to peak is already carried by ka_IN.
    tlag_IN = 0
  )
  
  
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))
  
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
