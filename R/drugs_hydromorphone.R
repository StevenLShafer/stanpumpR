hydromorphone <- function(weight, height, age, sex)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters
  
  v1  <- 0.16 * weight
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
    "Ritschel WA et al., J Clin Pharmacol 1987;27(9):647-653. ",
    "https://pubmed.ncbi.nlm.nih.gov/2445789/ (oral bioavailability)"
  )
  
  v2 <- v1 * k12 / k21
  v3 <- v1 * k13 / k31
  cl1 <- v1 * k10
  cl2 <- v1 * k12
  cl3 <- v1 * k13
  
  # Oral.  NOTE: the comment that used to sit here cited Lamminsalo and
  # Mandema, which are OXYCODONE references; it was inherited from
  # drugs_oxycodone.R and does not describe these numbers.  The values are
  # left as they were.  For what it is worth, Ritschel 1987 measured absolute
  # oral bioavailability at 51.4% (SD 29.3) in eight volunteers, so 0.6 sits
  # at the upper end of a very wide observed range.
  ka_PO <- 0.01                  # 1/min; gives a plasma peak at 46 min
  bioavailability_PO <- 0.6

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
