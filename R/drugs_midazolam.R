midazolam <- function(weight, height, age, sex, adjustToFFM = TRUE)
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

  default <- list(
    v1 = 3.3        * size$volume,
    v2 = 17.56348   * size$volume,
    v3 = 96.75715   * size$volume,
    cl1 = 0.5351973 * size$clearance,
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
  typical <- .100 
  upperTypical <- .040
  lowerTypical <- .120
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
