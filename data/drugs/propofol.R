propofol <- function(weight = 70, height = 171, age = 50, sex = "male")
{
  # Units **************
  # Time: Minutes
  # Volume: Liters
  #cat("Starting Propofol.R\n")


  # Schnider

  default <- list(
    v1 = 4.27,
    v2 = 18.9-0.391*(age-53),
    v3 = 238,
    cl1 = 1.89+0.0456*(weight-77)-0.0681*(lbmJames(weight, height, sex)-59)+0.0264*(height-177),
    cl2 = 1.29-0.024*(age-53),
    cl3 = 0.836
  )


  # Eleveld
  # Pharmacokinetic / pharmacodynamic model for propofol for broad application
  # in anaesthesia and sedation
  # BJA 2018;120:942-959

  # NONMEM Control file from digital supplement

  # ; Al-sallami FFM
  # HT2=(HGT/100.)*(HGT/100.) # Height (in meters) squared
  # MATM=0.88+((1-0.88)/(1+(AGE/13.4)**(-12.7))) # Males
  # MATF=1.11+((1-1.11)/(1+(AGE/7.1)**(-1.1)))   # Females
  # MATR=0.88+((1-0.88)/(1+(35./13.4)**(-12.7))) Reference male
  # FFMM=MATM*42.92*(HT2)*WGT/(30.93*(HT2)+WGT)  # Male equation normalized to 216, page 946, math is OK
  # FFMF=MATF*37.99*(HT2)*WGT/(35.98*(HT2)+WGT)  # Female equation normalized to
  # FFMR=MATR*42.92*(1.7*1.7)*70./(30.93*(1.7*1.7)+70.) # Reference male
  # FFM=FFMM*(2-M1F2) + FFMF*(M1F2-1)
  # NFFM=FFM/FFMR
  # ; maturation
  # DV1=1
  # DV2=1
  # DV3=1
  # ; sigmoidal maturation of CL
  # PMW=PMA*52.
  # PMR=(35.+40./52.)*52.
  # ME50=EXP(THETA(8))
  # MGAM=EXP(THETA(9))
  # MCL=(PMW**MGAM)/(PMW**MGAM+ME50**MGAM)
  # RCL=(PMR**MGAM)/(PMR**MGAM+ME50**MGAM)
  # DCL=MCL/RCL
  # DQ2=1
  # ; sigmoidal maturation of Q3 based on 40 weeks gestation
  # PMEW=AGE*52.+40.
  # PMER=35.*52.+40.
  # QE50=EXP(THETA(14))
  # MQ3=PMEW/(PMEW+QE50)
  # RQ3=PMER/(PMER+QE50)
  # DQ3=MQ3/RQ3
  # ; aging
  # KV1=1
  # KV2=EXP(THETA(10)*(AGE-35.))
  # KV3=EXP(THETA(13)*(AGE)*(TECH-1))
  # KCL=EXP(THETA(11)*(AGE)*(TECH-1))
  # KQ2=1
  # KQ3=1
  # ; covariate structure
  # ; V1 scales sigmoid with weight
  # VV50=EXP(THETA(12))
  # CV1=WGT/(WGT+VV50)
  # RV1=70./(70.+VV50)
  # M1 =(CV1/RV1) * KV1 * DV1
  # VCV1=(A1V2-1)*(1-CV1)
  # V1 =EXP(THETA(1)+ETA(1)) * M1 * (1+VCV1*EXP(THETA(17)))
  # M2 =(WGT/70.)**1 * KV2 * DV2
  # V2 =EXP(THETA(2)+ETA(2)) * M2
  # M3 =(NFFM)**1 * KV3 * DV3
  # V3 =EXP(THETA(3)+ETA(3)) * M3
  # M4 =(WGT/70.)**0.75 * KCL * DCL
  # CL =EXP((2-M1F2)*THETA(4)+(M1F2-1)*THETA(15)+ETA(4)) * M4
  # RV2=EXP(THETA(2))
  # M5 =(V2/RV2)**0.75 * KQ2 * DQ2
  # KM5=1+EXP(THETA(16))*(1-MQ3)
  # Q2 =EXP(THETA(5)+ETA(5)+(A1V2-1)*THETA(18)) * M5 * KM5
  # RV3=EXP(THETA(3))
  # M6 =(V3/RV3)**0.75 * KQ3 * DQ3
  # Q3 =EXP(THETA(6)+ETA(6)) * M6
  # S1 =V1
  # ; error model
  # RESV=THETA(7)
  # $THETA
  # (1, 1.837860e+00, 3) ; v1ref(adult)=6.283 l
  # (2, 3.238730e+00, 5) ; v2ref=25.501 l
  # (4, 5.608800e+00, 7) ; v3ref=272.817 l
  # (0, 5.819830e-01, 1) ; clref(male)=1.790 l/min
  # (0, 5.596720e-01, 2) ; q2ref(adult)=1.750 l/min
  # (-1, 1.030460e-01, 1) ; q3ref=1.109 l/min
  # (0.15, 1.913070e-01, 0.5) ; log-error
  # (1, 3.744220e+00, 6) ; maturation CL E50=42.3 weeks
  # (0.1, 2.203300e+00, 4) ; maturation CL slope=9.05
  # (-0.1, -1.563300e-02, 0.1) ; V2 declines with age
  # (-0.01, -2.857090e-03, 0.01) ; CL declines with age with opiates
  # (2, 3.513130e+00, 5) ; V1 sigmoid E50=33.6 kg
  # (-0.1, -1.381660e-02, 0.1) ; V3 declines with age with opiates
  # (2, 4.223570e+00, 5) ; maturation Q3 E50=68.3 weeks
  # (0, 7.420430e-01, 1.5) ; clref(female)=2.100 l/min
  # (-8, 2.656420e-01, 3) ; q2ref(child) factor=1.304
  # (-3, 3.498850e-01, 2) ; v1 extra(max,venous)=1.419 l
  # (-3, -3.849270e-01, 2) ; q2 less for venous=0.681 l/min

  A1V2 <- 1 # (Interested in prediction arterial concentrations)

  #Implementation
  THETA <- c(
    1.837860e+00,    # 1 v1ref(adult)=6.283 l
    3.238730e+00,    # 2 v2ref=25.501 l
    5.608800e+00,    # 3 v3ref=272.817 l
    5.819830e-01,    # 4 clref(male)=1.790 l/min
    5.596720e-01,    # 5 q2ref(adult)=1.750 l/min
    1.030460e-01,    # 6 q3ref=1.109 l/min
    1.913070e-01,    # 7 log-error
    3.744220e+00,    # 8 maturation CL E50=42.3 weeks
    2.203300e+00,    # 9 maturation CL slope=9.05
   -1.563300e-02,    # 10 V2 declines with age
   -2.857090e-03,    # 11 CL declines with age with opiates
    3.513130e+00,    # 12 V1 sigmoid E50=33.6 kg
   -1.381660e-02,    # 13 V3 declines with age with opiates
    4.223570e+00,    # 14 maturation Q3 E50=68.3 weeks
    7.420430e-01,    # 15 clref(female)=2.100 l/min
    2.656420e-01,    # 16 q2ref(child) factor=1.304
    3.498850e-01,    # 17 v1 extra(max,venous)=1.419 l
   -3.849270e-01     # 18 q2 less for venous=0.681 l/min
  )


  # Al-sallami FFM
  BMI <-  weight / (height / 100)^2
  if (sex == "male")
  {
    FFM <- (0.88 + ((1 - 0.88) / (1 + (age / 13.4)^(-12.7)))) *
      42.92 * weight / (30.93 + BMI)
    M1F2 <- 1
  } else {
    FFM <- (1.11 + ((1 - 1.11) / (1 + (age/7.1)^(-1.1)))) *
      37.99 * weight / (35.98 + BMI)
    M1F2 <- 2
  }
  FFMReference <- 0.88 + ((1 - 0.88) / (1 + (35 / 13.4)^(-12.7))) *
    42.92 *70 / (30.93 + 70 / 1.7^2)
  NFFM <- FFM/FFMReference

  # maturation
  DV1 <- 1
  DV2 <- 1
  DV3 <- 1

  # sigmoidal maturation of CL
  #PMW  <- PMA * 52 # PMA = post menstural age. if age is 0, PMA is 40 weeks
  PMW   <- 40 + age * 52  # Age is always in units of years
  PMR  <- (35 + 40/52) * 52
  ME50 <- exp(THETA[8])
  MGAM <- exp(THETA[9])
  MCL  <- (PMW^MGAM) / (PMW^MGAM + ME50^MGAM)
  RCL  <- (PMR^MGAM) / (PMR^MGAM + ME50^MGAM)
  DCL  <- MCL / RCL
  DQ2  <- 1
  # sigmoidal maturation of Q3 based on 40 weeks gestation
  PMEW <- age * 52 + 40
  PMER <- 35 *  52 + 40
  QE50 <- exp(THETA[14])
  MQ3  <- PMEW / (PMEW + QE50)
  RQ3  <- PMER / (PMER + QE50)
  DQ3  <- MQ3 / RQ3
  # aging
  KV1  <- 1
  KV2  <- exp(THETA[10] * (age - 35))
  KV3  <- exp(THETA[13] * age) # * (TECH - 1)) TECH == 2 if opioids
  KCL  <- exp(THETA[11] * age) # * (TECH - 1))
  KQ2  <- 1
  KQ3  <- 1
  # covariate structure
  # V1 scales sigmoid with weight
  VV50 <- exp(THETA[12])
  CV1  <- weight / (weight + VV50)
  RV1  <- 70 /(70 + VV50)
  M1   <- (CV1 / RV1) * KV1 * DV1
  VCV1 <- (A1V2 - 1) * (1 - CV1)
  V1   <- exp(THETA[1]) * M1 * (1 + VCV1*exp(THETA[17]))
  M2   <- (weight / 70) * KV2 * DV2
  V2   <- exp(THETA[2]) * M2
  M3   <- (NFFM)^1 * KV3 * DV3
  V3   <- exp(THETA[3]) * M3
  M4   <- (weight / 70)^0.75 * KCL * DCL
  CL1   <- exp((2 - M1F2) * THETA[4]+(M1F2 - 1)*THETA[15]) * M4
  RV2  <- exp(THETA[2])
  M5   <- (V2 / RV2)^0.75 * KQ2 * DQ2
  KM5  <- 1 + exp(THETA[16]) * (1 - MQ3)
  CL2   <- exp(THETA[5] + (A1V2 - 1) * THETA[18]) * M5 * KM5
  RV3  <- exp(THETA[3])
  M6   <- (V3 / RV3)^0.75 * KQ3 * DQ3
  CL3   <- exp(THETA[6]) * M6

  default <- list(
    v1 = V1,
    v2 = V2,
    v3 = V3,
    cl1 = CL1,
    cl2 = CL2,
    cl3 = CL3
  )

  events <- c("default")
  PK <- sapply(events, function(x) list(get0(x)))
  print(str(PK))

  tPeak <- 1.600 # Anesthesiology 90:1502-1516, 1999
  # typical <- 3
  # upperTypical <- 4.0
  # lowerTypical <- 2.5
  # MEAC <- 0
  reference <- "Anesthesiology 1998"
  #cat("Exiting Propofol.R\n")


  return(
    list(
      PK = PK,
      tPeak = tPeak,
      reference = reference
    )
  )
}

