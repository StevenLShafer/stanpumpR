remifentanil <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters

  # adjustToFFM is accepted for a uniform signature but not used: both the
  # Eleveld and the Kim models below carry their own fat-free-mass covariate.

  # Schnider

  # lbm <- lbmJames(weight, height, sex)
  # v1 <- 5.1-0.0201*(age-40)+0.072*(lbm-55)
  # v2 <- 9.82-0.0811*(age-40)+0.108*(lbm-55)
  # v3 <- 5.42
  # cl1 <- 2.6-0.0162*(age-40)+0.0191*(lbm-55)
  # cl2 <- 2.05-0.0301*(age-40)
  # cl3 <- 0.076-0.00113*(age-40)

  BMI <-  weight / (height / 100)^2

  if (BMI < 30) # NIH Obesity cutoff
  {
  # Eleveld
  if (sex == SEX_MALE)
  {
    M1F2 <- 1
  } else {
    M1F2 <- 2
  }

  # THETA
  THETA01 <-  1.759110     # v1=5.81
  THETA02 <-  2.177170     # v2=8.82
  THETA03 <-  1.614450     # v3=5.03
  THETA04 <-  0.946257     # cl=2.58
  THETA05 <-  0.540315     # q2=1.72
  THETA06 <- -2.083880     # q3=0.12
  THETA07 <-  2.878980     # e50 for cl maturation
  THETA08 <- -0.00554481   # aging v1/q2/q3
  THETA09 <- -0.00326985   # aging v2/cl
  THETA10 <- -0.0315135    # aging v3
  THETA11 <-  0.4704050    # increase cl/q2/v2 in females 12-45 years
  THETA12 <- -0.0260496    # weight correction v3
  THETA13 <-  0.111308     # residual error Minto data
  THETA14 <-  0.271478     # residual error Ross data
  THETA15 <-  0.240246     # residual error Mertens data


  # maturation
  SE50=THETA07
  ADLT=(weight^2)/((weight^2)+(SE50^2))
  AREF=(70.^2)/((70.^2)+(SE50^2))
  KMAT=ADLT/AREF

  # scaling using Al-sallami FFM
  HT2=(height/100.)*(height/100.)
  MATM=0.88+((1-0.88)/(1+(age/13.4)^(-12.7)))
  MATF=1.11+((1-1.11)/(1+(age/7.1)^(-1.1)))
  FFMM=MATM*42.92*(HT2)*weight/(30.93*(HT2)+weight)
  FFMF=MATF*37.99*(HT2)*weight/(35.98*(HT2)+weight)
  FFMF=MATF*37.99*(HT2)*weight/(35.98*(HT2)+weight)
  FFMR=42.92*(1.7*1.7)*70./(30.93*(1.7*1.7)+70.)

  MAL=2-M1F2
  FEM=M1F2-1
  bsize=(MAL*FFMM + FEM*FFMF)/FFMR

  # aging for v1/q2/q2, v3 and v2/cl
  kv1=exp(THETA08*(age-35.))
  kv2=exp(THETA09*(age-35.))
  kv3=exp(THETA10*(age-35.))
  kcl=kv2
  kcl2=kv1
  kcl3=kv1

  # sex correction for cl, v2 and q2
  PPUB=(age^6)/(age^6 + 12^6)
  ELDY=(age^6)/(age^6 + 45^6)
  ksex=1+(M1F2-1)*PPUB*(1-ELDY)*THETA11

  # weight correction for v3
  Wv3=exp(THETA12*(weight-70.))

  # compartmental allometric scaling
  M1 =(bsize)^1 * kv1
  M2 =(bsize)^1 * kv2 * ksex
  M3 =(bsize)^1 * kv3 * Wv3
  v1 =exp(THETA01) * M1
  v2 =exp(THETA02) * M2
  v3 =exp(THETA03) * M3
  rv2=exp(THETA02)
  rv3=exp(THETA03)
  M4 =(bsize)^0.75 * kcl * ksex * KMAT
  M5 =(v2/rv2)^0.75 * kcl2 * ksex
  M6 =(v3/rv3)^0.75 * kcl3
  cl1 =exp(THETA04) * M4
  cl2 =exp(THETA05) * M5
  cl3 =exp(THETA06) * M6

  reference <- "Eleveld DJ et al., Anesthesiology 2017;126(6):1005-1018. https://pubmed.ncbi.nlm.nih.gov/28509794/"

  } else {

  # Kim Model
  # Kim TK, Obara S, Egan TD, et al. Disposition of remifentanil in obesity: a
  # new pharmacokinetic model incorporating the influence of body mass.
  # Anesthesiology 2017;126:1019-1032.  Fat-free mass by Janmahasatian et al.
  # (Clin Pharmacokinet 2005;44:1051-1065):
  #   men:   FFM = 9270 * WT / (6680 + 216 * BMI)
  #   women: FFM = 9270 * WT / (8780 + 244 * BMI)
  # written here with numerator and denominator divided by 1000.  The
  # denominator takes BMI; an earlier version of this file used weight there,
  # which roughly halved FFM, and so V2, for an obese adult.
  BMI <-  weight / (height / 100)^2
  if (sex == SEX_MALE)
  {
    FFM <- 9.27 * weight / (6.68 + 0.216 * BMI)
  } else {
    FFM <- 9.27 * weight / (8.78 + 0.244 * BMI)
  }

  v1 <- 4.76 * (weight / 74.5)^0.658
  v2 <- 8.4 * (FFM / 52.3)^0.573 - 0.0936*(age - 37)
  v3 <- 4 - 0.0477 * (age - 37)
  cl1 <- 2.77 * (weight / 74.5)^0.336 - 0.0149 * (age - 37)
  cl2 <- 1.94 - 0.028 * (age - 37)
  cl3 <- 0.197

  reference <- "Kim TK et al., Anesthesiology 2017;126(6):1019-1032. https://pubmed.ncbi.nlm.nih.gov/28509796/"

  }

default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  infusate_concentration <- 50
  awake_concentration <- 1	# Desired Cp on emergence
  weightAdjust <- TRUE
  tPeak = 1.6 # Opioid simulation spreadsheet
  MEAC <- 1
  typical <- MEAC * 1.2
  upperTypical <- MEAC * 0.8
  lowerTypical <- MEAC * 2.0
  # The citation is set in the branch above, so that the References panel
  # names the model actually computed: Eleveld 2017 below BMI 30, Kim 2017 at
  # and above it.  Minto 1997 (Anesthesiology 86:10-23, PMID 9009935), the
  # model STANPUMP used, remains in the comments at the top of this file.

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
