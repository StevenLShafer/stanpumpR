remifentanil <- function(weight, height, age, sex)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters

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
  if (sex == "male")
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

  } else {

  # Kim Model
  BMI <-  weight / (height / 100)^2
  if (sex == "male")
  {
    FFM <- 9.27 * weight / (6.68  + 0.216 * weight)
  } else {
    FFM <- 9.27 * weight / (8.78 + 0.244 * weight)
  }

  v1 <- 4.76 * (weight / 74.5)^0.658
  v2 <- 8.4 * (FFM / 52.3)^0.573 - 0.0936*(age - 37)
  v3 <- 4 - 0.0477 * (age - 37)
  cl1 <- 2.77 * (weight / 74.5)^0.336 - 0.0149 * (age - 37)
  cl2 <- 1.94 - 0.028 * (age - 37)
  cl3 <- 0.197

  }

default = list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3
  )

  events <- c("default")
  PK <- sapply(events, function(x) list(get0(x)))

  infusate_concentration <- 50
  awake_concentration <- 1	# Desired Cp on emergence
  weightAdjust <- TRUE
  tPeak = 1.6 # Opioid simulation spreadsheet
  MEAC <- 1
  typical <- MEAC * 1.2
  upperTypical <- MEAC * 0.8
  lowerTypical <- MEAC * 2.0
  reference <- "Minto/Schnider"

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
