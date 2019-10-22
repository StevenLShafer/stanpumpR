# Drugs.R
#Function to set drug information
getDrugPK <- function(
  drug = "propofol",
  weight = 70,
  height = 170,
  age = 50,
  sex = "Woman"
  )
{
  bsa = weight^0.425 * height ^ 0.725 * 0.007184
  if (sex == "woman")
    lbm = 1.07 * weight - 148 * (weight/height)^2
  else
    lbm = 1.10 * weight - 128 * (weight/height)^2

  reference <- "Need to check"
  tPeak <- 0
  awakeConcentration <- 0
  infusateConcentration <- 0
  MEAC <- 0
  typical <- 0
  upperTypical <- 0
  lowerTypical <- 0
  weightAdjust <- FALSE
  switch(
    drug,
    fentanyl = {
      reference <-  "JPET 1987240:159-166"
      v1  <- 12.7
      k10 <- 0.056
      k12 <- 0.373
      k13 <- 0.180
      k21 <- 0.096
      k31 <- 0.0077
      tPeak <- 3.694		# from Shafer/Varvel, t_peaks.xls
      awakeConcentration <- 1.0	# target concentration at end of case
      infusateConcentration <- 50
      MEAC <- 0.6
      typical <- MEAC * 1.2
      upperTypical <- MEAC * 0.8
      lowerTypical <- MEAC * 2.0

    },
    alfentanil = {
      reference <-  "JPET 1987240:159-166"
      v1  <- 2.1853
      k10 <- 0.091
      k12 <- 0.656
      k13 <- 0.113
      k21 <- 0.214
      k31 <- 0.017
      tPeak <- 1.4		# see t_peaks.xls
      awakeConcentration <- 100	# target concentration at end of case
      infusateConcentration <- 500	# undiluted drug
      MEAC <- 39
      typical <- MEAC * 1.2
      upperTypical <- MEAC * 0.8
      lowerTypical <- MEAC * 2.0
    },
    sufentanil = {
      reference <- "Anesthesiology 1995 83:1194-1204"
      v1  <- 14.3
      k10 <- 0.0645
      k12 <- 0.1086
      k13 <- 0.0229
      k21 <- 0.0245
      k31 <- 0.0013
      tPeak <- 5.8		# from Shafer/Varvel, t_peaks.xls
      awakeConcentration <- .25	# Desired Cp on emergence
      MEAC <- 0.056
      typical <- MEAC * 1.2
      upperTypical <- MEAC * 0.8
      lowerTypical <- MEAC * 2.0
    },
    dexmedetomidine = {
      reference <- "Barry Dyck, Check reference and numbers"
      v1  <- 8.0574
      k10 <- 0.0552
      k12 <- 0.258
      k13 <- 0.247
      k21 <- 0.163
      k31 <- 0.0112
      awakeConcentration <- 5.0
    },
    midazolam = {
      v1  <- 3.3
      k10 <- 0.162181
      k12 <- 0.610464
      k13 <- 0.252155
      k21 <- 0.114700
      k31 <- 0.008600
      k41 <- .693 / 4.0	# From Hung, Buhrer, & Stanski, unpublished, approx.
      awakeConcentration <- 15	# Desired Cp on emergence
      infusateConcentration <- 1000
    },
    propofol = {
      # Schnider for now, will switch to Eleveld
      reference <- "Anesthesiology 1998"
      infusate_concentration <- 10
      v1 <- 4.27
      v2 <- 18.9-0.391*(age-53)
      v3 <- 238
      cl1 <- 1.89+0.0456*(weight-77)-0.0681*(lbm-59)+0.0264*(height-177)
      cl2 <- 1.29-0.024*(age-53)
      cl3 <- 0.836
      k10 <- cl1 / v1
      k12 <- cl2 / v1
      k13 <- cl3 / v1
      k21 <- cl2 / v2
      k31 <- cl3 / v3
      awakeConcentration <- 0.5	# Desired Cp on emergence
      tPeak <- 1.600 # Anesthesiology 90:1502-1516, 1999
      weightAdjust <- TRUE
      typical <- 3
      upperTypical <- 4.0
      lowerTypical <- 2.5
    },
    lidocaine = {
      reference <- "Schnider?"
      v1 <- 0.088 * weight
      k10 <- 0.227273
      k12 <- 0.636364
      k13 <- 0.0
      k21 <- 0.14
      k31 <- 0.0
      awake_concentration <- 1.0	# Desired Cp on emergence
      weightAdjust <- TRUE
    },
    ketamine = {
      reference <- "Clin Pharmacol Ther 199136:645-653"
      v1  <- .063 * weight
      k10 <- 0.438
      k12 <- 0.592
      k13 <- 0.590
      k21 <- 0.247
      k31 <- 0.0146
      awake_concentration <- .25	# Desired Cp on emergence
      infusate_concentration <- 10
      weightAdjust <- TRUE
    },
    etomidate = {
      reference <- "Anesthesiology 198665:19-27"
      v1  <- .090 * weight
      k10 <- 0.205
      k12 <- 0.284
      k13 <- 0.209
      k21 <- 0.131
      k31 <- 0.00479
      k41 <- .693/1.6
      infusate_concentration <- 2
      awake_concentration <- .2
      weightAdjust <- TRUE
    },
    methadone = {
      reference <- "Inturissi?"
      v1  <- 0.18 * weight
      k10 <- 0.00806
      k12 <- 0.408
      k13 <- 0.112
      k21 <- 0.06
      k31 <- 0.00114
      tPeak <- 11.3
      awake_concentration <- 1.0	# Desired Cp on emergence
      weightAdjust <- TRUE
      MEAC <- 60
      typical <- MEAC * 1.2
      upperTypical <- MEAC * 0.8
      lowerTypical <- MEAC * 2.0
    },
    morphine = {
      reference <- "Lotsch PK"
      v1  <- 0.25 * weight
      k10 <- 0.070505618
      k12 <- 0.127340824
      k13 <- 0.018258427
      k21 <- 0.025964108
      k31 <- 0.001633166
      tPeak <- 93.8
      awake_concentration <- 1.0	# Desired Cp on emergence
      weightAdjust <- TRUE
      MEAC <- 8
      typical <- MEAC * 1.2
      upperTypical <- MEAC * 0.8
      lowerTypical <- MEAC * 2.0
    },
    hydromorphone = {
      reference <- "Drover PK"
      v1  <- 0.16 * weight
      k10 <- 0.116
      k12 <- 0.3
      k13 <- 0.08
      k21 <- 0.03
      k31 <- 0.00095
      tPeak <- 19.6
      awake_concentration <- 1.0	# Desired Cp on emergence
      weightAdjust <- TRUE
      MEAC <- 1.5
      typical <- MEAC * 1.2
      upperTypical <- MEAC * 0.8
      lowerTypical <- MEAC * 2.0
    },
    rocuronium={
      v1  <-  0.056 * weight
      k10 <- 0.1746
      k12 <- 0.100381
      k13 <- 0
      k21 <- 0.0245
      k31 <- 0
      k41 <- 0.168
      weightAdjust <- TRUE
    },
    lorazepam = {
      reference <- "Zomorodi, 1997"
      v1  <- 41
      k10 <- 0.00276
      k12 <- 0.04366
      k13 <- 0.0
      k21 <- 0.01738
      k31 <- 0.0
      k41 <- .693 / 1.2
      awake_concentration <- 120	# Desired Cp on emergence
      infusate_concentration <- 2000
    },
    remifentanil = {
      reference <- "Minto/Schnider"
      v1 <- 5.1-0.0201*(age-40)+0.072*(lbm-55)
      v2 <- 9.82-0.0811*(age-40)+0.108*(lbm-55)
      v3 <- 5.42
      cl1 <- 2.6-0.0162*(age-40)+0.0191*(lbm-55)
      cl2 <- 2.05-0.0301*(age-40)
      cl3 <- 0.076-0.00113*(age-40)
      k10 <- cl1 / v1
      k12 <- cl2 / v1
      k13 <- cl3 / v1
      k21 <- cl2 / v2
      k31 <- cl3 / v3
      k41 <- 0.595-0.007*(age-40)
      k41 <- .693 / 1.2
      infusate_concentration <- 50
      awake_concentration <- 1	# Desired Cp on emergence
      weightAdjust <- TRUE
      tPeak = 1.6 # Opioid simulation spreadsheet
      MEAC <- 1
      typical <- MEAC * 1.2
      upperTypical <- MEAC * 0.8
      lowerTypical <- MEAC * 2.0
    }
  )

  X <- cube(k10, k12, k13, k21, k31)
  lambda_1 <- X[1]
  lambda_2 <- X[2]
  lambda_3 <- X[3]

  p_coef_bolus_1 <- 0
  p_coef_bolus_2 <- 0
  p_coef_bolus_3 <- 0
  p_coef_infusion_1 <- 0
  p_coef_infusion_2 <- 0
  p_coef_infusion_3 <- 0

  e_coef_bolus_1 <- 0
  e_coef_bolus_2 <- 0
  e_coef_bolus_3 <- 0
  e_coef_bolus_4 <- 0
  e_coef_infusion_1 <- 0
  e_coef_infusion_2 <- 0
  e_coef_infusion_3 <- 0
  e_coef_infusion_4 <- 0

  if (k31 > 0)
  {
    p_coef_bolus_1 <- (k21 - lambda_1) * (k31 - lambda_1) /
      (lambda_1 - lambda_2) /
      (lambda_1 - lambda_3) / v1
    p_coef_bolus_2 <- (k21 - lambda_2) * (k31 - lambda_2) /
      (lambda_2 - lambda_1) /
      (lambda_2 - lambda_3) /
      v1
    p_coef_bolus_3 <- (k21 - lambda_3) * (k31 - lambda_3) /
      (lambda_3 - lambda_2) /
      (lambda_3 - lambda_1) /
      v1
  }
  else
  {
    if (lambda_2 > 0)
    {
      p_coef_bolus_1 <- (k21 - lambda_1) / (lambda_2 - lambda_1) / v1
      p_coef_bolus_2 <- (k21 - lambda_2) / (lambda_1 - lambda_2) / v1
    }
    else
    {
      p_coef_bolus_1 <- 1 / lambda_1 / v1
    }
  }

  p_coef_infusion_1 <- p_coef_bolus_1 / lambda_1
  if (lambda_2 > 0) p_coef_infusion_2 <- p_coef_bolus_2 / lambda_2
  if (lambda_3 > 0) p_coef_infusion_3 <- p_coef_bolus_3 / lambda_3

  # find ke0 from tPeak
  if (tPeak > 0)
    lambda_4 <- optimize(tPeakError, c(0,200),tPeak,
                         p_coef_bolus_1,
                         p_coef_bolus_2,
                         p_coef_bolus_3,
                         lambda_1,
                         lambda_2,
                         lambda_3
                         )$minimum
  else
    lambda_4 <- 0

  if (lambda_4 > 0)
  {
    e_coef_bolus_1 <- p_coef_bolus_1 / (lambda_4 - lambda_1) * lambda_4
    e_coef_infusion_1 <- e_coef_bolus_1 / lambda_1

    if (lambda_2 > 0)
    {
      e_coef_bolus_2 <-  p_coef_bolus_2 / (lambda_4 - lambda_2) * lambda_4
      e_coef_infusion_2 <- e_coef_bolus_2 / lambda_2
    }
    if (lambda_3 > 0)
    {
      e_coef_bolus_3 <- p_coef_bolus_3 / (lambda_4 - lambda_3) * lambda_4
      e_coef_infusion_3 <- if (lambda_3 > 0) e_coef_bolus_3 / lambda_3
    }
    e_coef_bolus_4 <- - e_coef_bolus_1 - e_coef_bolus_2 - e_coef_bolus_3
    e_coef_infusion_4 <- e_coef_bolus_4 / lambda_4
  }

  return(
    list(
      drug = drug,
      lambda_1 = lambda_1,
      lambda_2 = lambda_2,
      lambda_3 = lambda_3,
      lambda_4 = lambda_4,

      p_coef_bolus_1 = p_coef_bolus_1,
      p_coef_bolus_2 = p_coef_bolus_2,
      p_coef_bolus_3 = p_coef_bolus_3,

      e_coef_bolus_1 = e_coef_bolus_1,
      e_coef_bolus_2 = e_coef_bolus_2,
      e_coef_bolus_3 = e_coef_bolus_3,
      e_coef_bolus_4 = e_coef_bolus_4,

      p_coef_infusion_1 = p_coef_infusion_1,
      p_coef_infusion_2 = p_coef_infusion_2,
      p_coef_infusion_3 = p_coef_infusion_3,

      e_coef_infusion_1 = e_coef_infusion_1,
      e_coef_infusion_2 = e_coef_infusion_2,
      e_coef_infusion_3 = e_coef_infusion_3,
      e_coef_infusion_4 = e_coef_infusion_4,
      reference = reference,
      tPeak = tPeak,
      awakeConcentration = awakeConcentration,
      infusateConcentration = infusateConcentration,
      MEAC = MEAC,
      weightAdjust = weightAdjust,
      weight = weight,
      height = height,
      age = age,
      sex = sex,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      Concentration.Units = drugDefaults$Concentration.Units[drugDefaults$Drug == drug],
      Bolus.Units         = drugDefaults$Bolus.Units[drugDefaults$Drug == drug],
      Infusion.Units      = drugDefaults$Infusion.Units[drugDefaults$Drug == drug]
    )
  )
}

cube <- function(k10, k12, k13, k21, k31)
{
  toradian <- asin(1.0) * 2.0 / 180.0	# pi/180
  if (k31 > 0)
  {
    # first take roots of X^3 + a2X^2 + a1X^1 + a0 <- 0
    # where the coefficients are :
    a0 <- k10 * k21 * k31
    a1 <- k10 * k31 + k21 * k31 + k21 * k13 + k10 * k21 + k31 * k12
    a2 <- k10 + k12 + k13 + k21 + k31

    # now transform to x^3 + px + q <- 0
    p <- a1 - (a2 * a2 / 3.0)
    q <- (2 * a2 * a2 * a2 / 27.0) - (a1 * a2 / 3.0) + a0
    r1 <- sqrt(-(p * p * p) / 27.0)
    phi <- (-q / 2.0) / r1
    if (phi > 1)
      phi <- 1
    else if (phi < -1)
      phi <- -1
    phi <- (acos(phi) / 3.0)
    r1 <- 2.0 * exp(log(r1) / 3.0)
    root1 <- -(cos(phi) * r1 - a2 / 3.0)
    root2 <- -(cos(phi + 120.0 * toradian) * r1 - a2 / 3.0)
    root3 <- -(cos(phi + 240.0 * toradian) * r1 - a2 / 3.0)
  } else {
    if (k21 > 0)
      {
        # first take roots of X^2 - a1X^1 + a0 = 0
        # where the coefficients are :
        a0 <- k10 * k21
        a1 <- -(k10 + k12 + k21)
        root1 <- (-a1 + sqrt(a1 * a1 - 4 * a0)) / 2
        root2 <- (-a1 - sqrt(a1 * a1 - 4 * a0)) / 2
        root3 <- 0
      } else {
        # one compartment model
        root1 <- k10
        root2 <- 0
        root3 <- 0
      }
  }

  # sort - nothing fancy is needed
  roots <- sort(c(root1, root2, root3),decreasing=TRUE)
  return(roots)
}

CE <- function(t, e_coef_bolus_1, e_coef_bolus_2, e_coef_bolus_3, e_coef_bolus_4, lambda_1, lambda_2, lambda_3, lambda_4)
{
  e_coef_bolus_1 * exp(-lambda_1 * t) +
  e_coef_bolus_2 * exp(-lambda_2 * t) +
  e_coef_bolus_3 * exp(-lambda_3 * t) +
  e_coef_bolus_4 * exp(-lambda_4 * t)
}

tPeakError <-   function(lambda_4, tPeak, p_coef_bolus_1,p_coef_bolus_2,p_coef_bolus_3, lambda_1, lambda_2, lambda_3)
{
  e_coef_bolus_1 <- p_coef_bolus_1 / (lambda_4 - lambda_1) * lambda_4

  if (lambda_2 > 0)
  {
    e_coef_bolus_2 <-  p_coef_bolus_2 / (lambda_4 - lambda_2) * lambda_4
  }
  if (lambda_3 > 0)
  {
    e_coef_bolus_3 <- p_coef_bolus_3 / (lambda_4 - lambda_3) * lambda_4
  }
  e_coef_bolus_4 <- - e_coef_bolus_1 - e_coef_bolus_2 - e_coef_bolus_3

  predPeak <- optimize(CE,c(0,100), e_coef_bolus_1, e_coef_bolus_2, e_coef_bolus_3, e_coef_bolus_4, lambda_1, lambda_2, lambda_3, lambda_4, maximum=TRUE)$maximum
  return((tPeak-predPeak)^2)
}

