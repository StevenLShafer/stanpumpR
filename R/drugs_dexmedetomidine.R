dexmedetomidine <- function(weight, height, age, sex)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters

  if (age > 1)
  {
    v1  <- 8.0574
    k10 <- 0.0552
    k12 <- 0.258
    k13 <- 0.247
    k21 <- 0.163
    k31 <- 0.0112

    tPeak <- 10 # Just a guess
    typical <- 10 #Clin Pharmacol Ther. 1995 Jul;58(1):35-43.
    upperTypical <- 0.4
    lowerTypical <- 0.8
    MEAC <- 0
    reference <- "Barry Dyck, Check reference and numbers"

    v2 <- v1 * k12 / k21
    v3 <- v1 * k13 / k31
    cl1 <- v1 * k10
    cl2 <- v1 * k12
    cl3 <- v1 * k13

    default <- list(
      v1 = v1,
      v2 = v2,
      v3 = v3,
      cl1 = cl1,
      cl2 = cl2,
      cl3 = cl3
    )
    events <- c("default")
  } else {
    v3 <- 1
    cl3 <- 0
    # Before cardiopulmonary bypass:
    v1 <- 132   * (weight / 70) # liters
    v2 <- 78.9  * (weight / 70) # liters
    cl1 <- 1240 * (weight / 70) ^ 0.75 / 1000 # (L / min)
    cl2 <- 2300 * (weight / 70) ^ 0.75 / 1000 # (L / min)
    default <- list(
      v1 = v1,
      v2 = v2,
      v3 = v3,
      cl1 = cl1,
      cl2 = cl2,
      cl3 = cl3
    )

    # During cardiopulmonary bypass ************************************************
    # 37 degrees
    v1 <- 115 * (weight / 70) * (37 / 37) ^ (-1.6)  # liters
    v2 <- 144 * (weight / 70) # liters
    cl1 <- 74.1 * (weight / 70) ^ 0.75 / 1000 # (L / min)
    cl2 <- 2980 * (weight / 70) ^ 0.75 / 1000 # (L / min)
    CPBStart <- list(
      v1 = v1,
      v2 = v2,
      v3 = v3,
      cl1 = cl1,
      cl2 = cl2,
      cl3 = cl3
    )

    # 36 degrees
    v1 <- 115 * (weight / 70) * (36 / 37) ^ (-1.6)  # liters
    v2 <- 144 * (weight / 70) # liters
    cl1 <- 74.1 * (weight / 70) ^ 0.75 / 1000 # (L / min)
    cl2 <- 2980 * (weight / 70) ^ 0.75 / 1000 # (L / min)
    CPB36 <- list(
      v1 = v1,
      v2 = v2,
      v3 = v3,
      cl1 = cl1,
      cl2 = cl2,
      cl3 = cl3
    )

    # 35 degrees
    v1 <- 115 * (weight / 70) * (35 / 37) ^ (-1.6)  # liters
    v2 <- 144 * (weight / 70) # liters
    cl1 <- 74.1 * (weight / 70) ^ 0.75 / 1000 # (L / min)
    cl2 <- 2980 * (weight / 70) ^ 0.75 / 1000 # (L / min)
    CPB35 <- list(
      v1 = v1,
      v2 = v2,
      v3 = v3,
      cl1 = cl1,
      cl2 = cl2,
      cl3 = cl3
    )

    # 34 degrees
    v1 <- 115 * (weight / 70) * (34 / 37) ^ (-1.6)  # liters
    v2 <- 144 * (weight / 70) # liters
    cl1 <- 74.1 * (weight / 70) ^ 0.75 / 1000 # (L / min)
    cl2 <- 2980 * (weight / 70) ^ 0.75 / 1000 # (L / min)
    CPB34 <- list(
      v1 = v1,
      v2 = v2,
      v3 = v3,
      cl1 = cl1,
      cl2 = cl2,
      cl3 = cl3
    )

    # 33 degrees
    v1 <- 115 * (weight / 70) * (33 / 37) ^ (-1.6)  # liters
    v2 <- 144 * (weight / 70) # liters
    cl1 <- 74.1 * (weight / 70) ^ 0.75 / 1000 # (L / min)
    cl2 <- 2980 * (weight / 70) ^ 0.75 / 1000 # (L / min)
    CPB33 <- list(
      v1 = v1,
      v2 = v2,
      v3 = v3,
      cl1 = cl1,
      cl2 = cl2,
      cl3 = cl3
    )
    # 32 degrees
    v1 <- 115 * (weight / 70) * (32 / 37) ^ (-1.6)  # liters
    v2 <- 144 * (weight / 70) # liters
    cl1 <- 74.1 * (weight / 70) ^ 0.75 / 1000 # (L / min)
    cl2 <- 2980 * (weight / 70) ^ 0.75 / 1000 # (L / min)
    CPB32 <- list(
      v1 = v1,
      v2 = v2,
      v3 = v3,
      cl1 = cl1,
      cl2 = cl2,
      cl3 = cl3
    )

    # 31 degrees
    v1 <- 115 * (weight / 70) * (31 / 37) ^ (-1.6)  # liters
    v2 <- 144 * (weight / 70) # liters
    cl1 <- 74.1 * (weight / 70) ^ 0.75 / 1000 # (L / min)
    cl2 <- 2980 * (weight / 70) ^ 0.75 / 1000 # (L / min)
    CPB31 <- list(
      v1 = v1,
      v2 = v2,
      v3 = v3,
      cl1 = cl1,
      cl2 = cl2,
      cl3 = cl3
    )

    # After cardiopulmonary bypass:
    v1 <-  155 * (weight / 70) # liters
    v2 <-  105 * (weight / 70) # liters
    cl1 <- 623 * (weight / 70) ^ 0.75 * (age * 365) / (1.77 + age * 365) / 1000 # (L / min)
    cl2 <- 209 * (weight / 70) ^ 0.75 / 1000 # (L / min)
    CPBEnd <- list(
      v1 = v1,
      v2 = v2,
      v3 = v3,
      cl1 = cl1,
      cl2 = cl2,
      cl3 = cl3
    )
    events <- c("default", "CPBStart","CPB36", "CPB35", "CPB34", "CPB33", "CPB32", "CPB31", "CPBEnd")

    tPeak <- 2 # Just a guess
    typical <- 10 #Clin Pharmacol Ther. 1995 Jul;58(1):35-43.
    upperTypical <- 0.4
    lowerTypical <- 0.8
    MEAC <- 0
    reference <- "Zuppa BJA 2019"
  }

  PK <- sapply(events, function(x) list(get0(x)))

  # cat("Here is the structure of PK")
  # print(utils::str(PK))

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
