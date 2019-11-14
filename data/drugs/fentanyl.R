fentanyl <- function(weight, height, age, sex)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters
  # Data from
  # 1 = McClain and Hug, Clin Pharmacol Ther. 1980;28:106-14.
  # 2 = Scott and Stanski, Anesthesiology. 1990;73:1091-102.
  # 3 = Hudson, Anesthesiology. 1986;64:334-8.
  # 4 = Varvel, Anesthesiology. 1989;70:928-34.
  # 5 = Shafer, Anesthesiology. 1990;73:1091-102.

  default <- list(
  v1  = 12.1  * weight/70,
  v2  = 35.7  * weight/70,
  v3  = 224   * weight/70,
  cl1 = 0.632 * (weight/70) ^ 0.75,
  cl2 = 2.8   * (weight/70) ^ 0.75,
  cl3 = 1.55  * (weight/70) ^ 0.75
  )

  events <- c("default")
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 3.694		# from Shafer/Varvel, t_peaks.xls
  MEAC <- 0.6
  typical <- MEAC * 1.2
  upperTypical <- MEAC * 0.8
  lowerTypical <- MEAC * 2.0
  reference <-  "JPET 1987,240:159-166"

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
