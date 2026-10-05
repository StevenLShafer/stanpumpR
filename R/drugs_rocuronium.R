rocuronium <- function(weight, height, age, sex)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters

  v1  <-  0.056 * weight
  k10 <- 0.1746
  k12 <- 0.100381
  k13 <- 0
  k21 <- 0.0245
  k31 <- 0
  k41 <- 0.168

  v2 <- v1 * k12 / k21
  v3 <- 1
  cl1 <- v1 * k10
  cl2 <- v1 * k12
  cl3 <- 0

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

  tPeak = 2.2 # tPeak per Cortinez LI et al., Br J Anaesth 2007;99(5):679-685. https://pubmed.ncbi.nlm.nih.gov/17681967/
  typical <- 1.5
  upperTypical <- 2.2
  lowerTypical <- 1
  MEAC <- 0
  reference <- "Plaud B et al., Clin Pharmacol Ther 1995;58(2):185-191. https://pubmed.ncbi.nlm.nih.gov/7648768/"


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
