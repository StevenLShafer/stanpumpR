# immunoglobulin: see R/drugs_immunoglobulin.R (Fokkink 2022, Table 2).
fokkink <- function(w) {
  r <- w / 70
  list(cl = 0.28 * r^0.75 / 1440, v1 = 2.87 * r, q = 0.25 * r^0.75 / 1440, v2 = 2.65 * r)
}
noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("Table 2, the covariates at the reference, switch off", {
  for (w in c(18, 70, 122)) {
    pk <- immunoglobulin(w, 170, 50, "male", FALSE)$PK$default
    ref <- fokkink(w)
    expect_equal(c(pk$cl1, pk$v1, pk$cl2, pk$v2), c(ref$cl, ref$v1, ref$q, ref$v2))
  }
})

test_that("the pharmacokinetic weight with the switch on", {
  size <- pkSizeFactors(110, 165, 40, "female", TRUE)
  pk <- immunoglobulin(110, 165, 40, "female", TRUE)$PK$default
  ref <- fokkink(size$pkWeight)
  expect_equal(c(pk$cl1, pk$v1, pk$cl2, pk$v2), c(ref$cl, ref$v1, ref$q, ref$v2))
})

test_that("the half-lives in the header, and the GBS baseline", {
  pk <- immunoglobulin(70, 175, 50, "male", FALSE)$PK$default
  k10 <- pk$cl1 / pk$v1; k12 <- pk$cl2 / pk$v1; k21 <- pk$cl2 / pk$v2
  b <- k10 + k12 + k21
  l <- c(b + sqrt(b^2 - 4 * k10 * k21), b - sqrt(b^2 - 4 * k10 * k21)) / 2
  expect_equal(log(2) / l / 1440, c(2.879, 18.13), tolerance = 0.002)   # days
  expect_equal(IMMUNOGLOBULIN_BASELINE, 10.335)
})

test_that("0.4 g/kg: the patient's IgG plus the infused, in g/L", {
  PK <- getDrugPK("immunoglobulin", 70, 175, 50, "male", adjustToFFM = FALSE)
  expect_equal(PK$endogenousActivity, 10.335)
  w <- simCpCe(data.frame(Drug = "immunoglobulin", Time = 60, Dose = 0.4, Units = "g/kg"),
               noEvents, PK, 180, FALSE)$wide
  expect_equal(w$Plasma[w$Time < 60], rep(10.335, sum(w$Time < 60)))
  expect_equal(max(w$Plasma[w$Time == 60]), 10.335 + 28 / 2.87, tolerance = 1e-6)
  PK2 <- getDrugPK("immunoglobulin", 70, 175, 50, "male", adjustToFFM = FALSE,
                   factorBaselines = list(igGBaseline = 4))
  w2 <- simCpCe(data.frame(Drug = "immunoglobulin", Time = 60, Dose = 0.4, Units = "g/kg"),
                noEvents, PK2, 180, FALSE)$wide
  expect_equal(w2$Plasma - w$Plasma, rep(4 - 10.335, nrow(w)))
  expect_identical(patientFieldReaders("igGBaseline"), "immunoglobulin")
  expect_identical(plottedUnitText("gL", "immunoglobulin"), "g/L")
})
