# anakinra: see R/drugs_anakinra.R (Urien 2013, Table 1 and the Results).
urien <- function(bw) list(cl = 0.847 * bw^0.47 / 60, v = 2.581 * bw^0.76)

test_that("the Results' equations, switch off", {
  for (w in c(4.3, 21, 70, 120)) {
    pk <- anakinra(w, 150, 10, "male", FALSE)$PK$default
    ref <- urien(w)
    expect_equal(c(pk$cl1, pk$v1), c(ref$cl, ref$v))
    expect_equal(pk$ka_SC, 0.38 / 60)
    expect_equal(pk$bioavailability_SC, 1)
  }
})

test_that("Table 1's values at 70 kg and the derived checks", {
  pk <- anakinra(70, 175, 35, "male", FALSE)$PK$default
  expect_equal(pk$cl1 * 60, 6.24, tolerance = 0.002)
  expect_equal(pk$v1, 65.2, tolerance = 0.002)
  k <- pk$cl1 / pk$v1; ka <- pk$ka_SC
  expect_equal(log(2) / k / 60, 7.24, tolerance = 0.002)          # half-life, h
  expect_equal(log(ka / k) / (ka - k) / 60, 4.85, tolerance = 0.002) # tmax after one dose, h
  # Figure 4's dose for a mean Css of 0.4 mg/L: 2.4 mg/kg/day at 10 kg, 1 at 50 kg
  dose <- function(w) urien(w)$cl * 60 * 0.4 * 24 / w
  expect_equal(dose(10), 2.4, tolerance = 0.02)
  expect_equal(dose(50), 1.0, tolerance = 0.03)
})

test_that("the pharmacokinetic weight with the switch on", {
  size <- pkSizeFactors(110, 170, 40, "female", TRUE)
  pk <- anakinra(110, 170, 40, "female", TRUE)$PK$default
  expect_equal(pk$v1, urien(size$pkWeight)$v)
  expect_equal(pk$cl1, urien(size$pkWeight)$cl)
})

test_that("3 mg/kg SC daily at 20 kg: the steady state against NOMID's", {
  PK <- getDrugPK("anakinra", 20, 115, 6, "male", adjustToFFM = FALSE)
  w <- simCpCe(data.frame(Drug = "anakinra", Time = 0, Dose = 3,
                          Units = "mg/kg SC qd"),
               data.frame(Time = numeric(0), Event = character(0)), PK,
               14 * 1440, FALSE)$wide
  day <- w$Time >= 12 * 1440 & w$Time <= 13 * 1440
  peak <- max(w$Plasma[day]); trough <- min(w$Plasma[day])
  # NOMID (label 12.3): median C24h 203 (53-1979), Cmax 3628 (655-8511) ng/mL
  expect_equal(trough, 142, tolerance = 0.03)
  expect_equal(peak, 1421, tolerance = 0.03)
  expect_true(trough > 53 && trough < 1979 && peak > 655 && peak < 8511)
})
