# levodopaEntacaponeIntestinalGel: see R/drugs_levodopaEntacaponeIntestinalGel.R
# (Senek 2020, Table 2).

test_that("Table 2 at 70 kg, switch off: the LCIG clearance 36.5% lower", {
  m <- levodopaEntacaponeIntestinalGel(70, 170, 70, "male", FALSE)
  p <- m$PK$default
  expect_equal(p$cl1 * 60, 27.9 * (1 - 0.365))
  expect_equal_rounded(17.7, p$cl1 * 60, tolerance = 0.05)
  expect_equal(p$v1, 74.5)
  expect_equal(p$ka_PO * 60, 50)
  expect_equal(p$bioavailability_PO, 1)
  expect_true(m$enteralInfusion)
  expect_equal(m$ke0, levodopa(70, 170, 70, "male", FALSE)$ke0)
})

test_that("allometric weight, and the pharmacokinetic weight with the switch on", {
  p <- levodopaEntacaponeIntestinalGel(51, 170, 70, "male", FALSE)$PK$default
  expect_equal(p$cl1 * 60, 27.9 * 0.635 * (51 / 70)^0.75)
  expect_equal(p$v1, 74.5 * 51 / 70)
  w <- pkSizeFactors(99, 170, 70, "male", TRUE)$pkWeight
  on <- levodopaEntacaponeIntestinalGel(99, 170, 70, "male", TRUE)$PK$default
  expect_equal(on$v1, 74.5 * w / 70)
})

test_that("a continuous rate reaches rate / CL, and 35% less matches LCIG", {
  res <- pdSimulate(data.frame(Drug = "levodopaEntacaponeIntestinalGel", Time = 0, Dose = 60,
                               Units = "mg/hr PO"), maximum = 24 * 60)
  p <- pdSite(res)
  expect_equal(p$Y[nrow(p)], 60 / (27.9 * 0.635), tolerance = 0.005)
  # the authors' conclusion: 35% lower rate for Senek's own LCIG steady state
  res <- pdSimulate(data.frame(Drug = "levodopaEntacaponeIntestinalGel", Time = 0, Dose = 39,
                               Units = "mg/hr PO"), maximum = 24 * 60)
  p <- pdSite(res)
  expect_equal(p$Y[nrow(p)], 60 / 27.9, tolerance = 0.03)
})

test_that("a dose is absorbed within minutes", {
  res <- pdSimulate(data.frame(Drug = "levodopaEntacaponeIntestinalGel", Time = 0, Dose = 200,
                               Units = "mg PO"), maximum = 240)
  expect_lt(pdPeak(res)[["t"]] * 60, 10)
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "levodopaEntacaponeIntestinalGel", ]
  expect_equal(row$endCe, 0.81)
  expect_true("mg/hr PO" %in% row$Units[[1]])
})

test_that("the help page shows the gel's absorption as oral, not as a tissue route", {
  h <- as.character(helpDrugPageHTML("levodopaEntacaponeIntestinalGel"))
  expect_match(h, "<td>Oral (PO)</td>", fixed = TRUE)
  expect_no_match(h, "Regional anesthesia (RA)", fixed = TRUE)
})
