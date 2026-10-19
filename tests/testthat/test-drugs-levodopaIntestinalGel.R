# levodopaIntestinalGel: see R/drugs_levodopaIntestinalGel.R (Othman & Dutta
# 2014; the transit chain and the enteral rate are tested in test-transit.R).

test_that("the gel's transit rate and relative bioavailability, the shared disposition", {
  g <- levodopaIntestinalGel(70, 170, 64, "male", FALSE)
  t <- levodopa(70, 170, 64, "male", FALSE)
  expect_equal(g$PK$default$ka_PO * 60, 9.2)
  expect_equal(g$PK$default$bioavailability_PO, 0.97)
  expect_true(g$enteralInfusion)
  for (f in c("v1", "v2", "cl1", "cl2")) expect_equal(g$PK$default[[f]], t$PK$default[[f]], info = f)
  expect_equal(g$ke0, t$ke0)
})

test_that("a continuous rate approaches rate x F / CL", {
  res <- pdSimulate(data.frame(Drug = "levodopaIntestinalGel", Time = 0, Dose = 60,
                               Units = "mg/hr PO"), maximum = 72 * 60, age = 64)
  p <- pdSite(res)
  expect_equal(p$Y[nrow(p)], 60 * 0.97 / 24.8, tolerance = 0.01)
  # after a 200 mg morning dose over 15 minutes and 8 hours of 60 mg/h, still
  # short of it (the slow peripheral phase)
  dt <- data.frame(Drug = "levodopaIntestinalGel", Time = c(0, 15), Dose = c(800, 60),
                   Units = "mg/hr PO")
  p <- pdSite(pdSimulate(dt, maximum = 480, age = 64))
  expect_equal_rounded(2.065, p$Y[nrow(p)], tolerance = 0.002)
})

test_that("the gel is absorbed faster than tablets", {
  gel <- pdPeak(pdSimulate(data.frame(Drug = "levodopaIntestinalGel", Time = 0, Dose = 200,
                                      Units = "mg PO"), maximum = 240, age = 64))
  tab <- pdPeak(pdSimulate(data.frame(Drug = "levodopa", Time = 0, Dose = 200,
                                      Units = "mg PO"), maximum = 240, age = 64))
  expect_lt(gel[["t"]], tab[["t"]] / 2)
})
