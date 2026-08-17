test_that("returns the correct calculations", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- oxycodone(weight, height, age, sex)

  expected <- list(
    PK = list(
      default = list(
        v1 = 90.2,
        v2 = 68.9,
        v3 = 1,
        cl1 = 0.6233333,
        cl2 = 3.433333,
        cl3 = 0,
        ka_PO = 0.06,
        bioavailability_PO = 0.5,
        tlag_PO = 0
      )
    ),
    tPeak = 60,
    MEAC = 12,
    typical = 14.4,
    upperTypical = 9.6,
    lowerTypical = 24,
    reference = "Lamminsalo 2019"
  )
  expect_equal_rounded(actual, expected)
})

test_that("the intravenous route is offered alongside the oral route", {
  # The Lamminsalo IV disposition model has always been present in drugs_oxycodone.R,
  # but Bolus.Units and Infusion.Units were blank in drugDefaults_global.csv, so the
  # UI never presented an IV option and suggest() built dose rows with NA units.
  defaults <- getDrugDefaults("oxycodone")

  expect_equal(defaults$Bolus.Units, "mg")
  expect_equal(defaults$Infusion.Units, "mg/hr")
  expect_true(all(c("mg", "mg/hr", "mg PO") %in% unlist(defaults$Units)))

  # Oxycodone is predominantly an oral drug, so the dose grid still defaults to PO.
  expect_equal(defaults$Default.Units, "mg PO")
})

test_that("oral and intravenous doses combine on a single curve", {
  events <- data.frame(Time = double(), Event = character(), Fill = character())
  simulate <- function(poDose, ivDose) {
    doseTable <- data.frame(
      Drug  = "oxycodone",
      Time  = c(0, 60),
      Dose  = c(poDose, ivDose),
      Units = c("mg PO", "mg")
    )
    simulateDrugsWithCovariates(
      dose = doseTable, events = events,
      weight = 70, height = 171, age = 50, sex = "male",
      maximum = 480, plotRecovery = FALSE
    )$oxycodone
  }

  # Giving both routes the same dose times keeps all three runs on one internal
  # timeline, so they can be compared point by point without interpolation error.
  oral  <- simulate(10, 0)
  iv    <- simulate(0, 5)
  mixed <- simulate(10, 5)

  plasma <- function(x) x$results$Y[x$results$Site == "Plasma"]

  expect_true(all(is.finite(plasma(mixed))))
  expect_gt(max(plasma(mixed)), max(plasma(oral)))

  # The disposition model is linear, so the mixed simulation must be the exact
  # superposition of its oral and intravenous parts.
  expect_equal(plasma(mixed), plasma(oral) + plasma(iv))
})
