# Fluorouracil (R/drugs_fluorouracilTan.R): Tan 2026, two compartments with
# Michaelis-Menten elimination alone.  Drafted by Claude Code, 2026-11-06.

p <- FLUOROURACIL_TAN
noEvents <- data.frame(Time = double(), Event = character())
auc <- function(w) sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2) / 60

test_that("the model is Table 2 at the patient's BSA", {
  w <- 80; h <- 175
  PK <- getDrugPK("fluorouracilTan", w, h, 60, "male", adjustToFFM = FALSE)
  bsa <- sqrt(w * h / 3600)
  expect_equal_rounded(PK$michaelisMenten$vmax, 1900 * (bsa / 1.94)^1.14 / 60)
  expect_equal_rounded(PK$michaelisMenten$km, 8.04)
  expect_null(PK$michaelisMenten$clLinear)
  set <- PK$PK$default
  expect_equal_rounded(set$v1, 19.7)
  expect_equal_rounded(set$v2, 15)
  expect_equal_rounded(set$cl2, 52.4 / 60)
})

test_that("Table 3's median AUCs are reproduced at BSA 1.94", {
  w <- 80; h <- 169.3                       # Mosteller 1.94 m2
  bsa <- sqrt(w * h / 3600)
  run <- function(bolus) {
    d <- if (bolus == 0)
      data.frame(Drug = "fluorouracilTan", Time = c(0, 46 * 60),
                 Dose = c(2400 * bsa / 46, 0), Units = "mg/hr")
    else
      data.frame(Drug = "fluorouracilTan", Time = c(0, 10, 10, 10 + 46 * 60),
                 Dose = c(bolus * bsa / 10 * 60, 0, 2400 * bsa / 46, 0), Units = "mg/hr")
    auc(simulateDrugsWithCovariates(d, noEvents, w, h, 60, "male", 60 * 60, FALSE,
                                    adjustToFFM = FALSE)$fluorouracilTan$wide)
  }
  expect_equal(run(0), 21, tolerance = 0.03)     # 2400 over 46 h
  expect_equal(run(400), 29, tolerance = 0.03)   # 400 bolus + 2400
  expect_equal(run(200), 24, tolerance = 0.03)   # 200 bolus + 2400
})

test_that("the plateau is R Km / (Vmax - R), an independent check of the engine", {
  w <- 80; h <- 169.3
  bsa <- sqrt(w * h / 3600)
  rate <- 2400 * bsa / 46                          # mg/h
  vmax <- p$vmax * (bsa / 1.94)^1.14
  css <- rate * p$km / (vmax - rate)
  wide <- simulateDrugsWithCovariates(
    data.frame(Drug = "fluorouracilTan", Time = c(0, 46 * 60), Dose = c(rate, 0),
               Units = "mg/hr"),
    noEvents, w, h, 60, "male", 50 * 60, FALSE, adjustToFFM = FALSE)$fluorouracilTan$wide
  expect_equal(wide$Plasma[which.min(abs(wide$Time - 40 * 60))], css, tolerance = 1e-3)
})

test_that("fluorouracilTan is a chemotherapy drug, plasma only, with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "fluorouracilTan", ]
  expect_true(is.na(row$Category) || row$Category == "")
  expect_true(row$Drug %in% unlist(lapply(MODEL_GROUPS, function(g) g$model)))  # a candidate under its drug's one name
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  PK <- getDrugPK("fluorouracilTan", 70, 170, 60, "male")
  expect_equal(PK$tPeak, 0)
  expect_match(PK$reference, "Tan Z")
  expect_identical(therapeuticRange("fluorouracilTan")$band_status, "not_applicable")
})
