# Doses per square metre of body surface area (R/bsa-dosing.R): the
# chemotherapy drugs are labelled per m2, so a dose in "mg/m2" must give the
# same curve as the same dose in mg times the patient's body surface area.

test_that("the per-m2 and per-kg units are recognised", {
  expect_true(all(isBsaUnit(c("mg/m2", "g/m2/hr", "mg/m2/day", "mg/m2 PO",
                              "mg/m2 PO bid", "U/m2"))))
  expect_false(any(isBsaUnit(c("mg", "mg/kg", "mg/min", "mg/hr", "mg PO"))))
  expect_true(all(isPerKgUnit(c("mg/kg", "mcg/kg/min", "mg/kg/day", "U/kg",
                                "mg/kg PO qd", "mg/kg citrate"))))
  expect_false(any(isPerKgUnit(c("mg/m2", "mg", "mOsm"))))
  expect_true(all(c(bsaBolusUnits, bsaInfusionUnits, "mg/m2 PO",
                    ivPotencyBodySizeUnits) %in% allUnits))
  expect_true(all(c("mg/m2 PO bid", "mg/m2 PO qd", "mg/m2 qd") %in% scheduledUnits))
  expect_true(all(isRateUnit(c(bsaInfusionUnits, "mg/kg/day"))))
  expect_false(any(isRateUnit(c(bsaBolusUnits, "mg/m2 PO", ivPotencyBodySizeUnits))))
  expect_true(all(doseRoute(c(bsaBolusUnits, bsaInfusionUnits, "U/m2")) == ROUTE_IV))
  expect_equal(doseRoute("mg/m2 PO bid"), ROUTE_PO)
})

test_that("bodySizeDoseFactor() is the weight per kg and the Mosteller BSA per m2", {
  f <- bodySizeDoseFactor(c("mg", "mg/kg", "mg/m2", "mg/m2/hr", "U/kg"), 80, 180)
  expect_equal(f, c(1, 80, rep(sqrt(80 * 180 / 3600), 2), 80))
  expect_error(bodySizeDoseFactor("mg/m2", 80, NA), "height")
  expect_equal(bodySizeDoseFactor("mg/kg", 80), 80)
})

simBsa <- function(drug, dose, units, weight = 70, height = 170) {
  r <- simulateDrugsWithCovariates(
    doseTable = data.frame(Drug = drug, Time = 0, Dose = dose, Units = units),
    weight = weight, height = height, age = 50, sex = "male", maximum = 600)
  r[[drug]]$results$Cp
}

test_that("a per-m2 dose is the absolute dose times the body surface area", {
  bsa <- sqrt(90 * 185 / 3600)
  cases <- list(
    list("fluorouracil", 400, "mg/m2", 400 * bsa, "mg"),
    list("fluorouracil", 1200, "mg/m2/day", 1200 * bsa / 24, "mg/hr"),
    list("ifosfamide", 1.2, "g/m2/hr", 1200 * bsa, "mg/hr"),
    list("capecitabine", 1250, "mg/m2 PO bid", 1250 * bsa, "mg PO bid"),
    list("temozolomide", 150, "mg/m2 PO qd", 150 * bsa, "mg PO qd"),
    list("bleomycin", 15, "U/m2", 15 * bsa, "U"))
  for (k in cases) {
    a <- simBsa(k[[1]], k[[2]], k[[3]], 90, 185)
    b <- simBsa(k[[1]], k[[4]], k[[5]], 90, 185)
    expect_gt(max(b), 0)
    expect_equal(a, b, tolerance = 1e-10, info = paste(k[[1]], k[[3]]))
  }
})

test_that("the label's per-kg units reach the engine", {
  expect_equal(simBsa("cladribine", 0.09, "mg/kg/day"),
               simBsa("cladribine", 0.09 * 70 / 24, "mg/hr"), tolerance = 1e-10)
  expect_equal(simBsa("bleomycin", 0.3, "U/kg"),
               simBsa("bleomycin", 0.3 * 70, "U"), tolerance = 1e-10)
  expect_equal(simBsa("thioguanine", 2, "mg/kg PO qd"),
               simBsa("thioguanine", 140, "mg PO qd"), tolerance = 1e-10)
})

test_that("every chemotherapy drug labelled per m2 offers a per-m2 unit", {
  d <- getDrugDefaultsGlobal()
  chemo <- d[d$Category %in% CHEMOTHERAPY_DRUG_CATEGORY, ]
  # The drugs whose FDA label (inst/extdata/fdaLabels.csv) doses per m2.
  perM2 <- c("fluorouracil", "carboplatin", "methotrexate", "pemetrexed",
             "melphalan", "vincristine", "docetaxel", "cyclophosphamide",
             "doxorubicin", "doxorubicinLiposomal", "irinotecan", "topotecan",
             "vinorelbine", "cisplatin", "etoposide", "daunorubicin",
             "gemcitabine", "capecitabine", "paclitaxel",
             "paclitaxelProteinBound", "oxaliplatin", "bendamustine",
             "fludarabine", "cytarabine", "ifosfamide", "epirubicin",
             "temozolomide", "dacarbazine", "bleomycin")
  expect_true(all(perM2 %in% chemo$Drug))
  for (drug in perM2) {
    u <- chemo$Units[[match(drug, chemo$Drug)]]
    expect_true(any(isBsaUnit(u)), info = drug)
  }
  # Every unit a chemotherapy drug offers is one the dose table accepts.
  expect_true(all(unlist(chemo$Units) %in% c(allUnits, scheduledUnits)))
})

test_that("the Visualizer reads a per-m2 dose as simCpCe() does", {
  u <- list(amount = "mg")
  expect_equal(viewerBaseDose(c(100, 100, 60), c("mg/m2", "mg/kg", "mg/m2/hr"), 80, u, 180),
               c(100 * sqrt(80 * 180 / 3600), 8000, sqrt(80 * 180 / 3600)))
})
