# The serum creatinine covariate, shared by every model with a renal term.
# See R/renalFunction.R.

renalDrugs <- c("mannitol", "vancomycin", "gentamicin", "cefazolin", "sugammadex",
                "gabapentin")

test_that("a blank creatinine is the assumed normal value for the patient's sex", {
  expect_equal(patientCreatinine(NULL, "male"), SCR_ASSUMED_MALE)
  expect_equal(patientCreatinine(NA, "female"), SCR_ASSUMED_FEMALE)
  expect_equal(patientCreatinine(2.5, "female"), 2.5)
})

test_that("every renal model takes the creatinine, and the rest ignore it", {
  for (drug in getDrugDefaultsGlobal()$Drug[!isGasDrug(getDrugDefaultsGlobal()$Drug)]) {
    takes <- "creatinine" %in% names(formals(get(drug, mode = "function")))
    expect_identical(takes, drug %in% renalDrugs, info = drug)
  }
})

test_that("each renal model is unchanged at the assumed creatinine", {
  # Entering the assumed value must reproduce leaving the field blank, so
  # existing simulations do not move.
  for (drug in renalDrugs) {
    for (sex in SEX_VALUES) {
      blank   <- getDrugPK(drug, 70, 170, 50, sex)
      entered <- getDrugPK(drug, 70, 170, 50, sex,
                           creatinine = assumedCreatinine(sex))
      expect_equal(entered$PK, blank$PK, info = paste(drug, sex))
    }
  }
})

test_that("a raised creatinine lowers every renal clearance", {
  for (drug in renalDrugs) {
    normal <- getDrugPK(drug, 70, 170, 50, "male")$PK$default$cl1
    raised <- getDrugPK(drug, 70, 170, 50, "male", creatinine = 3)$PK$default$cl1
    expect_lt(raised, normal, label = drug)
  }
})

test_that("a non-renal model ignores the creatinine", {
  expect_identical(getDrugPK("propofol", 70, 170, 50, "male", creatinine = 3),
                   getDrugPK("propofol", 70, 170, 50, "male"))
})

test_that("vancomycin floors the creatinine at 60 umol/L as Thomson did", {
  # 0.3 mg/dL is below the 0.68 mg/dL floor, so it gives the floor's clearance.
  low   <- vancomycin(70, 170, 50, "male", creatinine = 0.3)$PK$default$cl1
  floor <- vancomycin(70, 170, 50, "male", creatinine = 60 / 88.42)$PK$default$cl1
  expect_equal(low, floor)
})
