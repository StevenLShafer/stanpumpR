# The serum creatinine covariate, shared by every model with a renal term.
# See R/renalFunction.R.

renalDrugs <- c("mannitol", "vancomycin", "gentamicin", "cefazolin", "sugammadex",
                "gabapentin", "pregabalin")

test_that("a blank creatinine is the assumed normal value for age and sex", {
  expect_equal(patientCreatinine(NULL, 40, "male"), SCR_ASSUMED_MALE)
  expect_equal(patientCreatinine(NA, 40, "female"), SCR_ASSUMED_FEMALE)
  expect_equal(patientCreatinine(2.5, 40, "female"), 2.5)
  expect_equal(patientCreatinine(NULL, 5, "male"), assumedCreatinine(5, "male"))
  expect_equal(patientCreatinine(0.9, 5, "male"), 0.9)
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
                           creatinine = assumedCreatinine(50, sex))
      expect_equal(entered$PK, blank$PK, info = paste(drug, sex))
    }
  }
})

test_that("a child's normal creatinine for age, entered, reproduces a blank field", {
  # Before the normal values for age, a blank field was an adult's
  # creatinine, so entering a child's real, normal value roughly doubled the
  # renal clearance of the Cockcroft-Gault models.
  children <- list(c(age = 0.5, weight = 7, height = 65),
                   c(age = 5, weight = 20, height = 110),
                   c(age = 15, weight = 55, height = 165))
  for (drug in renalDrugs) {
    for (sex in SEX_VALUES) {
      for (p in children) {
        blank   <- getDrugPK(drug, p[["weight"]], p[["height"]], p[["age"]], sex)
        entered <- getDrugPK(drug, p[["weight"]], p[["height"]], p[["age"]], sex,
                             creatinine = assumedCreatinine(p[["age"]], sex))
        expect_equal(entered$PK, blank$PK, info = paste(drug, sex, p[["age"]]))
      }
    }
  }
})

test_that("a child's creatinine is read against the normal for age", {
  # Mannitol's clearance is proportional to Cockcroft-Gault, so a 5 y boy at
  # twice the normal creatinine for his age clears it at half the rate he
  # does at the normal, and at the normal he clears it as before the
  # normal values for age: at Cockcroft-Gault on an adult's creatinine.
  q <- assumedCreatinine(5, "male")
  normal <- mannitol(20, 110, 5, "male", adjustToFFM = FALSE)$PK$default$cl1
  double <- mannitol(20, 110, 5, "male", adjustToFFM = FALSE, creatinine = 2 * q)$PK$default$cl1
  expect_equal(double, normal / 2)
  reference <- creatinineClearanceCG(FFM_REFERENCE_WEIGHT, FFM_REFERENCE_AGE,
                                     FFM_REFERENCE_SEX, SCR_ASSUMED_MALE)
  expect_equal(normal / mannitol(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default$cl1,
               creatinineClearanceCG(20, 5, "male", 1.0) / reference)
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
