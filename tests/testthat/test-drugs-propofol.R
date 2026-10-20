test_that("returns the correct calculations for male", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- propofol(weight, height, age, sex)

  expected <- list(
    PK = list(
      default = list(
        v1 = 6.283078,
        v2 = 20.17078,
        v3 = 137.4287,
        cl1 = 1.551355,
        cl2 = 1.516118,
        cl3 = 0.6698454
      )
    ),
    tPeak = 1.6,
    reference = "Eleveld DJ et al., Br J Anaesth 2018;120(5):942-959. https://pubmed.ncbi.nlm.nih.gov/29661412/"
  )

  expect_equal_rounded(actual, expected)
})

test_that("returns the correct calculations for female", {
  weight <- 55
  height <- 160
  age <- 50
  sex <- "female"
  actual <- propofol(weight, height, age, sex)

  expected <- list(
    PK = list(
      default = list(
        v1 = 5.772932,
        v2 = 15.84847,
        v3 = 92.30994,
        cl1 = 1.519402,
        cl2 = 1.265265,
        cl3 = 0.4969959
      )
    ),
    tPeak = 1.6,
    reference = "Eleveld DJ et al., Br J Anaesth 2018;120(5):942-959. https://pubmed.ncbi.nlm.nih.gov/29661412/"
  )

  expect_equal_rounded(actual, expected)
})

test_that("pregnancy: clearance x 1.28 (Gin 1990 and 1991 against this model)", {
  on  <- getDrugPK("propofol", 70, 165, 30, SEX_FEMALE, pregnant = TRUE)
  off <- getDrugPK("propofol", 70, 165, 30, SEX_FEMALE, pregnant = FALSE)
  expect_identical(off, getDrugPK("propofol", 70, 165, 30, SEX_FEMALE))
  expect_equal(PROPOFOL_PREGNANCY_CL_FACTOR, 1.28)
  expect_equal(on$PK$default$cl1 / off$PK$default$cl1, PROPOFOL_PREGNANCY_CL_FACTOR,
               tolerance = 1e-12)
  for (k in c("v1", "v2", "v3", "cl2", "cl3"))
    expect_identical(on$PK$default[[k]], off$PK$default[[k]], info = k)
  expect_identical(on$tPeak, off$tPeak)
  # The factor: each caesarean group's whole-blood clearance (Gin 1990: 2.55
  # L/min at 65.3 kg, 29.4 y; Gin 1991: 31.5 mL/kg/min at 72.5 kg, 27.4 y)
  # over this model's for a nonpregnant woman of the same age and weight,
  # combined as a geometric mean weighted by group size (10 and 8).  The
  # model reproduces Gin 1990's nonpregnant group (1.57 L/min, 52.8 kg, 33.3 y).
  cl <- function(w, a) getDrugPK("propofol", w, 158, a, SEX_FEMALE)$PK$default$cl1
  expect_equal(cl(52.8, 33.3), 1.57, tolerance = 0.02)
  r90 <- 2.55 / cl(65.3, 29.4)
  r91 <- 31.5 * 72.5 / 1000 / cl(72.5, 27.4)
  expect_equal(exp((10 * log(r90) + 8 * log(r91)) / 18), PROPOFOL_PREGNANCY_CL_FACTOR,
               tolerance = 0.005)
  expect_match(on$reference, "Gin T et al.", fixed = TRUE)
  expect_no_match(off$reference, "Gin T")
})
