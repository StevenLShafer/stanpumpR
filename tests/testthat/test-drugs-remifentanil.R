calculateBMI <- function(weight, height) {
  weight / (height / 100) ^ 2
}

test_that("returns the correct calculations if BMI is > 30 and male", {
  weight <- 70
  height <- 150
  age <- 50
  sex <- "male"

  expect_gt(calculateBMI(weight, height), 30)

  actual <- remifentanil(weight, height, age, sex)

  expected <- list(
    PK = list(
      default = list(
        v1 = 4.568805,
        v2 = 6.820768,
        v3 = 3.3799,
        cl1 = 2.518915,
        cl2 = 1.576,
        cl3 = 0.197
      )
    ),
    tPeak = 1.6,
    MEAC = 1,
    typical = 1.2,
    upperTypical = 2,
    lowerTypical = 0.8,
    reference = "Kim TK et al., Anesthesiology 2017;126(6):1019-1032. https://pubmed.ncbi.nlm.nih.gov/28509796/"
  )
  expect_equal_rounded(actual, expected)
})

test_that("returns the correct calculations if BMI is <= 30 and male", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"

  expect_lte(calculateBMI(weight, height), 30)

  actual <- remifentanil(weight, height, age, sex)

  expected <- list(
    PK = list(
      default = list(
        v1 = 5.371314,
        v2 = 8.442319,
        v3 = 3.148362,
        cl1 = 2.462212,
        cl2 = 1.52838,
        cl3 = 0.08064262
      )
    ),
    tPeak = 1.6,
    MEAC = 1,
    typical = 1.2,
    upperTypical = 2,
    lowerTypical = 0.8,
    reference = "Eleveld DJ et al., Anesthesiology 2017;126(6):1005-1018. https://pubmed.ncbi.nlm.nih.gov/28509794/"
  )
  expect_equal_rounded(actual, expected)
})


test_that("returns the correct calculations if BMI is > 30 and female", {
  weight <- 70
  height <- 150
  age <- 50
  sex <- "female"

  expect_gt(calculateBMI(weight, height), 30)

  actual <- remifentanil(weight, height, age, sex)

  expected <- list(
    PK = list(
      default = list(
        v1 = 4.568805,
        v2 = 5.9494,
        v3 = 3.3799,
        cl1 = 2.518915,
        cl2 = 1.576,
        cl3 = 0.197
      )
    ),
    tPeak = 1.6,
    MEAC = 1,
    typical = 1.2,
    upperTypical = 2,
    lowerTypical = 0.8,
    reference = "Kim TK et al., Anesthesiology 2017;126(6):1019-1032. https://pubmed.ncbi.nlm.nih.gov/28509796/"
  )
  expect_equal_rounded(actual, expected)
})


test_that("returns the correct calculations if BMI is <= 30 and female", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "female"

  expect_lte(calculateBMI(weight, height), 30)

  actual <- remifentanil(weight, height, age, sex)

  expected <- list(
    PK = list(
      default = list(
        v1 = 4.403734,
        v2 = 8.051189,
        v3 = 2.581221,
        cl1 = 2.467679,
        cl2 = 1.715685,
        cl3 = 0.06948164
      )
    ),
    tPeak = 1.6,
    MEAC = 1,
    typical = 1.2,
    upperTypical = 2,
    lowerTypical = 0.8,
    reference = "Eleveld DJ et al., Anesthesiology 2017;126(6):1005-1018. https://pubmed.ncbi.nlm.nih.gov/28509794/"
  )
  expect_equal_rounded(actual, expected)
})

# The switch to Kim's obesity model is for adults only: Kim's subjects were
# aged 20 to 85, Eleveld's data began in infancy.  Before 2026-10-12 a
# 5-year-old's clearance jumped 67% on crossing BMI 30.
test_that("a child stays on Eleveld's model on both sides of BMI 30", {
  below <- remifentanil(43.1, 120, 5, "male")
  at    <- remifentanil(43.2, 120, 5, "male")
  expect_lt(calculateBMI(43.1, 120), 30)
  expect_gte(calculateBMI(43.2, 120), 30)

  expect_match(below$reference, "Eleveld")
  expect_match(at$reference, "Eleveld")
  expect_equal_rounded(below$PK$default$cl1, 1.662147)
  expect_equal_rounded(at$PK$default$cl1, 1.663649)
  # continuous across the boundary: a 0.1 kg change moves clearance under 1%
  expect_lt(abs(at$PK$default$cl1 / below$PK$default$cl1 - 1), 0.01)
})

test_that("an adult at BMI 30 is on Kim's model from the age the rule names", {
  expect_equal(REMIFENTANIL_KIM_MIN_AGE, 18)
  child <- remifentanil(43.2, 120, REMIFENTANIL_KIM_MIN_AGE - 0.1, "female")
  adult <- remifentanil(43.2, 120, REMIFENTANIL_KIM_MIN_AGE, "female")
  lean  <- remifentanil(43.1, 120, REMIFENTANIL_KIM_MIN_AGE, "female")

  expect_match(child$reference, "Eleveld")
  expect_match(adult$reference, "Kim")
  expect_match(lean$reference, "Eleveld")
  expect_equal_rounded(child$PK$default$cl1, 2.204358)
  expect_equal_rounded(adult$PK$default$cl1, 2.589623)
  expect_equal_rounded(adult$PK$default$v1, 3.325649)
  expect_equal_rounded(lean$PK$default$cl1, 2.203018)
})

test_that("the Pregnant field applies the review's factor (R/pregnancy.R)", {
  # A woman of 30 years, 70 kg, 165 cm, fat-free-mass scaling on.  Eleveld, CL1 x 2.
  # The values are those the model gives with the flag on; test-pregnancy.R
  # checks the ratio against the flag off and that nothing else moves.
  actual <- remifentanil(70, 165, 30, "female", pregnant = TRUE)$PK$default
  expect_equal_rounded(4.812838479, actual$v1)
  expect_equal_rounded(10.34103066, actual$v2)
  expect_equal_rounded(4.742054499, actual$v3)
  expect_equal_rounded(6.374084526, actual$cl1)
  expect_equal_rounded(2.844531893, actual$cl2)
  expect_equal_rounded(0.1225004483, actual$cl3)
  expect_equal(remifentanil(70, 165, 30, "female")$PK$default,
               remifentanil(70, 165, 30, "female", pregnant = FALSE)$PK$default)
  expect_match(remifentanil(70, 165, 30, "female", pregnant = TRUE)$reference, "Ansari", fixed = TRUE)
  expect_no_match(remifentanil(70, 165, 30, "female")$reference, "Ansari")
})

test_that("an obese adult for whom Kim's model is not defined gets Eleveld's (issue #310)", {
  # A woman of 100 cm, 35 kg (BMI 35), 88 years: Kim's linear age term takes
  # V2 below zero, so Eleveld's model is used, and the citation says why.
  kim <- remifentanilKim(35, 100, 88, "female")
  expect_lt(kim$parameters$v2, 0)
  x <- remifentanil(35, 100, 88, "female")
  expect_equal(x$PK[[PK_EVENT_DEFAULT]], remifentanilEleveld(35, 100, 88, "female"))
  expect_true(all(unlist(x$PK[[PK_EVENT_DEFAULT]]) > 0))
  expect_match(x$reference, "^Eleveld DJ")
  expect_match(x$reference, "Kim's model for obesity gives a non-positive V2", fixed = TRUE)
  # getDrugPK() builds it without complaint
  pk <- getDrugPK("remifentanil", 35, 100, 88, "female")$PK[[1]]
  expect_true(all(c(pk$lambda_1, pk$lambda_2, pk$lambda_3) > 0))
  # An ordinary obese adult keeps Kim's model, unchanged
  y <- remifentanil(120, 170, 50, "male")
  expect_match(y$reference, "^Kim TK")
  expect_equal(y$PK[[PK_EVENT_DEFAULT]], remifentanilKim(120, 170, 50, "male")$parameters)
})
