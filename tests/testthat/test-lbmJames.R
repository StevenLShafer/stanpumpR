test_that("it returns the correct value for male", {
  weight <- 70
  height <- 171
  sex <- "male"

  actual <- lbmJames(weight, height, sex)
  expected <- 55.55067
  expect_equal_rounded(actual, expected)
})

test_that("it returns the correct value for female", {
  weight <- 55
  height <- 160
  sex <- "female"

  actual <- lbmJames(weight, height, sex)
  expected <- 41.36172
  expect_equal_rounded(actual, expected)
})

test_that("it is a parabola in weight that peaks at a fixed BMI and then falls to zero", {
  # LBM = weight * (a - b * BMI / 10000): the peak is at BMI a * 10000 / (2 b)
  # whatever the height, and LBM is zero at twice that BMI
  peakBMI <- c(male = 1.10 * 10000 / (2 * 128), female = 1.07 * 10000 / (2 * 148))
  expect_equal_rounded(unname(peakBMI), c(42.96875, 36.14865))
  for (sex in c("male", "female")) {
    for (height in c(150, 170, 190)) {
      wPeak <- peakBMI[[sex]] * (height / 100)^2
      lbm <- function(w) lbmJames(w, height, sex)
      # rises to the peak, falls after it
      expect_gt(lbm(wPeak), lbm(wPeak - 10))
      expect_gt(lbm(wPeak), lbm(wPeak + 10))
      expect_equal(optimize(lbm, c(10, 400), maximum = TRUE)$maximum, wPeak, tolerance = 1e-4)
      # zero at twice the peak BMI, negative beyond
      expect_equal(lbm(2 * wPeak), 0, tolerance = 1e-10)
      expect_lt(lbm(2 * wPeak + 1), 0)
    }
  }
  # A 160 cm woman: 49.5 kg of lean body mass at 92.5 kg, 30.4 kg at 150 kg,
  # where Al-Sallami's fat-free mass, which the library's models use, is 61.1
  expect_equal_rounded(lbmJames(92.5, 160, "female"), 49.50918)
  expect_equal_rounded(lbmJames(150, 160, "female"), 30.42188)
  expect_gt(ffmAlSallami(150, 160, 40, "female"), ffmAlSallami(92.5, 160, 40, "female"))
})
