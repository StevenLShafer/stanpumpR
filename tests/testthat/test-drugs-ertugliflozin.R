# ertugliflozin: see R/drugs_ertugliflozin.R (Fediuk 2021, data set 1).
test_that("the 85 kg, 65-year-old man at eGFR 90, with type 2 diabetes", {
  # creatinine chosen to give an MDRD eGFR of 90 at 65 years
  scr <- (90 / (175 * 65^-0.203))^(1 / -1.154)
  expect_equal(egfrMDRD(65, "male", scr), 90)
  pk <- ertugliflozin(85, 175, 65, "male", adjustToFFM = FALSE, creatinine = scr)$PK$default
  expect_equal(pk$cl1 * 60, 11.9 * 0.921)
  expect_equal(pk$cl2 * 60, 7.76)
  expect_equal(c(pk$v1, pk$v2), c(6.51, 107))
  expect_equal(pk$ka_PO * 60, 0.329)
  expect_equal(pk$tlag_PO, 0.226 * 60)
})
test_that("women, weight, age and eGFR (capped at 120)", {
  m <- ertugliflozin(85, 170, 65, "male", FALSE, creatinine = 1)$PK$default
  f <- ertugliflozin(85, 170, 65, "female", FALSE, creatinine = 1)$PK$default
  egfrM <- egfrMDRD(65, "male", 1); egfrF <- egfrMDRD(65, "female", 1)
  expect_equal(f$cl1 / m$cl1, 0.960 * (egfrF / egfrM)^0.458)
  expect_equal(f$v1 / m$v1, 1.46)
  heavy <- ertugliflozin(170, 170, 65, "male", FALSE, creatinine = 1)$PK$default
  expect_equal(heavy$v2 / m$v2, 2)
  expect_equal(heavy$cl2 / m$cl2, 2^0.75)
  old <- ertugliflozin(85, 170, 85, "male", FALSE, creatinine = 1)$PK$default
  expect_equal(old$v1 / m$v1, (85 / 65)^-0.192)
  young <- ertugliflozin(85, 170, 10, "male", FALSE, creatinine = 1)$PK$default
  expect_equal(young$v1, 6.51 * (18 / 65)^-0.192)   # held at 18 years
  low <- ertugliflozin(85, 170, 30, "male", FALSE, creatinine = 0.4)$PK$default
  expect_equal(low$cl1 * 60, 11.9 * 0.921 * (120 / 90)^0.458)
})
