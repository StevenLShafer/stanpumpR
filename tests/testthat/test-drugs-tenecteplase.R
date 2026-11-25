# tenecteplase: see R/drugs_tenecteplase.R (Tang 2023, Table 1 and Equation 9).
crclNM <- function(age, sex, scr) {
  # Cockcroft-Gault normalised to 70 kg, from the paper's definition
  (140 - age) * 70 / (72 * scr) * (if (sex == "female") 0.85 else 1)
}
tang <- function(wt, age, sex, scr) {
  w <- wt / 70
  list(cl = (91.3 + 0.322 * (crclNM(age, sex, scr) - 82.6)) * w^0.75 / 1000,
       vc = 3.61 * w, q = 0.0100 * w^0.75, vp = 0.914 * w)
}

test_that("Table 1 and Equation 9, switch off", {
  for (p in list(c(70, 40, 1.0), c(81.8, 65, 0.9), c(120, 80, 2.5))) {
    pk <- tenecteplase(p[1], 175, p[2], "male", FALSE, creatinine = p[3])$PK$default
    ref <- tang(p[1], p[2], "male", p[3])
    expect_equal(c(pk$cl1, pk$v1, pk$cl2, pk$v2), c(ref$cl, ref$vc, ref$q, ref$vp))
  }
  pk <- tenecteplase(60, 160, 50, "female", FALSE, creatinine = 1.1)$PK$default
  expect_equal(pk$cl1, tang(60, 50, "female", 1.1)$cl)
})

test_that("the paper's reference patient and AUCs", {
  # CL 103 mL/min at 81.8 kg and CRCL-NM 82.6 (Discussion)
  expect_equal(91.3 * (81.8 / 70)^0.75, 103, tolerance = 0.005)
  # an age and creatinine that give CRCL-NM 82.6 exactly
  scr <- (140 - 60) * 70 / (72 * 82.6)
  pk <- tenecteplase(81.8, 175, 60, "male", FALSE, creatinine = scr)$PK$default
  expect_equal(pk$cl1 * 1000, 91.3 * (81.8 / 70)^0.75)
  auc <- function(mg) mg / (pk$cl1 * 1000)   # mg.min/mL, x1000 mcg
  expect_equal(1000 * auc(0.1 * 81.8), 79.7, tolerance = 0.01)
  expect_equal(1000 * auc(0.5 * 81.8), 398, tolerance = 0.01)
  expect_equal(1000 * auc(50), 487, tolerance = 0.01)
  # the label's ranges (12.3)
  expect_equal(pk$v1, 4.22, tolerance = 0.002)
  k10 <- pk$cl1 / pk$v1; k12 <- pk$cl2 / pk$v1; k21 <- pk$cl2 / pk$v2
  beta <- ((k10 + k12 + k21) - sqrt((k10 + k12 + k21)^2 - 4 * k10 * k21)) / 2
  # the model's terminal half-life (77 min) is below the label's 90-130 min;
  # the clearance, 103 mL/min, is inside the label's 99-119
  expect_equal(log(2) / beta, 77.3, tolerance = 0.002)
  expect_true(pk$cl1 * 1000 > 99 && pk$cl1 * 1000 < 119)
})

test_that("the pharmacokinetic weight with the switch on", {
  size <- pkSizeFactors(120, 175, 50, "male", TRUE)
  pk <- tenecteplase(120, 175, 50, "male", TRUE, creatinine = 1)$PK$default
  expect_equal(pk$cl1, tang(size$pkWeight, 50, "male", 1)$cl)
  expect_equal(pk$v1, tang(size$pkWeight, 50, "male", 1)$vc)
})

test_that("renal function: assumed normal when blank, and pregnancy", {
  blank <- tenecteplase(70, 165, 30, "female", FALSE)$PK$default
  expect_equal(blank$cl1, tang(70, 30, "female", 0.8)$cl)
  preg <- tenecteplase(70, 165, 30, "female", FALSE, pregnant = TRUE)$PK$default
  expect_equal(preg$cl1, (91.3 + 0.322 * (1.5 * crclNM(30, "female", 0.8) - 82.6)) / 1000)
  # a creatinine of 0 clearance cannot make CL negative: the intercept is 64.7
  expect_gt(tenecteplase(70, 175, 90, "male", FALSE, creatinine = 15)$PK$default$cl1, 0.06)
})
