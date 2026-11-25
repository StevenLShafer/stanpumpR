# thrombomodulinAlfa: see R/drugs_thrombomodulinAlfa.R (Mouksassi 2015, Table 2).
mouk <- function(wt, crcl) list(cl = 0.14 * (wt / 68)^0.56 * (crcl / 72.6)^0.27,
                                v = 5.47 * (wt / 68)^0.50)
cg <- function(wt, age, sex, scr) (140 - age) * wt / (72 * scr) * (if (sex == "female") 0.85 else 1)

test_that("Table 2 at 68 kg and CrCl 72.6", {
  scr <- (140 - 50) * 68 / (72 * 72.6)
  pk <- thrombomodulinAlfa(68, 170, 50, "male", FALSE, creatinine = scr)$PK$default
  expect_equal(pk$cl1 * 60, 0.14)
  expect_equal(pk$v1, 5.47)
  expect_equal(c(pk$cl2, pk$cl3), c(0, 0))
  expect_equal(log(2) * pk$v1 / (pk$cl1 * 60), 27.08, tolerance = 0.001)  # t1/2, h
})

test_that("weight and Cockcroft-Gault, held within 5-150 mL/min", {
  for (p in list(c(50, 30, 0.8), c(90, 70, 1.5), c(120, 85, 3))) {
    pk <- thrombomodulinAlfa(p[1], 175, p[2], "male", FALSE, creatinine = p[3])$PK$default
    ref <- mouk(p[1], cg(p[1], p[2], "male", p[3]))
    expect_equal(c(pk$cl1 * 60, pk$v1), c(ref$cl, ref$v))
  }
  w <- thrombomodulinAlfa(60, 160, 40, "female", FALSE, creatinine = 1)$PK$default
  expect_equal(w$cl1 * 60, mouk(60, cg(60, 40, "female", 1))$cl)
  lo <- thrombomodulinAlfa(70, 175, 85, "male", FALSE, creatinine = 15)$PK$default
  expect_equal(lo$cl1 * 60, mouk(70, 5)$cl)
  hi <- thrombomodulinAlfa(70, 175, 20, "male", FALSE, creatinine = 0.3)$PK$default
  expect_equal(hi$cl1 * 60, mouk(70, 150)$cl)
  # the pharmacokinetic weight with the switch on (Cockcroft-Gault keeps total weight)
  size <- pkSizeFactors(120, 175, 50, "male", TRUE)
  on <- thrombomodulinAlfa(120, 175, 50, "male", TRUE, creatinine = 1.2)$PK$default
  expect_equal(on$cl1 * 60, 0.14 * (size$pkWeight / 68)^0.56 * (cg(120, 50, "male", 1.2) / 72.6)^0.27)
  expect_equal(on$v1, mouk(size$pkWeight, 1)$v)
})

test_that("the paper's typical clearances by renal function; no band", {
  cl <- sapply(c(105, 75, 45, 22), function(c) mouk(68, c)$cl)
  expect_equal(cl, c(0.158, 0.145, 0.128, 0.105), tolerance = 0.05)  # medians over weights
  x <- thrombomodulinAlfa(70, 170, 40, "male")
  expect_equal(c(x$lowerTypical, x$typical, x$upperTypical), c(0, 0, 0))
  blank <- thrombomodulinAlfa(70, 165, 30, "female", FALSE)$PK$default
  expect_equal(blank$cl1 * 60, mouk(70, cg(70, 30, "female", 0.8))$cl)
  preg <- thrombomodulinAlfa(70, 165, 30, "female", FALSE, pregnant = TRUE)$PK$default
  expect_equal(preg$cl1 * 60, mouk(70, min(150, 1.5 * cg(70, 30, "female", 0.8)))$cl)
})
