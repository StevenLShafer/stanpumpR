# Dexmedetomidine: Morse 2020, the universal model (since 2026-11-17).  The
# infant bypass model that was its branch at age <= 1 is
# test-drugs-dexmedetomidineInfantCPB.R.

morseMF <- function(pma) pma / (52.4 + pma)

test_that("the standard man of Table 1 receives the published values", {
  # 70 kg, 176 cm, 35 y man: Al-Sallami FFM 56.13 kg, the paper's 56.1
  x <- dexmedetomidine(70, 176, 35, "male")$PK$default
  expect_equal(x$v1, 25.2, tolerance = 0.001)
  expect_equal(x$v2, 34.4, tolerance = 0.001)
  expect_equal(x$v3, 65.4, tolerance = 0.001)
  expect_equal(x$cl2, 1.68, tolerance = 0.001)
  expect_equal(x$cl3, 0.62, tolerance = 0.001)
  # clearance carries the unnormalised maturation, 0.973 at 35 years
  expect_equal(x$cl1, 0.897 * morseMF(40 + 52 * 35), tolerance = 0.001)
  expect_equal_rounded(unlist(x),
    c(v1 = 25.208103, v2 = 34.411061, v3 = 65.421028,
      cl1 = 0.872741, cl2 = 1.680615, cl3 = 0.620227))
})

test_that("the library's reference man is pinned", {
  x <- dexmedetomidine(70, 170, 35, "male")
  expect_equal_rounded(unlist(x$PK$default),
    c(v1 = 24.718918, v2 = 33.743285, v3 = 64.151479,
      cl1 = 0.853402, cl2 = 1.643374, cl3 = 0.606483))
  expect_equal(x$tPeak, 14.9)
  expect_equal(x$MEAC, 0)
  expect_equal(c(x$lowerTypical, x$typical, x$upperTypical), c(0.4, 0.6, 0.8))
  expect_match(x$reference, "Morse JD")
})

test_that("volumes follow normal fat mass and clearances fat-free mass", {
  ffm <- ffmAlSallami(140, 176, 40, "male")
  nfm <- ffm + 0.293 * (140 - ffm)
  x <- dexmedetomidine(140, 176, 40, "male")$PK$default
  expect_equal(x$v1, 25.2 * nfm / (56.1 + 0.293 * 13.9), tolerance = 1e-8)
  expect_equal(x$cl2, 1.68 * (ffm / 56.1)^0.75, tolerance = 1e-8)
  expect_equal(x$cl1, 0.897 * (ffm / 56.1)^0.75 * morseMF(40 + 52 * 40),
               tolerance = 1e-8)
  expect_equal_rounded(unlist(x),
    c(v1 = 40.549187, v2 = 55.352858, v3 = 105.234795,
      cl1 = 1.130833, cl2 = 2.170297, cl3 = 0.800943))
})

test_that("clearance matures with postmenstrual age, as the paper says", {
  # "Clearance in term neonates is 42% of adult values, reaching 80% by 3 years"
  expect_equal(morseMF(40), 0.433, tolerance = 0.002)
  expect_equal(morseMF(40 + 52 * 3), 0.789, tolerance = 0.002)
  term <- dexmedetomidine(3.5, 50, 0, "female")$PK$default
  pre  <- dexmedetomidine(3.5, 50, 0, "female", gestationalAge = 28)$PK$default
  expect_equal(term$cl1 / pre$cl1, morseMF(40) / morseMF(28), tolerance = 1e-8)
  # maturation is on clearance alone
  expect_equal(term[c("v1", "v2", "v3", "cl2", "cl3")],
               pre[c("v1", "v2", "v3", "cl2", "cl3")])
  expect_equal_rounded(c(term$cl1, pre$cl1), c(0.0426725, 0.0343291))
})

test_that("the size switch does not change the model", {
  for (a in list(c(70, 170, 35), c(140, 176, 40), c(10, 75, 1))) {
    expect_equal(dexmedetomidine(a[1], a[2], a[3], "female", adjustToFFM = FALSE),
                 dexmedetomidine(a[1], a[2], a[3], "female", adjustToFFM = TRUE))
  }
})

test_that("Morse is one model at every age, with no step at one year", {
  infant <- dexmedetomidine(9, 75, 1,     "male")$PK
  child  <- dexmedetomidine(9, 75, 1.001, "male")$PK
  expect_named(infant, PK_EVENT_DEFAULT)
  expect_equal(infant$default$v1, child$default$v1, tolerance = 1e-3)
})

test_that("the model is finite at the extremes and runs through getDrugPK()", {
  for (s in c("male", "female")) for (a in c(0, 0.01, 18, 90))
    for (w in c(0.5, 3, 70, 250)) {
      x <- unlist(dexmedetomidine(w, if (w < 5) 45 else 170, a, s,
                                  gestationalAge = 22)$PK$default)
      expect_true(all(is.finite(x) & x > 0), info = paste(s, a, w))
    }
  pk <- getDrugPK("dexmedetomidine", 70, 170, 35, "male")$PK$default
  expect_gt(pk$ke0, 0)
  expect_true(all(is.finite(c(pk$lambda_1, pk$lambda_2, pk$lambda_3))))
})
