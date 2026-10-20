# quetiapine: see R/drugs_quetiapine.R (Zheng 2024).  Pins are the published
# numbers, and for the scaled patient the published numbers times the
# fat-free-mass factors in docs/adding-a-drug.md, worked by hand.

test_that("the reference patient receives Zheng's published values", {
  for (adjust in c(TRUE, FALSE)) {
    actual <- quetiapine(70, 170, 35, "male", adjustToFFM = adjust)
    expected <- list(
      PK = list(default = list(
        v1 = 530, v2 = 1, v3 = 1,
        cl1 = 76.1 / 60, cl2 = 0, cl3 = 0,
        ka_PO = 1.46 / 60, bioavailability_PO = 1, tlag_PO = 0
      )),
      tPeak = 0, MEAC = 0,
      typical = 0, upperTypical = 0, lowerTypical = 0,
      reference = actual$reference
    )
    expect_equal_rounded(actual, expected)
  }
})

test_that("the switch off reproduces Zheng's allometry; on, fat-free mass", {
  off <- quetiapine(120, 170, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(off$v1, 530 * 120 / 70)
  expect_equal(off$cl1, 76.1 / 60 * (120 / 70)^0.75)

  on <- quetiapine(120, 170, 50, "male")$PK$default
  expect_equal_rounded(on$v1, 530 * 1.3049067)
  expect_equal_rounded(on$cl1, 76.1 / 60 * 1.2209126)
})

test_that("it is oral only and has no effect site", {
  dd <- getDrugDefaultsGlobal()
  units <- dd$Units[dd$Drug == "quetiapine"][[1]]
  expect_true(all(doseRoute(units) == ROUTE_PO))
  pk <- getDrugPK("quetiapine", 70, 170, 35, "male", getDrugDefaults("quetiapine"))
  expect_equal(pk$PK$default$ke0, 0)
})

test_that("pregnancy: apparent clearance x 4.2 (Westin 2017)", {
  # A woman of 30 years, 70 kg, 165 cm, fat-free-mass scaling on.
  on  <- getDrugPK("quetiapine", 70, 165, 30, SEX_FEMALE, pregnant = TRUE)
  off <- getDrugPK("quetiapine", 70, 165, 30, SEX_FEMALE, pregnant = FALSE)
  expect_equal(off, getDrugPK("quetiapine", 70, 165, 30, SEX_FEMALE))
  a <- on$PK$default
  b <- off$PK$default
  expect_equal(QUETIAPINE_PREGNANCY_CL_FACTOR, 4.2)
  expect_equal(a$cl1 / b$cl1, QUETIAPINE_PREGNANCY_CL_FACTOR, tolerance = 1e-6)
  for (p in c("v1", "v2", "v3", "cl2", "cl3", "ka_PO", "bioavailability_PO", "tlag_PO", "ke0")) {
    expect_identical(a[[p]], b[[p]], info = p)
  }
  expect_equal_rounded(4.531817256, a$cl1)
  expect_equal_rounded(427.2303782, a$v1)
  expect_match(on$reference, "Westin", fixed = TRUE)
  expect_match(on$reference, "pregnancy:", fixed = TRUE)
  expect_no_match(off$reference, "Westin")
})
