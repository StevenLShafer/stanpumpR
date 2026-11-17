# Carboplatin in children (R/drugs_carboplatinPeng.R): Peng 1995, and its
# place in the carboplatin group.  Drafted by Claude Code, 2026-11-17.

test_that("Table 2 per m2: CL 6.31 L/h/m2, V2 = V1 k12/k21", {
  X <- carboplatinPeng(20, 115, 6, "male", adjustToFFM = FALSE)$PK$default
  bsa <- bsaMosteller(20, 115)
  expect_equal(X$v1 / bsa, 5.092252)
  expect_equal(X$cl1 / X$v1, 0.020648)
  expect_equal(X$cl2 / X$v1, 0.025259)
  expect_equal(X$cl2 / X$v2, 0.018519)
  expect_equal(round(X$cl1 / bsa * 60, 2), 6.31)
  expect_equal(round(X$v2 / bsa, 2), 6.95)
})

test_that("both switch positions scale by BSA, from the PK weight when on", {
  off <- carboplatinPeng(50, 160, 14, "male", adjustToFFM = FALSE)$PK$default
  on <- carboplatinPeng(50, 160, 14, "male", adjustToFFM = TRUE)$PK$default
  expect_equal(off$v1, 5.092252 * bsaMosteller(50, 160))
  w <- pkSizeFactors(50, 160, 14, "male", TRUE, legacyVolume = 1, legacyClearance = 1)$pkWeight
  expect_equal(on$v1, 5.092252 * bsaMosteller(w, 160))
})

test_that("the selector: Peng for infants and teenagers, Duong for young children, Duffull for adults", {
  chosen <- function(age, wt, ht) carboplatin(wt, ht, age, "male")$modelChosen
  expect_match(chosen(0.5, 7, 65), "Peng")
  expect_match(chosen(14, 50, 160), "Peng")
  expect_match(chosen(5, 18, 108), "carboplatinPediatric")
  expect_match(chosen(8, 35, 135), "Peng")          # outside Duong's weights
  expect_match(chosen(30, 70, 175), "Peng")         # nearer Peng (18.4) than Duffull (49)
  expect_match(chosen(40, 70, 175), "Duffull")
  expect_match(chosen(60, 70, 175), "Duffull")
})

test_that("a chemotherapy candidate: no Category, no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "carboplatinPeng", ]
  expect_true(is.na(row$Category) || row$Category == "")
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  expect_match(getDrugPK("carboplatinPeng", 50, 160, 14, "male")$reference, "Peng")
})
