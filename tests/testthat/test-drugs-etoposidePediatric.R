# Etoposide, children (R/drugs_etoposidePediatric.R): Urien 2011.
# Drafted by Claude Code, 2026-11-06.

test_that("Table 3 at 70 kg, and the derived half-lives and Vss", {
  set <- getDrugPK("etoposidePediatric", 70, 170, 10, "male",
                   adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(set$cl1 * 60, 2.05)
  expect_equal_rounded(set$v1, 9.3)
  expect_equal_rounded(set$cl2 * 60, 0.68)
  expect_equal_rounded(set$v2, 5.7)
  expect_equal(set$cl3, 0)
  halfLives <- sort(log(2) / c(set$lambda_1, set$lambda_2) / 60)
  expect_equal(round(halfLives, 2), c(2.07, 8.81))
  expect_equal(set$v1 + set$v2, 15)
})

test_that("weight is allometric (3/4, 1); the footnote's 15-kg check is 0.646 L/h", {
  m <- etoposidePediatric(15, 100, 3.5, "female", adjustToFFM = FALSE)$PK$default
  # the paper prints 0.75 L/h, a misprint: its own equation gives 0.646
  expect_equal(round(m$cl1 * 60, 3), 0.646)
  expect_equal_rounded(m$v1, 9.3 * 15 / 70)
  expect_equal_rounded(m$cl2, 0.68 * (15 / 70)^0.75 / 60)
  size <- pkSizeFactors(15, 100, 3.5, "female", TRUE)
  on <- etoposidePediatric(15, 100, 3.5, "female")$PK$default
  expect_equal_rounded(on$v1, 9.3 * size$pkWeight / 70)
})

test_that("etoposidePediatric is a chemotherapy drug, plasma only, with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "etoposidePediatric", ]
  expect_true(is.na(row$Category) || row$Category == "")
  expect_true(row$Drug %in% unlist(lapply(MODEL_GROUPS, function(g) g$model)))  # a candidate under its drug's one name
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  PK <- getDrugPK("etoposidePediatric", 20, 110, 5, "male")
  expect_equal(PK$tPeak, 0)
  expect_match(PK$reference, "Urien")
  expect_identical(therapeuticRange("etoposidePediatric")$band_status, "not_applicable")
})
