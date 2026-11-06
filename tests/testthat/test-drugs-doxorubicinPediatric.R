# Doxorubicin in children (R/drugs_doxorubicinPediatric.R,
# R/drugs_doxorubicinolPediatric.R): Voller 2015.  Drafted by Claude Code,
# 2026-11-06.

bsaHeight <- function(bsa, w) bsa^2 * 3600 / w   # Mosteller

test_that("Eq. 1 reproduces the paper's three clearances: 24.1, 12.0 and 29.1 L/h/m2", {
  cl <- function(bsa, age, w) {
    doxorubicinPediatric(w, bsaHeight(bsa, w), age, "female",
                         adjustToFFM = FALSE)$PK$default$cl1 * 60 / bsa
  }
  expect_equal(round(cl(0.77, 5.32, 19.3), 1), 24.1)
  expect_equal(round(cl(0.215, 0, 3.5), 1), 12.0)
  expect_equal(round(cl(1.8, 18, 65), 1), 29.1)
})

test_that("Table 2 at the median child: half-lives 0.10, 1.96 and 31.2 h; doxorubicinol 12.4 h", {
  set <- getDrugPK("doxorubicinPediatric", 19.3, bsaHeight(0.77, 19.3), 5.32, "male",
                   adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(set$v1, 9.34 * 0.77)
  expect_equal_rounded(set$v2, 560 * 0.77)
  halfLives <- sort(log(2) / c(set$lambda_1, set$lambda_2, set$lambda_3) / 60)
  expect_equal(round(halfLives, c(2, 2, 1)), c(0.10, 1.96, 31.2))
  m <- doxorubicinolPediatric(19.3, bsaHeight(0.77, 19.3), 5.32, "male",
                              adjustToFFM = FALSE)$PK$default
  expect_equal(round(log(2) * m$v1 / (m$cl1 * 60), 1), 12.4)
  pk <- getDrugPK("doxorubicinPediatric", 19.3, bsaHeight(0.77, 19.3), 5.32, "male",
                  adjustToFFM = FALSE)$PK$default
  expect_identical(pk$metabolite$name, "doxorubicinolPediatric")
})

test_that("doxorubicinPediatric is a chemotherapy drug, plasma only, with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "doxorubicinPediatric", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_identical(row$Concentration.Units, "ng")
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  PK <- getDrugPK("doxorubicinPediatric", 20, 110, 5, "male")
  expect_match(PK$reference, "Voller")
  expect_identical(therapeuticRange("doxorubicinPediatric")$band_status, "not_applicable")
  expect_identical(therapeuticRange("doxorubicinolPediatric")$band_status, "not_applicable")
})
