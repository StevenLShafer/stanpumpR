# Duong 2019 (R/drugs_carboplatinPediatric.R, R/drugs_melphalanPediatric.R,
# R/drugs_etoposideDuong.R).  Drafted by Claude Code, 2026-11-06.

halfLives <- function(drug, w = 15.5) {
  set <- getDrugPK(drug, w, 100, 4, "male", adjustToFFM = FALSE)$PK$default
  sort(log(2) / c(set$lambda_1, set$lambda_2) / 60)
}

test_that("Table 3 at the reference weight, 15.5 kg (Shafer's choice)", {
  expect_equal(DUONG_WT_STD, 15.5)
  for (d in list(list("carboplatinPediatric", "carboplatin"),
                 list("etoposideDuong", "etoposide"),
                 list("melphalanPediatric", "melphalan"))) {
    set <- getDrugPK(d[[1]], 15.5, 100, 4, "female", adjustToFFM = FALSE)$PK$default
    p <- DUONG_TABLE3[[d[[2]]]]
    expect_equal_rounded(set$cl1 * 60, p[["cl"]])
    expect_equal_rounded(set$v1, p[["v1"]])
    expect_equal_rounded(set$cl2 * 60, p[["q"]])
    expect_equal_rounded(set$v2, p[["v2"]])
  }
})

test_that("the derived half-lives: carboplatin 1.62 and 2.93 h, melphalan 0.30 and 1.36 h", {
  expect_equal(round(halfLives("carboplatinPediatric"), 2), c(1.62, 2.93))
  expect_equal(round(halfLives("melphalanPediatric"), 2), c(0.30, 1.36))
  expect_equal(round(halfLives("etoposideDuong"), 1), c(2.8, 42.5))
})

test_that("weight scales the clearances to the 3/4 power and the volumes in proportion", {
  a <- carboplatinPediatric(31, 130, 8, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(a$cl1 * 60, 2.7 * (31 / 15.5)^0.75)
  expect_equal_rounded(a$v2, 0.74 * 31 / 15.5)
  # carboplatin's AUC = dose / CL: 50 mg/kg at 16.8 kg is about Table 4's 17.4 mg/mL.min
  cl <- carboplatinPediatric(16.8, 105, 4, "male", adjustToFFM = FALSE)$PK$default$cl1 * 1000
  expect_equal(50 * 16.8 / cl, 17.4, tolerance = 0.05)
})

test_that("the three are chemotherapy drugs, plasma only, with no band", {
  dd <- getDrugDefaultsGlobal()
  for (drug in c("carboplatinPediatric", "etoposideDuong", "melphalanPediatric")) {
    row <- dd[dd$Drug == drug, ]
    expect_true(is.na(row$Category) || row$Category == "")
  expect_true(row$Drug %in% unlist(lapply(MODEL_GROUPS, function(g) g$model)))  # a candidate under its drug's one name
    expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
    expect_match(getDrugPK(drug, 15, 100, 4, "male")$reference, "Duong")
    expect_identical(therapeuticRange(drug)$band_status, "not_applicable")
  }
})
