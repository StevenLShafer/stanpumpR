# Methotrexate, high dose, adults (R/drugs_methotrexate.R): Ibarra 2023.
# Drafted by Claude Code, 2026-11-06.

p <- METHOTREXATE_IBARRA

test_that("Table 2 reproduces the paper's half-lives and steady-state volume", {
  set <- getDrugPK("methotrexate", 70, 170, 60, "male", adjustToFFM = FALSE,
                   creatinine = 0.92)$PK$default
  halfLives <- sort(log(2) / c(set$lambda_1, set$lambda_2, set$lambda_3) / 60)
  expect_equal(round(halfLives, 1), c(1.5, 4.9, 39.9))   # the paper: 1.5, 4.9, 40 h
  expect_equal(set$v1 + set$v2 + set$v3, 20.04)          # the paper: 20 L
  expect_equal(set$cl1 * 60, 5.92)                       # at SCRpop, 70 kg, albumin 4
})

test_that("creatinine enters as (SCRpop/SCR)^0.694, SCRpop by sex", {
  cl <- function(scr, sex = "male")
    methotrexate(70, 170, 60, sex, adjustToFFM = FALSE, creatinine = scr)$PK$default$cl1 * 60
  expect_equal(cl(0.92), 5.92)
  expect_equal(cl(0.77, "female"), 5.92)
  expect_equal(cl(1.84) / cl(0.92), 0.5^0.694)
  # a blank field: the library's assumed normal creatinine, 0.9 / 0.8 mg/dL
  blank <- methotrexate(70, 170, 60, "male", adjustToFFM = FALSE)$PK$default$cl1 * 60
  expect_equal(blank, 5.92 * (0.92 / 0.9)^0.694)
})

test_that("albumin enters clearance as (ALB/4)^0.594; blank is the reference 4 g/dL", {
  cl <- function(alb = NULL)
    methotrexate(70, 170, 60, "male", adjustToFFM = FALSE, creatinine = 0.92,
                 albumin = alb)$PK$default
  expect_equal(cl()$cl1 * 60, 5.92)
  expect_equal(cl(4)$cl1, cl()$cl1)
  # the cohort's median, 3.3 g/dL: about 11% lower
  expect_equal(cl(3.3)$cl1 * 60, 5.92 * (3.3 / 4)^0.594)
  expect_equal(round(1 - cl(3.3)$cl1 / cl()$cl1, 2), 0.11)
  expect_equal(round(1 - cl(2.5)$cl1 / cl()$cl1, 2), 0.24)
  # only clearance carries it
  expect_equal(cl(2.5)[c("v1", "v2", "v3", "cl2", "cl3")], cl()[c("v1", "v2", "v3", "cl2", "cl3")])
  # the power sees at least MIN_ALBUMIN, so it stays finite and positive
  expect_equal(cl(0.1)$cl1, cl(MIN_ALBUMIN)$cl1)
  expect_true(is.finite(cl(0)$cl1) && cl(0)$cl1 > 0)
  # getDrugPK passes the Patient Profile's albumin, and a blank (NA) is NULL
  viaPK <- function(alb)
    getDrugPK("methotrexate", 70, 170, 60, "male", adjustToFFM = FALSE,
              creatinine = 0.92, albumin = alb)$PK$default$cl1
  expect_equal(viaPK(3.3), cl(3.3)$cl1)
  expect_equal(viaPK(NA), cl()$cl1)
  expect_error(viaPK(9), "Invalid albumin")
  expect_error(viaPK(NaN), "Invalid albumin")
})

test_that("weight is allometric; the switch takes the pharmacokinetic weight", {
  off <- methotrexate(100, 180, 50, "male", adjustToFFM = FALSE, creatinine = 0.92)$PK$default
  expect_equal_rounded(off$v1, 14.4 * 100 / 70)
  expect_equal_rounded(off$cl2, 0.571 * (100 / 70)^0.75 / 60)
  size <- pkSizeFactors(100, 180, 50, "male", TRUE)
  on <- methotrexate(100, 180, 50, "male", creatinine = 0.92)$PK$default
  expect_equal_rounded(on$v1, 14.4 * size$pkWeight / 70)
  for (args in list(list(2, 45, 0, "female"), list(250, 210, 90, "male"))) {
    m <- do.call(methotrexate, args)$PK$default
    expect_true(all(is.finite(unlist(m))) && m$cl1 > 0 && m$v1 > 0)
  }
})

test_that("methotrexate is a chemotherapy drug, plasma only, with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "methotrexate", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  PK <- getDrugPK("methotrexate", 70, 170, 60, "male")
  expect_equal(PK$tPeak, 0)
  expect_identical(PK$Concentration.Units, "mcg")
  expect_match(PK$reference, "Ibarra")
  expect_identical(therapeuticRange("methotrexate")$band_status, "not_applicable")
})
