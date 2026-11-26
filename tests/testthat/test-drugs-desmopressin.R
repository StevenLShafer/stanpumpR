# Desmopressin (R/drugs_desmopressin.R): Agerso 2004's disposition with its
# creatinine-clearance terms, Argenti 1997's von Willebrand factor ke0.
# Drafted by Claude Code, 2026-11-26.

events <- data.frame(Time = double(), Event = character())

test_that("Table 1's parameters at a creatinine clearance of 60, switch off", {
  # 60 mL/min/1.73 m^2 is Agerso's reference; find the creatinine that gives it
  # for a 70 kg, 170 cm, 35-year-old man (Cockcroft-Gault on total weight,
  # indexed by Gehan and George's surface area).
  crclAt <- function(scr) creatinineClearanceCG(70, 35, "male", scr) * 1.73 /
    bsaGehanGeorge(70, 170)
  scr60 <- uniroot(function(s) crclAt(s) - 60, c(0.3, 5), tol = 1e-12)$root
  m <- desmopressin(70, 170, 35, "male", adjustToFFM = FALSE,
                    creatinine = scr60)$PK$default
  expect_equal_rounded(13.9, m$v1)
  expect_equal_rounded(12.5, m$v2)
  expect_equal_rounded(11.7, m$v3)
  expect_equal_rounded((2.41 + 3.91) / 60, m$cl1)
  expect_equal_rounded(4.43 / 60, m$cl2)
  expect_equal_rounded(27.5 / 60, m$cl3)
})

test_that("the creatinine-clearance terms are linear and reproduce Table 2", {
  # Table 2's group means: CrCL 103, 72, 37, 16 mL/min; total CL 10, 6.9,
  # 4.9, 2.9 L/h; renal CL 4.9, 2.8, 1.2, 0.65 L/h.
  cl <- sapply(c(103, 72, 37, 16), desmopressinClearance)
  expect_equal(round(colSums(cl), 2), c(9.69, 7.26, 4.52, 2.87))
  expect_equal(round(cl["renal", ], 2), c(4.21, 2.91, 1.45, 0.56))
  expect_equal(unname(desmopressinClearance(60)), c(2.41, 3.91))
  # 1 mL/min below 60 lowers CLr by 1.74% and CLnr by 0.933% (the text)
  r <- desmopressinClearance(59) / desmopressinClearance(60)
  expect_equal(unname(r), c(1 - 0.0174, 1 - 0.00933))
  # held within the studied 10-144 mL/min/1.73 m^2
  expect_equal(desmopressinClearance(2), desmopressinClearance(10))
  expect_equal(desmopressinClearance(300), desmopressinClearance(144))
  expect_true(all(desmopressinClearance(0) > 0))
})

test_that("a raised creatinine lowers clearance and leaves the volumes alone", {
  n <- desmopressin(70, 170, 60, "male", creatinine = 1)$PK$default
  i <- desmopressin(70, 170, 60, "male", creatinine = 4)$PK$default
  expect_lt(i$cl1, n$cl1 / 2)
  expect_equal(i$v1, n$v1)
  expect_equal(i$cl2, n$cl2)
})

test_that("with the switch on, a 120 kg man's parameters are pinned", {
  s <- desmopressin(120, 175, 50, "male", adjustToFFM = TRUE)$PK$default
  expect_equal_rounded(18.743283, s$v1)
  expect_equal_rounded(16.85547, s$v2)
  expect_equal_rounded(15.77672, s$v3)
  expect_equal_rounded(0.20522988, s$cl1)
  expect_equal_rounded(0.092390139, s$cl2)
  expect_equal_rounded(0.57352795, s$cl3)
})

test_that("the effect site is Argenti's von Willebrand factor ke0", {
  out <- desmopressin(70, 170, 35, "male")
  expect_equal(out$ke0, 2.16 / 60)
  expect_null(out$tPeak)
  expect_equal(c(out$MEAC, out$typical, out$lowerTypical, out$upperTypical),
               rep(0, 4))
})

test_that("0.3 mcg/kg over 15 min peaks near Argenti's 1100 pg/mL", {
  # Argenti 1997 (Discussion): mean plasma desmopressin about 1100 pg/mL at
  # the highest point, after 0.3 mcg/kg over 15 min in men of 60-90 kg.
  dose <- data.frame(Drug = "desmopressin", Time = c(0, 15),
                     Dose = c(0.3 * 70 * 4, 0), Units = "mcg/hr")
  e <- simulateDrugsWithCovariates(dose, events, 70, 177, 30, "male", 360,
                                   FALSE)$desmopressin
  peak <- max(e$wide$Plasma)
  expect_gt(peak, 900)
  expect_lt(peak, 1300)
  # the effect site peaks after the infusion ends
  w <- e$wide
  expect_gt(w$Time[which.max(w$`Effect Site`)], 30)
  # % effect on von Willebrand factor at its peak, Argenti's Emax model:
  # about 70% of the maximal rise, i.e. 100 + 267 x 0.7 = 290% of predose,
  # where Argenti saw about 260-290%.
  rows <- pdEffectRows("desmopressin")
  expect_setequal(rows$endpoint, c("vWF release", "FVIII:C release"))
  vwf <- pdEffectSeries(e, rows[rows$endpoint == "vWF release", ])
  top <- max(pdEffectPercent(vwf$C, 237e-6, 0.94))
  expect_gt(top, 55)
  expect_lt(top, 85)
})

test_that("desmopressin is a hemostatic drug in the library", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "desmopressin", ]
  expect_identical(row$Category, "Hemostasis and reversal")
  expect_identical(row$Concentration.Units, "pg")
  expect_true("creatinine" %in% names(formals(desmopressin)))
})
