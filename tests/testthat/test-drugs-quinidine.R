# Quinidine (R/drugs_quinidine.R): Fattinger 1991's two compartments with
# zero-order absorption, dosed as the sulfate.  Drafted by Claude Code,
# 2026-11-12.  Expectations from Table 1 and the text of the source, not
# from the drug file.

events <- data.frame(Time = double(), Event = character())

test_that("Table 1 and the paper's half-lives at a creatinine clearance of 100", {
  # the source's own creatinine clearance: (150 - age) x weight / Scr(umol/L),
  # + 10% in men; 100 mL/min at 65 years, 70 kg
  scr <- (150 - 65) * 70 * 1.1 / (100 * 88.4)
  p <- quinidine(70, 170, 65, "male", adjustToFFM = FALSE, creatinine = scr)$PK$default
  expect_equal(p$cl1 * 60, 12.6 + 0.0566 * 100)
  expect_equal(c(p$v1, p$v2, p$cl2 * 60), c(161, 66.7, 12.6))
  k10 <- p$cl1 / p$v1; k12 <- p$cl2 / p$v1; k21 <- p$cl2 / p$v2
  A <- matrix(c(-(k10 + k12), k21, k12, -k21), 2, byrow = TRUE)
  # "the two half-lives ... were 2.22 h and 10.1 h and the average oral
  # clearance was 18.1 l h-1"
  expect_equal(sort(log(2) / -eigen(A)$values / 60), c(2.22, 10.1), tolerance = 0.01)
  expect_equal(p$cl1 * 60, 18.1, tolerance = 0.01)
  # women: 10% subtracted rather than added
  w <- quinidine(70, 170, 65, "female", adjustToFFM = FALSE, creatinine = scr)$PK$default
  expect_equal(w$cl1 * 60, 12.6 + 0.0566 * 100 * 0.9 / 1.1)
})

test_that("a sulfate dose is converted at 0.829 and absorbed over 1.37 h", {
  expect_true(all(quinidineSulfateUnits %in% allUnits))
  expect_true(all(paste(quinidineSulfateUnits, c("tid", "qid", "q4h")) %in% scheduledUnits))
  d <- data.frame(Drug = "quinidine", Time = 0, Dose = 200, Units = "mg PO sulfate")
  # six days, so the terminal tail (half-life 10.6 h) has gone
  w <- simulateDrugsWithCovariates(d, events, 70, 170, 35, "male", 6 * 1440,
                                   FALSE)$quinidine$wide
  auc <- sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2) / 60
  cl <- quinidine(70, 170, 35, "male")$PK$default$cl1 * 60
  expect_equal(auc, 200 * 0.829 / cl, tolerance = 0.005)
  # the zero-order input ends, and the concentration peaks, at 1.37 h
  expect_equal(w$Time[which.max(w$Plasma)], 1.37 * 60, tolerance = 0.01)
  # the same with the schedule
  d <- data.frame(Drug = "quinidine", Time = 0, Dose = 300, Units = "mg PO sulfate qid")
  w <- simulateDrugsWithCovariates(d, events, 70, 170, 65, "male", 4 * 1440,
                                   FALSE)$quinidine$wide
  s <- w[w$Time >= 3 * 1440, ]
  expect_gt(min(s$Plasma), 1.5); expect_lt(max(s$Plasma), 6)
})

test_that("anchored at Fattinger's cohort: its typical patients get Table 1, FFM scales the rest", {
  # (a) Results (PDF p2): 46 men and 14 women, 65.5 years, 70.5 kg; heights
  # from NCD-RisC (Switzerland, born 1926).  No parameter has a sex term
  # (the creatinine clearance's 10% is the renal term's), so the cohort's man
  # and woman pooled geometrically receive Table 1's values
  kc <- cohortFactorsIndependent(QUINIDINE_COHORT)
  expect_equal(round(c(kc$volume, kc$clearance), 4), c(0.9481, 0.9608))
  m <- quinidine(70.5, 170.6, 65.5, "male", creatinine = 1e9)$PK$default
  f <- quinidine(70.5, 159.2, 65.5, "female", creatinine = 1e9)$PK$default
  pooled <- function(x, y) exp((46 * log(x) + 14 * log(y)) / 60)
  expect_equal(pooled(m$v1, f$v1), 161)
  expect_equal(pooled(m$v2, f$v2), 66.7)
  expect_equal(pooled(m$cl2, f$cl2) * 60, 12.6)
  # with no renal function the clearance is the anchored nonrenal 12.6 L/h
  expect_equal(pooled(m$cl1, f$cl1) * 60, 12.6, tolerance = 1e-6)
  # (b) twice the reference fat-free mass: volumes x 2, the nonrenal
  # clearance and Q x 2^0.75, over the cohort's factors; the renal term at
  # the pharmacokinetic weight, 140 kg, unanchored
  w <- uniroot(function(w) ffmAlSallami(w, 200, 65, "male") - 2 * FFM_REFERENCE,
               c(70, 400), tol = 1e-12)$root
  big <- quinidine(w, 200, 65, "male", creatinine = 1)$PK$default
  expect_equal(c(big$v1, big$v2), c(161, 66.7) * 2 / kc$volume)
  expect_equal(big$cl2 * 60, 12.6 * 2^0.75 / kc$clearance)
  crcl <- (150 - 65) * 140 / 88.4 * 1.1
  expect_equal(big$cl1 * 60, 12.6 * 2^0.75 / kc$clearance + 0.0566 * crcl)
  # (c) the switch off: Table 1 at any size, CLcr at total weight
  off <- quinidine(w, 200, 65, "male", adjustToFFM = FALSE, creatinine = 1)$PK$default
  expect_equal(c(off$v1, off$v2, off$cl2 * 60), c(161, 66.7, 12.6))
  expect_equal(off$cl1 * 60, 12.6 + 0.0566 * (150 - 65) * w / 88.4 * 1.1)
})

test_that("the band is the label's 2-6 mg/L", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "quinidine", ]
  expect_equal(c(row$Lower, row$Upper, row$Typical), c(2, 6, 4))
})
