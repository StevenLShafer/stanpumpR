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

test_that("the band is the label's 2-6 mg/L", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "quinidine", ]
  expect_equal(c(row$Lower, row$Upper, row$Typical), c(2, 6, 4))
})
