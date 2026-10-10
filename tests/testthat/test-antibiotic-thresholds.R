# The antibiotics' "time until threshold": free drug at the MIC.
#
# antibioticMicTable() (R/antibioticThresholds.R) holds, per antibiotic, the
# organism, the MIC, what the model plots, the free fraction and the resulting
# threshold.  These tests keep the shipped defaults, the help and the engines
# in step with it.
#
# (Claude Code, 2026-10-07, at the request of Steven L. Shafer; run on R 4.3.3.)

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("each antibiotic's default threshold is free drug at its MIC", {
  tab <- antibioticMicTable()
  dd  <- getDrugDefaultsGlobal()
  expect_setequal(tab$Drug, c("cefazolin", "cefalexin", "ceftriaxone", "clindamycin",
                              "gentamicin", "metronidazole", "vancomycin"))
  for (i in seq_len(nrow(tab))) {
    d <- tab$Drug[i]
    # The CSV ships the table's threshold
    expect_equal(dd$endCe[dd$Drug == d], tab$Threshold[i], label = paste(d, "endCe"))
    # The threshold is where free drug equals the MIC, to the rounding the
    # threshold is stored at
    expect_equal(tab$Threshold[i] * tab$FreeFraction[i], tab$MIC[i], tolerance = 0.06,
                 label = paste(d, "free concentration at the threshold"))
    # An unbound curve is compared with the MIC directly
    if (tab$Plotted[i] == "unbound") {
      expect_equal(tab$FreeFraction[i], 1)
      expect_equal(tab$Threshold[i], tab$MIC[i])
    } else {
      expect_lte(tab$FreeFraction[i], 1)
      expect_gte(tab$Threshold[i], tab$MIC[i])
    }
    expect_true(tab$Plotted[i] %in% c("unbound", "total"))
    expect_true(nzchar(tab$MicSource[i]) && nzchar(tab$BindingSource[i]))
    expect_false(grepl("PROVISIONAL", paste(tab$MicSource[i], tab$BindingSource[i])))
  }
})


test_that("the antibiotics, amiodarone, zolpidem, temazepam and ketorolac are exactly the drugs with no effect site and a threshold", {
  # A policy pin.  Until 2026-10-07 only the antibiotics were timed on their
  # plasma by default.  Amiodarone joined deliberately: its threshold is the
  # bottom of the therapeutic window, 1.0 mg/L of serum amiodarone, and it is
  # not an antibiotic, so no MIC, free fraction or antibiotic help applies to
  # it (R/drugs_amiodarone.R).  Zolpidem joined on 2026-10-09: its published
  # effects are direct functions of plasma concentration, and its threshold is
  # the FDA's 50 ng/mL driving level (R/drugs_zolpidem.R).  Temazepam joined
  # the same day, with no equilibration delay and a threshold of 250 ng/mL,
  # above which psychometric performance deteriorated (R/drugs_temazepam.R).
  # Ketorolac joined on 2026-10-10: no published ke0, and its threshold is
  # the adult analgesic EC50 of 0.37 mg/L racemate in plasma that Cloesmeijer
  # 2021 cite (R/drugs_ketorolac.R).
  dd <- getDrugDefaultsGlobal()
  timedOnPlasma <- character(0)
  for (drug in dd$Drug[!isGasDrug(dd$Drug)]) {
    PK <- getDrugPK(drug, 70, 170, 50, "male", dd[dd$Drug == drug, ])
    if (PK$PK[[PK_EVENT_DEFAULT]]$ke0 == 0 && dd$endCe[dd$Drug == drug] > 0)
      timedOnPlasma <- c(timedOnPlasma, drug)
  }
  expect_setequal(timedOnPlasma,
                  c(antibioticMicTable()$Drug, "amiodarone", "zolpidem", "temazepam",
                    "ketorolac"))
  expect_false(any(c("amiodarone", "zolpidem", "temazepam", "ketorolac") %in%
                     antibioticMicTable()$Drug))
  # Acute intravenous amiodarone (2026-10-08) deliberately has no default
  # threshold: the window is for chronic troughs (R/drugs_amiodaroneIV.R).
  expect_false("amiodaroneIV" %in% timedOnPlasma)
  amio <- helpDrugPageHTML("amiodarone")
  expect_false(grepl("Time until threshold: free drug at the MIC", amio, fixed = TRUE))
})


test_that("the time until threshold is the time until the plotted curve reaches it", {
  # With the shipped default, straight from the library: a usual dose.  Stop
  # giving drug, run the simulation on to the time the line reports, and the
  # plasma must be AT the threshold there and below it from then on.  (Run to
  # that instant rather than interpolated: the engine's time line spaces its
  # points out after a day, too far apart to read a crossing off.)
  dd <- getDrugDefaultsGlobal()
  doses <- list(cefazolin = c(2000, "mg"), ceftriaxone = c(2000, "mg"),
                vancomycin = c(1500, "mg"), gentamicin = c(350, "mg"),
                clindamycin = c(900, "mg"), metronidazole = c(500, "mg"),
                cefalexin = c(500, "mg PO"))
  for (drug in names(doses)) {
    PK <- getDrugPK(drug, 70, 170, 50, "male", dd[dd$Drug == drug, ])
    PK$endCe <- dd$endCe[dd$Drug == drug]
    DT <- data.frame(Drug = drug, Time = 0, Dose = as.numeric(doses[[drug]][1]),
                     Units = doses[[drug]][2])
    w <- simCpCe(DT, noEvents, PK, 240, TRUE)$wide
    expect_gt(max(w$Plasma), PK$endCe, label = paste(drug, "peak"))
    # At an hour, and at the peak
    for (t in unique(c(60, w$Time[which.max(w$Plasma)]))) {
      k <- which.min(abs(w$Time - t))
      rec <- w$Recovery[k]
      expect_gt(rec, 0, label = paste(drug, "time until threshold at", round(w$Time[k], 1)))
      expect_lt(rec, MINS_PER_WEEK)
      at <- function(u) {
        r <- simCpCe(DT, noEvents, PK, u, FALSE)$wide
        r$Plasma[nrow(r)]
      }
      expect_equal(at(w$Time[k] + rec), PK$endCe, tolerance = 1e-3,
                   label = paste(drug, "plasma when the line says it reaches the threshold"))
      expect_lt(at(w$Time[k] + rec + 30), PK$endCe)
    }
  }
})


test_that("each antibiotic's help explains that the threshold is free drug at the MIC", {
  tab <- antibioticMicTable()
  for (i in seq_len(nrow(tab))) {
    html <- helpDrugPageHTML(tab$Drug[i])
    expect_match(html, "Time until threshold: free drug at the MIC", fixed = TRUE)
    expect_match(html, if (tab$Plotted[i] == "unbound") "plots <strong>unbound</strong>"
                       else "plots <strong>total</strong>", fixed = TRUE)
    expect_match(html, sprintf("%s mg/L", helpFormatNumber(tab$MIC[i])), fixed = TRUE)
  }
  # And the general page carries the same numbers
  md <- helpReadMarkdown("models/recovery")
  for (i in seq_len(nrow(tab))) {
    row <- grep(paste0("^\\| ", tools::toTitleCase(tab$Drug[i]), " \\|"), strsplit(md, "\n")[[1]],
                value = TRUE)
    expect_length(row, 1)
    # The threshold is the last column
    expect_true(endsWith(row, paste0("| ", helpFormatNumber(tab$Threshold[i]), " |")),
                label = paste(tab$Drug[i], "threshold in the table in models/recovery.md"))
    # and the MIC the third
    expect_equal(trimws(strsplit(row, "|", fixed = TRUE)[[1]][4]), helpFormatNumber(tab$MIC[i]),
                 label = paste(tab$Drug[i], "MIC in the table in models/recovery.md"))
  }
})


test_that("a drug timed on its plasma looks a week ahead, one timed on its effect site a day", {
  # Chosen by Steven L. Shafer, 2026-10-07: an antibiotic's time above the
  # MIC commonly runs past a day, a recovery time past a day needs no more
  # precision than "more than a day".
  expect_equal(recoveryCalc(10, log(2) / 1200, 1), MINS_PER_DAY)           # capped
  expect_equal(recoveryCalc(10, log(2) / 1200, 1, MINS_PER_WEEK),
               1200 * log2(10), tolerance = 1e-4)                           # 66 h, found
  expect_equal(recoveryCalc(10, log(2) / 1200, 1e-9, MINS_PER_WEEK), MINS_PER_WEEK)

  dd <- getDrugDefaultsGlobal()
  pk <- function(drug) {
    PK <- getDrugPK(drug, 70, 170, 50, "male", dd[dd$Drug == drug, ])
    PK$endCe <- dd$endCe[dd$Drug == drug]
    PK
  }
  X <- simCpCe(data.frame(Drug = "vancomycin", Time = 0, Dose = 1500, Units = "mg"),
               noEvents, pk("vancomycin"), 120, TRUE)
  expect_equal(X$recoveryStates$horizon, MINS_PER_WEEK)
  expect_gt(max(X$wide$Recovery), MINS_PER_DAY)
  expect_lt(max(X$wide$Recovery), MINS_PER_WEEK)
  X <- simCpCe(data.frame(Drug = "cefalexin", Time = 0, Dose = 500, Units = "mg PO"),
               noEvents, pk("cefalexin"), 120, TRUE)
  expect_equal(X$recoveryStates$horizon, MINS_PER_WEEK)
  X <- simCpCe(data.frame(Drug = "fentanyl", Time = 0, Dose = 100, Units = "mcg"),
               noEvents, pk("fentanyl"), 120, TRUE)
  expect_equal(X$recoveryStates$horizon, MINS_PER_DAY)
})


test_that("an osmotic agent's threshold is read on its osmolality axis", {
  # Mannitol is plotted as serum osmolality, baseline + fraction x Cp, so a
  # threshold set on that axis is mapped back to a concentration before the
  # engine times it.  (Found by review, 2026-10-07: timing the raw
  # concentration against an osmolality made the line meaningless.)
  dd <- getDrugDefaultsGlobal()
  PK <- getDrugPK("mannitol", 70, 170, 50, "male", dd[dd$Drug == "mannitol", ])
  DT <- data.frame(Drug = "mannitol", Time = 0, Dose = 70, Units = "g")
  base <- simCpCe(DT, noEvents, PK, 600, FALSE)$wide
  thr <- (max(base$Plasma) + PK$osmotic$baseline) / 2
  PK$endCe <- thr
  w <- simCpCe(DT, noEvents, PK, 2 * 1440, TRUE)$wide
  k <- which.max(w$Plasma)
  rec <- w$Recovery[k]
  expect_gt(rec, 0)
  r <- simCpCe(DT, noEvents, PK, w$Time[k] + rec, FALSE)$wide
  expect_equal(r$Plasma[nrow(r)], thr, tolerance = 1e-3)
  # At or below the baseline it can never be reached: no threshold
  PK$endCe <- PK$osmotic$baseline - 1
  expect_true(all(simCpCe(DT, noEvents, PK, 600, TRUE)$wide$Recovery == 0))
})


test_that("no help page carries a placeholder", {
  for (id in c("models/recovery", "glossary", "drug-library", "in-development")) {
    expect_false(grepl("PLACEHOLDER", helpReadMarkdown(id)), label = id)
  }
})
