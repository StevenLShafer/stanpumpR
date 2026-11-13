# Amitriptyline (R/drugs_amitriptyline.R): Koh 2018's two compartments,
# forming the library's nortriptyline.  Drafted by Claude Code, 2026-11-13.
# Expectations from Table 3 of the source, Gupta 1999 and Hart 2026, not
# from the drug file.

events <- data.frame(Time = double(), Event = character())
auc <- function(w) sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2) / 60

test_that("Table 3's apparent parameters and the genotype groups", {
  p <- amitriptyline(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(p$cl1 * 60, 61.5 + 30.6)
  expect_equal(c(p$v1, p$cl2 * 60, p$v2), c(155, 109, 1670))
  expect_equal(p$ka_PO * 60, 0.125)
  expect_equal(p$bioavailability_PO, 277.40 / 313.86)
  cl <- function(c19, d6) amitriptyline(70, 170, 35, "male", adjustToFFM = FALSE,
                                        cyp2c19 = c19, cyp2d6 = d6)$PK$default$cl1 * 60
  expect_equal(cl("poor", "poor"), 17 + 18.8)          # Koh's TPM, 35.8
  expect_equal(cl("poor", "normal"), 17 + 30.6)
  expect_equal(cl("normal", "intermediate"), 61.5 + 18.8)
  expect_equal(cl("ultrarapid", "ultrarapid"), cl("normal", "normal"))
  expect_equal(cl("intermediate", "normal"), cl("normal", "normal"))
  expect_error(amitriptyline(70, 170, 35, "male", cyp2c19 = "fast"), "cyp2c19")
  expect_error(amitriptyline(70, 170, 35, "male", cyp2d6 = "fast"), "cyp2d6")
})

test_that("the release over D1 after the lag is four quarter doses", {
  x <- amitriptyline(70, 170, 35, "male")
  pulses <- x$oralPulses[[ORAL_PULSE_DEFAULT_FORM]]
  expect_equal(pulses$fraction, rep(0.25, 4))
  expect_equal(pulses$delay, (0.414 + 1.13 * c(1, 3, 5, 7) / 8) * 60)
  # against a 400-pulse release, the exact one to this precision
  d <- data.frame(Drug = "amitriptyline", Time = 0, Dose = 25, Units = "mg PO")
  run <- function() simulateDrugsWithCovariates(d, events, 70, 170, 35, "male",
                                                4 * 1440, FALSE)
  coarse <- run()
  original <- amitriptyline
  local_mocked_bindings(amitriptyline = function(...) {
    x <- original(...)
    n <- 400
    x$oralPulses[[1]] <- list(fraction = rep(1 / n, n),
                              delay = (0.414 + 1.13 * (seq_len(n) - 0.5) / n) * 60)
    x
  })
  fine <- run()
  for (m in c("amitriptyline", "nortriptyline")) {
    a <- coarse[[m]]$wide; b <- fine[[m]]$wide
    err <- max(abs(a$Plasma - approx(b$Time, b$Plasma, a$Time)$y), na.rm = TRUE) / max(b$Plasma)
    expect_lt(err, if (m == "amitriptyline") 0.015 else 0.002, label = m)
  }
})

test_that("nortriptyline formation reads the library's nortriptyline clearance", {
  x <- amitriptyline(70, 170, 35, "male")
  clNor <- nortriptyline(70, 170, 35, "male")$PK$default$cl1
  expect_equal(x$metabolite$kFormation * x$PK$default$v1, 61.5 / 58.2 * clNor)
  expect_equal(x$metabolite$mwRatio, 263.38 / 277.40)
  expect_equal(x$metabolite$firstPassFraction, 0)
  # CYP2C19 poor: formation 17 / 61.5 of normal
  pm <- amitriptyline(70, 170, 35, "male", cyp2c19 = "poor")$metabolite$kFormation
  expect_equal(pm / x$metabolite$kFormation, 17 / 61.5, tolerance = 1e-9)
  # a revision of nortriptyline's clearance carries into the formation
  original <- nortriptyline
  local_mocked_bindings(nortriptyline = function(...) {
    y <- original(...); y$PK$default$cl1 <- 2 * y$PK$default$cl1; y
  })
  expect_equal(amitriptyline(70, 170, 35, "male")$metabolite$kFormation,
               2 * x$metabolite$kFormation, tolerance = 1e-9)
})

test_that("Gupta 1999's areas after 75 mg of the hydrochloride", {
  # 14 healthy men, 77.4 kg: amitriptyline AUC 688, nortriptyline 648 ng.h/mL
  d <- data.frame(Drug = "amitriptyline", Time = 0, Dose = 75, Units = "mg PO")
  r <- simulateDrugsWithCovariates(d, events, 77.4, 175, 25, "male", 14 * 1440, FALSE)
  expect_equal(auc(r$amitriptyline$wide), 688, tolerance = 0.05)
  expect_equal(auc(r$nortriptyline$wide), 648, tolerance = 0.06)
})

test_that("Hart 2026's concentration per dose of the nortriptyline formed", {
  # Table 6, 24 h after a once-daily dose: nortriptyline formed 0.376 ng/mL
  # per mg/day
  d <- data.frame(Drug = "amitriptyline", Time = 0, Dose = 100, Units = "mg PO qd")
  r <- simulateDrugsWithCovariates(d, events, 70, 170, 35, "male", 21 * 1440, FALSE)
  w <- r$nortriptyline$wide
  trough <- w$Plasma[which.min(abs(w$Time - 21 * 1440 + 1e-6))]
  expect_equal(trough / 100, 0.376, tolerance = 0.05)
})

test_that("no band: the guideline's range is for the active moiety", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "amitriptyline", ]
  expect_equal(c(row$Lower, row$Upper, row$Typical), c(0, 0, 0))
  tr <- read.csv(system.file("extdata", "therapeuticRanges.csv", package = "stanpumpR"))
  expect_equal(tr$band_status[tr$drug == "amitriptyline"], "sum_not_drawn")
})
