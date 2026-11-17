# Pegylated liposomal doxorubicin (R/drugs_doxorubicinLiposomal.R) and the
# free doxorubicin it releases (R/drugs_doxorubicinLiposomalFree.R): Hsu 2018.
# Drafted by Claude Code, 2026-11-16.

noEvents <- data.frame(Time = double(), Event = character())

simulate <- function(mg = 90, hours = 14 * 24) {
  dose <- data.frame(Drug = "doxorubicinLiposomal", Time = c(0, 60), Dose = c(mg, 0),
                     Units = "mg/hr")
  simulateDrugsWithCovariates(dose, noEvents, 60, 165, 55, "female", hours * 60, FALSE)
}

test_that("Table I, with the encapsulated 98% written exactly", {
  X <- doxorubicinLiposomal(60, 165, 55, "female")
  d <- X$PK$default; p <- DOXORUBICIN_LIPOSOMAL_HSU
  expect_equal(d$cl1 / d$v1 * 60, 0.0212 / 2.52)               # k10 = Rel / V1
  expect_equal(log(2) / (d$cl1 / d$v1) / 60, 82.4, tolerance = 0.01)  # release half-life, h
  expect_equal(X$metabolite$kFormation * 60, 0.98 * 0.0212 / 2.52)
  expect_equal(X$metabolite$directFraction, 0.02)
  f <- doxorubicinLiposomalFree(60, 165, 55, "female")$PK$default
  expect_equal(c(f$v1, f$v2, f$cl1 * 60, f$cl2 * 60), c(9.52, 35.1, 2.68, 2.87))
})

test_that("the curves match an independent integration of Hsu's four states", {
  p <- DOXORUBICIN_LIPOSOMAL_HSU
  k0 <- 90                       # mg/h, set per interval below
  rhs <- function(t, a) {        # a = encapsulated, free central, free peripheral (mg); h
    rel <- p$rel * a[1] / p$v1
    c(0.98 * k0 - rel,
      0.02 * k0 + rel - p$clFree * a[2] / p$v2 - p$q * (a[2] / p$v2 - a[3] / p$v3),
      p$q * (a[2] / p$v2 - a[3] / p$v3))
  }
  # exact steps per interval, the infusion on or off for a whole interval (a
  # step that evaluates it at its own end loses a sixth of that step's input)
  a <- c(0, 0, 0); t <- 0; times <- c(0.5, 1, 6, 48, 168); ref <- NULL
  for (stop in times) {
    k0 <- if (stop <= 1) 90 else 0   # the infusion is on over whole intervals only
    n <- round((stop - t) / 0.01); h <- (stop - t) / n
    for (i in seq_len(n)) {
      k1 <- rhs(t, a); k2 <- rhs(t + h / 2, a + h / 2 * k1)
      k3 <- rhs(t + h / 2, a + h / 2 * k2); k4 <- rhs(t + h, a + h * k3)
      a <- a + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4); t <- t + h
    }
    t <- stop
    ref <- rbind(ref, c(a[1] / p$v1, a[2] / p$v2 * 1000))    # mcg/mL, ng/mL
  }
  r <- simulate(hours = 200)
  enc <- approx(r$doxorubicinLiposomal$wide$Time, r$doxorubicinLiposomal$wide$Plasma, times * 60)$y
  free <- approx(r$doxorubicinLiposomalFree$wide$Time, r$doxorubicinLiposomalFree$wide$Plasma,
                 times * 60)$y
  expect_equal(enc, ref[, 1], tolerance = 1e-4)
  expect_equal(free, ref[, 2], tolerance = 1e-4)
})

test_that("the unencapsulated 2% shows in free doxorubicin during the infusion", {
  r <- simulate(hours = 48)
  w <- r$doxorubicinLiposomalFree$wide
  early <- approx(w$Time, w$Plasma, 60)$y                      # end of infusion, ng/mL
  # release alone over the first hour gives far less than the direct 2%
  expect_gt(early, 100)
})

test_that("free doxorubicin is about 1% of the encapsulated over the course", {
  r <- simulate()
  auc <- function(w) sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2)
  ratio <- auc(r$doxorubicinLiposomalFree$wide) / 1000 / auc(r$doxorubicinLiposomal$wide)
  expect_true(ratio > 0.005 && ratio < 0.015)
  # areas to infinity: 0.98 x dose / Rel and dose / CLfree (h), 94% reached in 14 days
  expect_equal(auc(r$doxorubicinLiposomal$wide) / 60, 0.98 * 90 / 0.0212, tolerance = 0.07)
})

test_that("a chemotherapy drug of its own; its free doxorubicin is not offered", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "doxorubicinLiposomal", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  free <- dd[dd$Drug == "doxorubicinLiposomalFree", ]
  expect_true(is.na(free$Category) || free$Category == "")
  expect_identical(free$Concentration.Units, "ng")
  expect_match(getDrugPK("doxorubicinLiposomal", 60, 165, 55, "female")$reference, "Hsu")
  expect_true("doxorubicinLiposomal" %in% METABOLITE_FORMATION_BASIS$parent)
})
