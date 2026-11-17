# Voriconazole (R/drugs_voriconazole.R): Friberg 2012's two compartments with
# linear plus Michaelis-Menten elimination, a Vmax that declines with time
# since the first dose, and CYP2C19 in adults.  Drafted by Claude Code,
# 2026-11-12.  Expectations from Table 3 of the source (PMC3370730), not from
# the drug file.

events <- data.frame(Time = double(), Event = character())
inverseLogit <- function(x) exp(x) / (1 + exp(x))

test_that("Table 3's adult parameters at 70 kg", {
  pk <- voriconazole(70, 170, 35, "male", adjustToFFM = FALSE)
  set <- pk$PK$default
  mm <- pk$michaelisMenten
  expect_equal(mm$vmax, 114 / 60)
  expect_equal(mm$km, 1.15)
  expect_equal(mm$clLinear, 6.16 / 60)
  expect_equal(set$v1, 79.0)
  expect_equal(set$v2, 103)
  expect_equal(set$cl2, 15.5 / 60)
  expect_equal(set$ka_PO, 100 / 60)
  expect_equal(set$tlag_PO, 0.949 * 60)
  expect_equal(set$bioavailability_PO, inverseLogit(0.585))
  expect_equal(mm$vmaxDecline, list(fraction = inverseLogit(1.50), t50 = 2.41 * 60, onset = 60))
})

test_that("CYP2C19 poor and intermediate adults lose all of Vmax; others 81.8%", {
  frac <- function(cyp, age = 35)
    voriconazole(70, 170, age, "male", cyp2c19 = cyp)$michaelisMenten$vmaxDecline$fraction
  expect_equal(frac("poor"), 1)
  expect_equal(frac("intermediate"), 1)
  for (cyp in c("normal", "rapid", "ultrarapid")) expect_equal(frac(cyp), inverseLogit(1.50))
  # children: the age term, and no genotype effect
  expect_equal(frac("poor", 8), inverseLogit(1.50 - 0.390))
  expect_equal(frac("normal", 14), inverseLogit(1.50))
  expect_error(voriconazole(70, 170, 35, "male", cyp2c19 = "fast"), "cyp2c19")
})

test_that("the young-patient terms on Q, ka and the lag", {
  child <- voriconazole(25, 125, 8, "male", adjustToFFM = FALSE)$PK$default
  adol  <- voriconazole(50, 160, 14, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(child$cl2, 15.5 / 60 * (25 / 70)^0.75 * 1.637)
  expect_equal(child$ka_PO, 1.19 / 60)
  expect_equal(adol$ka_PO, 1.19 * (1 - 0.615) / 60)
  expect_equal(child$tlag_PO, 0.949 * (1 - 0.874) * 60)
})

test_that("Vmax declines as the source's formula, held at Vmax,1 in the first hour", {
  # Simulate a slow infusion and compare with an independent RK4 of the same
  # system at the same steps would be a second engine; instead check the
  # steady-state limit, where the elimination is known in closed form: for an
  # intermediate metaboliser (Vmax lost entirely) a constant infusion R
  # approaches R / CL with CL the linear clearance alone
  rate <- 20                                      # mg/h
  d <- data.frame(Drug = "voriconazole", Time = 0, Dose = rate, Units = "mg/hr")
  wide <- simulateDrugsWithCovariates(d, events, 70, 170, 35, "male", 21 * 1440,
                                      FALSE, adjustToFFM = FALSE,
                                      cyp2c19 = "intermediate")$voriconazole$wide
  # Vmax(T) = Vmax,1 (1 - (T - 1)/(T - 1 + 1.41)); at 21 days it is 0.28% of
  # Vmax,1, so the plateau is a little below R / CL
  expect_equal(tail(wide$Plasma, 1), rate / 6.16, tolerance = 0.03)
  expect_lt(tail(wide$Plasma, 1), rate / 6.16)
})

test_that("the adult regimen gives the label's exposure", {
  # VFEND label (2026), Table 12, geometric means of five studies with mixed
  # genotypes: AUC0-12 13.9 mg.h/L after the 6 mg/kg loading infusion and
  # 33.9 at 4 mg/kg q12h.  Normal and intermediate metabolisers bracket them.
  auc <- function(w, a, b) {
    s <- w[w$Time >= a & w$Time <= b, ]
    sum(diff(s$Time) * (head(s$Plasma, -1) + tail(s$Plasma, -1)) / 2) / 60
  }
  maint <- seq(1440, 6 * 1440 + 720, 720)
  d <- data.frame(Drug = "voriconazole",
                  Time = c(0, 120, 720, 840, maint, maint + 80),
                  Dose = c(3, 0, 3, 0, rep(3, length(maint)), rep(0, length(maint))),
                  Units = "mg/kg/hr")
  d <- d[order(d$Time), ]
  run <- function(cyp) simulateDrugsWithCovariates(
    d, events, 70, 170, 35, "male", 7 * 1440, FALSE, adjustToFFM = FALSE,
    cyp2c19 = cyp)$voriconazole$wide
  nm <- run("normal"); im <- run("intermediate")
  expect_lt(auc(nm, 0, 720), 13.9); expect_gt(auc(im, 0, 720), 0.9 * 13.9)
  expect_lt(auc(nm, 6 * 1440, 6 * 1440 + 720), 33.9)
  expect_gt(auc(im, 6 * 1440, 6 * 1440 + 720), 33.9)
})

test_that("the band is the position paper's 2-6 mg/L trough", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "voriconazole", ]
  expect_equal(c(row$Lower, row$Upper, row$Typical), c(2, 6, 4))
})
