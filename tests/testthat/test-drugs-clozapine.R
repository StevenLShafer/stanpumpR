# Clozapine and norclozapine (R/drugs_clozapine.R, R/drugs_norclozapine.R):
# Li 2012's joint one-compartment models, apparent oral.  Drafted by Claude
# Code, 2026-11-12.  Expectations from Table 2 of the source (PMC4011352).

events <- data.frame(Time = double(), Event = character())

test_that("Table 2's values, with the source's sex term when the switch is off", {
  m <- clozapine(70, 170, 35, "male", adjustToFFM = FALSE)
  f <- clozapine(70, 165, 35, "female", adjustToFFM = FALSE)
  expect_equal(f$PK$default$cl1, 21.9 / 60)
  expect_equal(m$PK$default$cl1, 21.9 * 1.208 / 60)
  expect_equal(m$PK$default$v1, 526)
  expect_equal(m$PK$default$ka_PO, 1.3 / 60)
  mw <- (17 * 12.011 + 17 * 1.008 + 35.45 + 4 * 14.007) /
        (18 * 12.011 + 19 * 1.008 + 35.45 + 4 * 14.007)
  expect_equal(m$metabolite$mwRatio, mw)
  expect_equal(m$metabolite$kFormation, 0.66 * m$PK$default$cl1 / 526)
  nf <- norclozapine(70, 165, 35, "female", adjustToFFM = FALSE)$PK$default
  nm <- norclozapine(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(nf$cl1, 32.7 / 60)
  expect_equal(nm$cl1, 32.7 * 1.242 / 60)
  expect_equal(nm$v1, 624)
})

test_that("the steady-state norclozapine to clozapine ratio is the model's KF CL / CLM", {
  # at steady state the formation rate is KF CL Cp and the metabolite's
  # clearance CLM, so Cm / Cp = KF CL / CLM x (MW ratio), about 0.41 by mass
  # in a man; Hart 2026 Table 7 gives the metabolite-to-parent ratio as
  # 0.45-0.79 in 40 nonsmokers, and Li's own Table 1 means 0.54 / 1.14 =
  # 0.47 (umol/L), so the model sits at the low edge
  dose <- data.frame(Drug = "clozapine", Time = 0, Dose = 150, Units = "mg PO bid")
  res <- simulateDrugsWithCovariates(dose, events, 70, 170, 35, "male", 14 * 1440, FALSE)
  # the ratio of the AVERAGE concentrations over the last day: at a single
  # time the lagging metabolite and the fluctuating parent give another ratio
  avg <- function(w) {
    s <- w[w$Time >= 13 * 1440, ]
    sum(diff(s$Time) * (head(s$Plasma, -1) + tail(s$Plasma, -1)) / 2) / diff(range(s$Time))
  }
  cz <- avg(res$clozapine$wide)
  nc <- avg(res$norclozapine$wide)
  expect_equal(nc / cz, 0.66 * 1.208 * 21.9 / (1.242 * 32.7) *
                 (312.801 / 326.828), tolerance = 0.03)
})

test_that("100 mg twice daily brackets the Clozaril label's steady state", {
  # Clozaril label: average steady-state peak 319 ng/mL (102-771), minimum
  # 122 ng/mL (41-343) at 100 mg twice daily
  dose <- data.frame(Drug = "clozapine", Time = 0, Dose = 100, Units = "mg PO bid")
  w <- simulateDrugsWithCovariates(dose, events, 70, 170, 35, "male", 10 * 1440,
                                   FALSE)$clozapine$wide
  last <- w[w$Time > 9 * 1440, ]
  expect_true(max(last$Plasma) > 102 && max(last$Plasma) < 771)
  expect_true(min(last$Plasma) > 41 && min(last$Plasma) < 343)
})

test_that("the band is the AGNP 2026 range", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "clozapine", ]
  expect_equal(c(row$Lower, row$Upper, row$Typical), c(350, 600, 475))
  expect_true(is.na(dd$Category[dd$Drug == "norclozapine"]) ||
                dd$Category[dd$Drug == "norclozapine"] == "")
})
