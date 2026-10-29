# GHB: see the header of R/drugs_GHB.R.  Chen 2020's two-compartment model
# with Michaelis-Menten elimination, rescaled from the source's sodium oxybate
# basis onto the GHB (acid) basis the curve is plotted on.
#
# Two kinds of test here.  The pins fix the arithmetic.  The SOURCE
# REPRODUCTIONS check the model against numbers nobody here chose: the
# paper's own simulated figures, and two independent studies the model was
# not fitted to.  Those are what would catch a basis error, which is this
# drug's characteristic failure.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

# Chen 2020, Table S4, as published (hours and the sodium oxybate basis)
VMAX_SRC <- 1508; KM_SRC <- 22.0; VC <- 29.5; KA <- 4.12
K23 <- 0.0968; K32 <- 0.536; EXP_VC <- 0.650; EXP_VMAX <- 0.633
# GHB 104.10 over sodium oxybate 126.09
ACID <- 104.10 / 126.09

ghbRun <- function(dose, units = "g PO oxybate", times = 0, weight = 70,
                   height = 175, age = 30, sex = "male", maximum = 1440) {
  simulateDrugsWithCovariates(
    data.frame(Drug = "GHB", Time = times, Dose = dose, Units = units),
    noEvents, weight, height, age, sex, maximum, TRUE)$GHB$wide
}
trapz <- function(w) sum(diff(w$Time) *
                         (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2) / 60

test_that("returns the published parameters with the fat-free-mass switch off", {
  actual <- GHB(70, 175, 30, "male", adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 29.5,                 # VC at the reference weight, L
        v2 = 1,
        v3 = 1,
        # Vmax/Km, the low-concentration limit, for the help tables only.  The
        # basis factor cancels in the ratio, so this is 1508/(60 x 22) exactly
        # whichever basis the model is written on.
        cl1 = 1.14242424,
        cl2 = 0,
        cl3 = 0,
        ka_PO = 0.06866667,        # 4.12 /h
        bioavailability_PO = 1,
        tlag_PO = 0
      )
    ),
    tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = actual$reference,
    michaelisMenten = list(
      vmax = 20.75009914,          # mg/min, 1508 mg/h on the acid basis
      km = 18.16321675,            # mg/L, 22.0 on the acid basis
      k23 = 0.0016133333,          # 0.0968 /h
      k32 = 0.0089333333,          # 0.536 /h
      saltFactor = list(default = 0.82560076)
    )
  )
  expect_equal_rounded(actual, expected)
})

test_that("the basis change is exactly the molecular weight ratio, and nothing else", {
  x <- GHB(70, 175, 30, "male", adjustToFFM = FALSE)
  # Both masses rescaled by the same factor; volume and rates untouched.
  expect_equal(x$michaelisMenten$vmax * 60, VMAX_SRC * ACID)
  expect_equal(x$michaelisMenten$km, KM_SRC * ACID)
  expect_equal(x$michaelisMenten$saltFactor$default, ACID)
  expect_equal(x$PK$default$v1, VC)
  expect_equal(x$michaelisMenten$k23 * 60, K23)
  expect_equal(x$michaelisMenten$k32 * 60, K32)
  expect_equal(x$PK$default$ka_PO * 60, KA)
  # Because dose, Vmax and Km all carry the same factor, the whole curve is
  # the source's own curve times that factor: a dose entered as the salt and
  # read as the acid is self-consistent.
  expect_equal(x$michaelisMenten$vmax / x$michaelisMenten$km,
               VMAX_SRC / KM_SRC / 60)
})

test_that("the peripheral volume is the one the source derives from k23/k32", {
  x <- GHB(70, 175, 30, "male", adjustToFFM = FALSE)
  vp <- x$michaelisMenten$k23 / x$michaelisMenten$k32 * x$PK$default$v1
  expect_equal(vp, 5.33, tolerance = 1e-2)   # Table S4 footnote a: 5.33 L
})

test_that("it reproduces the paper's own simulated adult peak", {
  # Chen Figure 4: adults given 4.5 g twice nightly, four hours apart, fasted,
  # have a simulated Cmax after the second dose of about 115-120 mcg/mL ON THE
  # SODIUM OXYBATE BASIS.  Converting back is how a basis slip would show.
  w <- ghbRun(c(4.5, 4.5), times = c(0, 240))
  cmax2 <- max(w$Plasma[w$Time >= 240])
  expect_gt(cmax2 / ACID, 110)
  expect_lt(cmax2 / ACID, 125)
  # The second peak is higher than the first: saturable elimination carrying
  # drug over, which is the behaviour the engine exists for.
  expect_gt(cmax2, max(w$Plasma[w$Time < 240]))
})

test_that("it reproduces Liechti's independent healthy volunteers", {
  # Liechti 2016, Br J Clin Pharmacol: 32 healthy men, mean 74.5 kg, single
  # oral doses, concentrations reported in nmol/mL so on the ACID basis.
  # Nothing in this study was used to fit the model.
  for (arm in list(list(mgkg = 20, cmax = 218), list(mgkg = 35, cmax = 453))) {
    w <- ghbRun(arm$mgkg * 74.5, "mg PO oxybate", weight = 74.5, age = 25,
                maximum = 400)
    observed <- arm$cmax * 104.10 / 1000          # nmol/mL -> mcg/mL as GHB
    expect_equal(max(w$Plasma), observed, tolerance = 0.15)
    # Peak inside the observed median range of 35 min (range 35-60)
    expect_gt(w$Time[which.max(w$Plasma)], 15)
    expect_lt(w$Time[which.max(w$Plasma)], 60)
  }
})

test_that("exposure rises faster than the dose, as the drug does", {
  a <- trapz(ghbRun(2.25))
  b <- trapz(ghbRun(4.5))
  # Doubling the dose more than doubles the area: the Xyrem label reports a
  # 3.7-fold rise for a 2-fold dose, and this model gives about 3.2.
  expect_gt(b / a, 2.5)
  expect_lt(b / a, 4)
  # And it is NOT linear, which is the whole reason for the engine
  expect_false(isTRUE(all.equal(b, 2 * a, tolerance = 0.05)))
})

test_that("the terminal half-life is under an hour", {
  # The paper: "a short elimination half-life of < 1 hour".  Measured on the
  # late log-linear stretch after a small dose, where C has fallen below Km.
  w <- ghbRun(1, maximum = 400)
  late <- w$Time >= 120 & w$Time <= 200 & w$Plasma > 0
  slope <- stats::coef(stats::lm(log(w$Plasma[late]) ~ w$Time[late]))[2]
  halfLife <- -log(2) / slope
  expect_gt(halfLife, 10)
  expect_lt(halfLife, 60)
})

test_that("the source's own weight exponents are carried, not the package's", {
  for (wt in c(50, 70, 120)) {
    x <- GHB(wt, 170, 40, "female", adjustToFFM = FALSE)
    expect_equal(x$PK$default$v1, VC * (wt / 70)^EXP_VC)
    expect_equal(x$michaelisMenten$vmax * 60, VMAX_SRC * ACID * (wt / 70)^EXP_VMAX)
    # Km is a concentration and carries no weight term
    expect_equal(x$michaelisMenten$km, KM_SRC * ACID)
  }
  # With the switch on, the same exponents at the fat-free-mass weight
  size <- pkSizeFactors(120, 170, 50, "male", TRUE)
  x <- GHB(120, 170, 50, "male")
  expect_equal(x$PK$default$v1, VC * (size$pkWeight / 70)^EXP_VC)
})

test_that("the source's categorical age effect on VC is carried, exponentially", {
  base <- function(wt) VC * (wt / 70)^EXP_VC
  adult <- GHB(40, 150, 30, "male", adjustToFFM = FALSE)$PK$default$v1
  teen  <- GHB(40, 150, 14, "male", adjustToFFM = FALSE)$PK$default$v1
  child <- GHB(40, 150, 9, "male", adjustToFFM = FALSE)$PK$default$v1
  expect_equal(adult, base(40))
  expect_equal(teen / base(40), exp(0.201))
  expect_equal(child / base(40), exp(0.201 - 0.328))
  # These are the paper's own "22% increase" and "12% reduction", which the
  # proportional reading (1.201 and 0.873) would not give.  The difference
  # between the two readings is under 2%, and this pins which one is carried.
  expect_equal(teen / base(40), 1.22, tolerance = 0.01)
  expect_equal(child / base(40), 0.88, tolerance = 0.01)
  # It steps at the birthdays rather than varying smoothly, and below 7 and
  # from 18 the adult value is used.
  expect_equal(GHB(40, 150, 18, "male", adjustToFFM = FALSE)$PK$default$v1, base(40))
  expect_equal(GHB(40, 150, 5, "male", adjustToFFM = FALSE)$PK$default$v1, base(40))
})

test_that("the assumed 70 kg allometric reference is worth under 3%", {
  # The source does not print the weight it normalised to.  70 kg is assumed;
  # the dataset's median was 72.1 kg.  This pins how much that could matter,
  # so the assumption cannot be forgotten: both well inside the bootstrap
  # interval of Vmax alone, which spans about +/- 11%.
  expect_lt(abs(1 - (70 / 72.1)^EXP_VC), 0.03)
  expect_lt(abs(1 - (70 / 72.1)^EXP_VMAX), 0.03)
})

test_that("GHB is offered orally only, in named salt units, plasma only, no band", {
  dd <- getDrugDefaultsGlobal(FALSE)
  row <- dd[dd$Drug == "GHB", ]
  units <- strsplit(row$Units, ",")[[1]]
  expect_setequal(doseRoute(units), ROUTE_PO)
  expect_setequal(units, c("g PO oxybate", "mg PO oxybate"))
  # Every unit names the salt that is swallowed, because the acid and the salt
  # differ by 21% and both are in common use.
  expect_true(all(grepl("oxybate", units)))
  # The product word must not be read as an oral formulation
  expect_true(all(is.na(doseFormulation(units))))
  expect_true(is.na(row$Bolus.Units))
  expect_true(is.na(row$Infusion.Units))
  expect_equal(row$MEAC, 0)
  expect_equal(row$endCe, 0)
  expect_equal(c(row$Lower, row$Upper, row$Typical), c(0, 0, 0))
  expect_equal(row$Category, ILLICIT_DRUG_CATEGORY)
  expect_equal(getDrugPK("GHB", 70, 175, 30, "male")$PK$default$ke0, 0)
})

test_that("a gram and a milligram dose agree, and the salt factor is applied", {
  # "g" is anchored in the dose-scaling grep, so a unit ending in a product
  # word must still scale: 1 g must equal 1000 mg exactly.
  expect_equal(ghbRun(1, "g PO oxybate")$Plasma,
               ghbRun(1000, "mg PO oxybate")$Plasma)
  # And the modelled moiety is the acid: the amount entering is the dose
  # times the molecular weight ratio, so the curve sits below what the same
  # number of milligrams of acid would give.
  w <- ghbRun(4.5)
  expect_lt(max(w$Plasma), 4500 / 29.5)        # below the undistributed limit
  expect_gt(max(w$Plasma), 0)
})

test_that("the dose table accepts the salt-named units", {
  # simulateDrugsWithCovariates() bypasses the dose-table validator, so a unit
  # the app would reject can still pass every test above.  This is the check
  # that the app itself would take the dose: a unit missing from allUnits
  # raises "Dose table contains unknown dose units".
  expect_true(all(ghbUnits %in% allUnits))
  DT <- data.frame(Drug = "GHB", Time = c("0", "240"), Dose = c("4.5", "4500"),
                   Units = c("g PO oxybate", "mg PO oxybate"),
                   stringsAsFactors = FALSE)
  expect_silent(validateDoseTableInput(DT))
  # and the units the CSV offers are exactly the ones named here
  dd <- getDrugDefaultsGlobal(FALSE)
  expect_setequal(strsplit(dd$Units[dd$Drug == "GHB"], ",")[[1]], ghbUnits)
})
