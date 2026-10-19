# phenytoin: see the header of R/drugs_phenytoin.R for the Odani 1996
# Michaelis-Menten model, the salt and PE conversions, and the CYP2C9 term.
# The engine itself is tested in test-michaelis-menten.R.  Expected values are
# the published numbers written out, not taken from the code under test.

test_that("the reference patient gets Odani's parameters, switch off", {
  a <- phenytoin(70, 170, 40, "male", adjustToFFM = FALSE)
  s <- 42 * (70 / 42)^0.463               # weight term at total body weight
  expect_equal(a$PK$default$v1, 1.23 * s, tolerance = 1e-8)
  expect_equal(a$michaelisMenten$vmax, 9.80 * s / (24 * 60), tolerance = 1e-8)
  expect_equal(a$michaelisMenten$km, 9.19)
  expect_equal(a$PK$default$cl1, a$michaelisMenten$vmax / a$michaelisMenten$km)
  expect_equal(a$tPeak, 0)
  expect_equal(a$MEAC, 0)
  expect_equal(a$michaelisMenten$saltFactor$IV, 252.27 / 274.25, tolerance = 1e-8)
  expect_equal(a$michaelisMenten$saltFactor$liquid, 1)
  expect_equal(unname(a$michaelisMenten$kConversion), log(2) / 15)
})

test_that("CYP2C9 diplotype lowers Vmax and nothing else", {
  n <- phenytoin(70, 170, 40, "male", adjustToFFM = FALSE, cyp2c9 = "*1/*1")
  i <- phenytoin(70, 170, 40, "male", adjustToFFM = FALSE, cyp2c9 = "*1/*3")
  p <- phenytoin(70, 170, 40, "male", adjustToFFM = FALSE, cyp2c9 = "*3/*3")
  expect_equal(n$michaelisMenten$vmax,
               phenytoin(70, 170, 40, "male", adjustToFFM = FALSE,
                         cyp2c9 = "*1/*2")$michaelisMenten$vmax)
  expect_equal(i$michaelisMenten$vmax, n$michaelisMenten$vmax * 0.67)
  expect_equal(p$michaelisMenten$vmax, n$michaelisMenten$vmax * 0.5)
  expect_equal(i$PK$default$v1, n$PK$default$v1)
  expect_error(phenytoin(70, 170, 40, "male", cyp2c9 = "normal"))
})

test_that("it scales to fat-free mass", {
  a <- phenytoin(120, 170, 50, "male", adjustToFFM = TRUE)
  size <- pkSizeFactors(120, 170, 50, "male", TRUE)
  s <- 42 * (size$pkWeight / 42)^0.463
  expect_equal(a$PK$default$v1, 1.23 * s, tolerance = 1e-8)
})

# The phenytoin-saturable-metabolism scenario exists to show that equal dose
# increments give unequal steady states.  Pin the point it makes, so that a
# change to the model or the engine that flattened it would fail here rather
# than quietly leaving a help page that says the wrong thing.
test_that("equal daily dose steps give unequal steady states", {
  ev <- data.frame(Time = numeric(0), Event = character(0))
  s <- helpScenarioById("phenytoin-saturable-metabolism")
  expect_false(is.null(s))
  p <- s$patient
  out <- simulateDrugsWithCovariates(s$doses, ev, p$weight, p$height, p$age, p$sex,
                                     s$options$maximum, FALSE, cyp2c9 = p$cyp2c9)
  r <- out$phenytoin$results
  r <- r[r$Site == "Plasma", ]
  trough <- function(day) approx(r$Time, r$Y, day * MINS_PER_DAY)$y

  # The dose rate rises by 100 mg/day at each step (200, 300, 400)
  expect_equal(s$doses$Dose, c(200, 300, 400))
  # Each step raises the trough further than the one before
  steps <- diff(c(trough(27), trough(55), trough(90)))
  expect_gt(steps[2], 2 * steps[1])
  # and the published average, Km x R / (Vmax - R), says the same
  a <- phenytoin(p$weight, p$height, p$age, p$sex)
  vmax <- a$michaelisMenten$vmax * MINS_PER_DAY
  css <- function(mgPerDay) {
    R <- mgPerDay * 252.27 / 274.25        # phenytoin sodium capsule to acid
    a$michaelisMenten$km * R / (vmax - R)
  }
  expect_equal(css(c(200, 300, 400)), c(5.01, 10.33, 22.04), tolerance = 1e-3)
  # A first-order drug would give equal steps; the second is more than twice
  # the first, which is the teaching point of the scenario's table
  expect_gt(css(400) - css(300), 2 * (css(300) - css(200)))
})

test_that("pregnancy: Km (total) x 0.48 and Vmax x 1.1, on every route", {
  # A woman of 30 years, 70 kg, 165 cm, fat-free-mass scaling on.  See the
  # PREGNANCY section of R/drugs_phenytoin.R (Tomson 1994).
  on  <- getDrugPK("phenytoin", 70, 165, 30, SEX_FEMALE, pregnant = TRUE)
  off <- getDrugPK("phenytoin", 70, 165, 30, SEX_FEMALE, pregnant = FALSE)
  expect_identical(off, getDrugPK("phenytoin", 70, 165, 30, SEX_FEMALE))
  expect_equal(on$michaelisMenten$km / off$michaelisMenten$km,
               PHENYTOIN_PREGNANCY_KM_FACTOR, tolerance = 1e-6)
  expect_equal(on$michaelisMenten$vmax / off$michaelisMenten$vmax,
               PHENYTOIN_PREGNANCY_VMAX_FACTOR, tolerance = 1e-6)
  expect_equal(PHENYTOIN_PREGNANCY_KM_FACTOR, 0.48)
  expect_equal(PHENYTOIN_PREGNANCY_VMAX_FACTOR, 1.1)
  # The help tables' low-concentration limit follows the block
  expect_equal(on$PK$default$cl1, on$michaelisMenten$vmax / on$michaelisMenten$km,
               tolerance = 1e-12)
  # The block's other fields (fosphenytoin conversion and IM absorption, the
  # salt factors) and the volume and oral absorption are untouched
  for (k in setdiff(names(off$michaelisMenten), c("vmax", "km")))
    expect_identical(on$michaelisMenten[[k]], off$michaelisMenten[[k]], info = k)
  for (k in c("v1", "v2", "v3", "cl2", "cl3", "ka_PO", "bioavailability_PO", "tlag_PO"))
    expect_identical(on$PK$default[[k]], off$PK$default[[k]], info = k)
  for (form in names(off$oralFormulations))
    for (k in c("ka_PO", "bioavailability_PO", "tlag_PO"))
      expect_identical(on$oralFormulations[[form]]$default[[k]],
                       off$oralFormulations[[form]]$default[[k]], info = paste(form, k))
  # Pinned with the flag on
  expect_equal_rounded(59.22801933, on$PK$default$v1)
  expect_equal_rounded(0.3604776696, on$michaelisMenten$vmax)
  expect_equal_rounded(4.4112, on$michaelisMenten$km)
  expect_equal_rounded(0.08171873177, on$PK$default$cl1)
  # The point of the two factors: at the dose rate that holds the total at
  # 15 mg/L outside pregnancy, the total falls to about 38% (Tomson: 39%)
  # while the unbound, km(unbound) x r / (1 - r) with km(unbound) unchanged,
  # stays at about 79% (Tomson: 82%)
  css <- function(mm, R) mm$km * R / (mm$vmax - R)
  R <- off$michaelisMenten$vmax * 15 / (15 + off$michaelisMenten$km)
  expect_equal(css(off$michaelisMenten, R), 15, tolerance = 1e-10)
  expect_equal(css(on$michaelisMenten, R) / 15, 0.380, tolerance = 2e-3)
  unbound <- css(on$michaelisMenten, R) / PHENYTOIN_PREGNANCY_KM_FACTOR / 15
  expect_equal(unbound, 0.792, tolerance = 2e-3)
  expect_match(on$reference, "Tomson")
  expect_no_match(off$reference, "Tomson")
})
