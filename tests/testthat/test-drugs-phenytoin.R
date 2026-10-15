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
