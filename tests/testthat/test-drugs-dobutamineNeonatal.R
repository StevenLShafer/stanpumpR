# Dobutamine in neonates (R/drugs_dobutamineNeonatal.R): Hallik 2020's one
# compartment with allometric birth weight and a postmenstrual-age maturation,
# and the heart-rate effect site.  Drafted by Claude Code, 2026-10-27.
#
# The expectations are written out from Table 2 independently of the drug file.

# Hallik 2020, Table 2 and Table 5
cl0 <- 41.2; v0 <- 5.29; wt0 <- 1.618; tm50 <- 37.4; hill <- 2.67; ke0h <- 6.59

hallikCL <- function(bw, pma) cl0 * (bw / wt0)^0.75 * pma^hill / (tm50^hill + pma^hill)

events <- data.frame(Time = double(), Event = character())

test_that("clearance and volume are Table 2's, at the median infant and elsewhere", {
  # The study's median: 1618 g, 30.4 weeks, a few hours old
  set <- dobutamineNeonatal(1.618, 42, 0, "male", adjustToFFM = FALSE,
                            gestationalAge = 30.4)$PK$default
  expect_equal_rounded(hallikCL(1.618, 30.4) / 60, set$cl1)
  expect_equal_rounded(v0, set$v1)
  expect_equal(set$cl2, 0)
  # 15.04 L/h, 155 mL/min/kg: the mature 41.2 L/h times a maturation of 0.365
  expect_equal(round(set$cl1 * 60, 2), 15.04)
  expect_equal(round(set$cl1 / 1.618 * 1000), 155)
  # A 3.5 kg term infant at 3 days, and a 0.6 kg infant born at 24 weeks at 2 days
  for (args in list(list(bw = 3.5, ga = 39, age = 3 / 365),
                    list(bw = 0.6, ga = 24, age = 2 / 365))) {
    s <- dobutamineNeonatal(args$bw, 45, args$age, "female", adjustToFFM = FALSE,
                            gestationalAge = args$ga)$PK$default
    expect_equal_rounded(hallikCL(args$bw, args$ga + 52 * args$age) / 60, s$cl1)
    expect_equal_rounded(v0 * args$bw / wt0, s$v1)
  }
})

test_that("a blank gestational age is a term birth", {
  blank <- dobutamineNeonatal(1.618, 42, 0, "male", adjustToFFM = FALSE)$PK$default
  term  <- dobutamineNeonatal(1.618, 42, 0, "male", adjustToFFM = FALSE,
                              gestationalAge = 40)$PK$default
  expect_equal(blank$cl1, term$cl1)
  expect_equal(blank$cl1,
               dobutamineNeonatal(1.618, 42, 0, "male", adjustToFFM = FALSE,
                                  gestationalAge = NA)$PK$default$cl1)
  # and overstates a 30-week infant's clearance 1.5-fold on the first day
  early <- dobutamineNeonatal(1.618, 42, 0, "male", adjustToFFM = FALSE,
                              gestationalAge = 30)$PK$default
  expect_equal(round(blank$cl1 / early$cl1, 2), 1.53)
  # The volume does not mature
  expect_equal(blank$v1, early$v1)
})

test_that("getDrugPK passes the gestational age, and only to models that name it", {
  pk <- getDrugPK("dobutamineNeonatal", 1.618, 42, 0, "male", adjustToFFM = FALSE,
                  gestationalAge = 30.4)$PK$default
  expect_equal_rounded(hallikCL(1.618, 30.4) / 60, pk$cl1)
  # NA from an empty field is blank
  expect_equal(getDrugPK("dobutamineNeonatal", 1.618, 42, 0, "male",
                         gestationalAge = NA)$PK$default$cl1,
               getDrugPK("dobutamineNeonatal", 1.618, 42, 0, "male")$PK$default$cl1)
  # Out of range is refused
  expect_error(getDrugPK("dobutamineNeonatal", 1.618, 42, 0, "male",
                         gestationalAge = 15), "gestationalAge")
  # A model that does not name it is unaffected
  expect_equal(getDrugPK("milrinonePediatric", 3, 50, 0, "male",
                         gestationalAge = 28)$PK$default$cl1,
               getDrugPK("milrinonePediatric", 3, 50, 0, "male")$PK$default$cl1)
  expect_equal(postmenstrualAge(0.5), 66)
  expect_equal(postmenstrualAge(0, 28), 28)
  # NaN is not blank, on the direct path either
  expect_error(postmenstrualAge(0, NaN), "gestationalAge")
  expect_error(dobutamineNeonatal(1.618, 42, 0, "male", gestationalAge = NaN),
               "gestationalAge")
  expect_error(getDrugPK("dobutamineNeonatal", 1.618, 42, 0, "male",
                         gestationalAge = NaN), "gestationalAge")
})

test_that("the effect site is the heart-rate model's ke0", {
  pk <- getDrugPK("dobutamineNeonatal", 1.618, 42, 0, "male", adjustToFFM = FALSE,
                  gestationalAge = 30.4)
  expect_equal(pk$PK$default$ke0, ke0h / 60, tolerance = 1e-6)
  # One compartment with an effect site: tPeak = ln(ke0/k) / (ke0 - k)
  k <- hallikCL(1.618, 30.4) / v0
  expect_equal(pk$tPeak, log(ke0h / k) / (ke0h - k) * 60, tolerance = 1e-4)
  expect_equal(round(pk$tPeak, 1), 13.5)
  expect_equal(pk$MEAC, 0)
  expect_equal(pk$typical, 0)
})

test_that("an infusion reaches rate over clearance, and the effect site follows", {
  # 10 mcg/kg/min in the median infant: 16.18 mcg/min / 0.2507 L/min = 64.5 mcg/L
  wide <- simulateDrugsWithCovariates(
    data.frame(Drug = "dobutamineNeonatal", Time = c(0, 120), Dose = c(10, 0),
               Units = "mcg/kg/min"),
    events, 1.618, 42, 0, "male", 240, FALSE, adjustToFFM = FALSE,
    gestationalAge = 30.4)$dobutamineNeonatal$wide
  cl <- hallikCL(1.618, 30.4) / 60
  k  <- cl / v0
  times <- wide$Time[wide$Time > 0 & wide$Time <= 120]
  ref <- 10 * 1.618 / cl * (1 - exp(-k * times))
  expect_lt(max(abs(wide$Plasma[wide$Time > 0 & wide$Time <= 120] / ref - 1)), 1e-6)
  expect_equal(round(10 * 1.618 / cl, 1), 64.5)
  # the effect site lags the plasma during the rise and has caught up by 120 min
  rising <- wide$Time > 0 & wide$Time < 20
  expect_true(all(wide$`Effect Site`[rising] < wide$Plasma[rising]))
  expect_lt(abs(wide$`Effect Site`[wide$Time == 120] /
                  wide$Plasma[wide$Time == 120] - 1), 0.01)
})

test_that("the fat-free-mass switch moves only the weight the equations see", {
  on  <- dobutamineNeonatal(1.618, 42, 0, "male", adjustToFFM = TRUE,
                            gestationalAge = 30.4)$PK$default
  size <- pkSizeFactors(1.618, 42, 0, "male", TRUE)
  expect_equal_rounded(hallikCL(size$pkWeight, 30.4) / 60, on$cl1)
  expect_equal_rounded(v0 * size$pkWeight / wt0, on$v1)
})

test_that("the drug is a vasoactive infusion with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "dobutamineNeonatal", ]
  expect_identical(row$Category, VASOACTIVE_DRUG_CATEGORY)
  expect_setequal(row$Units[[1]], c("mcg/kg/min", "mcg/min"))
  expect_true(is.na(row$Bolus.Units) || row$Bolus.Units == "")
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  expect_true("gestationalAge" %in% names(formals(dobutamineNeonatal)))
})
