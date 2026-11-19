# insulinDegludec: Tang 2026 (Clin Pharmacokinet 65:417-429), see
# R/drugs_insulinDegludec.R.  Every expectation is worked from Table 2's
# printed numbers, not from the code under test.
noEvents <- data.frame(Time = numeric(0), Event = character(0))

# Table 2: ka 0.0476 /h x 0.837 (IDeg) x 0.85 (T1DM); CL/F 1.93 L/h x 0.978
# (T1DM); Vc/F 20.9 L x 0.537 (T1DM/T2DM); lag 0.4 h; weight powers 1.04 and
# 1.54 about 70 kg, held at 93.1 kg above and unheld below (Shafer, 2026-11-18)
tangKa <- 0.0476 * 0.837 * 0.85                       # 0.033865 /h
tangCl <- function(w) 1.93 * 0.978 * (w / 70)^1.04    # L/h
tangV  <- function(w) 20.9 * 0.537 * (w / 70)^1.54    # L

# Independent one-compartment solution with a lag: concentration in U/L for a
# dose in U at time 0 (t, lag in h)
oneCompartment <- function(t, dose, ka, cl, v, lag) {
  k <- cl / v; s <- pmax(t - lag, 0)
  dose / v * ka / (ka - k) * (exp(-k * s) - exp(-ka * s))
}

test_that("the reference patient: Table 2's type 1 diabetes typical values", {
  actual <- insulinDegludec(70, 170, 35, "male", adjustToFFM = FALSE)
  expected <- list(
    PK = list(default = list(
      v1 = 11.2233, v2 = 1, v3 = 1,
      cl1 = 1.88754 / 60, cl2 = 0, cl3 = 0,
      ka_SC = 0.03386502 / 60, bioavailability_SC = 1, tlag_SC = 24
    )),
    ke0 = INSULIN_KE0 / 60, MEAC = 0, typical = 0, upperTypical = 0, lowerTypical = 0
  )
  expect_equal_rounded(actual[names(expected)], expected)
  expect_equal(actual$PK$default$ka_SC * 60, tangKa)
  expect_match(actual$reference, "10.1007/s40262-025-01598-x", fixed = TRUE)
})

test_that("body weight: power terms about 70 kg, held at 93.1 kg above only", {
  pk <- function(w) insulinDegludec(w, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(pk(85)$cl1 * 60, tangCl(85))
  expect_equal(pk(85)$v1, tangV(85))
  expect_equal(pk(85)$ka_SC * 60, tangKa)            # no weight term on ka
  expect_equal(pk(120)$cl1 * 60, tangCl(93.1))       # held at the 95th percentile
  expect_equal(pk(120)$v1, tangV(93.1))
  # below 54.4 kg Tang's terms carry on (FDA pediatric review, exponent 0.977)
  expect_equal(pk(40)$cl1 * 60, tangCl(40))
  expect_equal(pk(20)$cl1 * 60, tangCl(20))
  expect_equal(pk(10)$v1, tangV(10))
  # per-kilogram dosing gives a child about an adult's average level:
  # 0.4 U/kg x 6 nmol/U / CL/F / 24 h, 3.90 nmol/L at 20 kg, 3.71 at 70 kg
  avg <- function(w) 0.4 * w * 6 / tangCl(w) / 24
  expect_equal(round(c(avg(20), avg(70)), 2), c(3.90, 3.71))
  # With the fat-free-mass switch on, the terms read the pharmacokinetic weight
  w <- pkSizeFactors(120, 170, 50, "male", TRUE)$pkWeight
  on <- insulinDegludec(120, 170, 50, "male")$PK$default
  expect_equal(on$cl1 * 60, tangCl(min(w, 93.1)))
  expect_equal(on$v1, tangV(min(w, 93.1)))
  # the reference man is unchanged by the switch
  expect_equal(insulinDegludec(70, 170, 35, "male")$PK$default$cl1,
               pk(70)$cl1, tolerance = 1e-6)
})

test_that("a single dose matches the independent closed-form solution", {
  PK <- getDrugPK("insulinDegludec", 70, 170, 35, "male", adjustToFFM = FALSE)
  w <- simCpCe(data.frame(Drug = "insulinDegludec", Time = 0, Dose = 28, Units = "U SC"),
               noEvents, PK, 4 * MINS_PER_DAY, FALSE)$wide
  ref <- 1000 * oneCompartment(w$Time / 60, 28, tangKa, tangCl(70), tangV(70), 0.4)
  expect_equal(w$Plasma, ref, tolerance = 1e-6)
  expect_true(all(w$Plasma[w$Time <= 24] == 0))     # nothing before the 0.4 h lag
})

test_that("0.4 U/kg once daily against the label (type 1 diabetes, day 8)", {
  # Label 12.3: Cmax 4472 pmol/L at a median 9 h after 8 days of 0.4 U/kg;
  # 1 U = 6 nmol, so 4472 pmol/L = 745.3 mcU/mL.  Steady state "after 3-4 days".
  PK <- getDrugPK("insulinDegludec", 70, 170, 35, "male", adjustToFFM = FALSE)
  w <- simCpCe(data.frame(Drug = "insulinDegludec", Time = 0, Dose = 28, Units = "U SC qd"),
               noEvents, PK, 8 * MINS_PER_DAY, FALSE)$wide
  day8 <- w[w$Time >= 7 * MINS_PER_DAY & w$Time < 8 * MINS_PER_DAY, ]
  peak <- max(day8$Plasma)
  # the closed-form steady state, independently
  k <- tangCl(70) / tangV(70); s <- seq(0, 24, by = 0.001) - 0.4; s <- s %% 24
  css <- 1000 * 28 / tangV(70) * tangKa / (tangKa - k) *
    (exp(-k * s) / (1 - exp(-k * 24)) - exp(-tangKa * s) / (1 - exp(-tangKa * 24)))
  expect_equal(peak, max(css), tolerance = 2e-3)                     # 695.7 mcU/mL
  expect_equal(6 * peak, 4472, tolerance = 0.08)                      # 4174 pmol/L, 6.7% low
  tPeak <- (day8$Time[which.max(day8$Plasma)] - 7 * MINS_PER_DAY) / 60
  expect_gt(tPeak, 7); expect_lt(tPeak, 10)                          # 8.1 h; label median 9 h
  # trough before the 4th dose: 92% of steady state; before the 5th: 97%
  trough <- function(day) w$Plasma[which.min(abs(w$Time - day * MINS_PER_DAY))]
  expect_gt(trough(3) / min(css), 0.90)
  expect_gt(trough(4) / min(css), 0.96)
  # once-daily average = dose / (CL/F x 24 h)
  t <- day8$Time; cp <- day8$Plasma
  avg <- sum(diff(t) * (head(cp, -1) + tail(cp, -1)) / 2) / (max(t) - min(t))
  expect_equal(avg, 28000 / (tangCl(70) * 24), tolerance = 0.01)
  # terminal (absorption) half-life: flip-flop, ka < CL/V
  expect_lt(tangKa, k)
  expect_equal(log(2) / tangKa, 20.47, tolerance = 1e-3)
})

test_that("Tang's healthy set reproduces the FDA review's single dose and Tang's Table 4", {
  # Not the drug's default (type 1 diabetes), but the published typical values
  # for a healthy 70 kg subject: ka 0.0476 x 0.837, CL/F 1.93, Vc/F 20.9.
  ka <- 0.0476 * 0.837; cl <- 1.93; v <- 20.9
  t <- seq(0, 120, by = 0.01)
  c1 <- 6 * 1000 * oneCompartment(t, 0.4 * 70, ka, cl, v, 0.4)       # pmol/L
  # FDA OCP review of NDA 203314, Table 7 (NN1250-3769, healthy, 0.4 U/kg):
  # Cmax 2083 pmol/L, tmax 14 h (10-22), CL/F 0.029 L/h/kg, Vd/F 0.667 L/kg,
  # t1/2 16 h
  expect_equal(max(c1), 2083, tolerance = 0.15)                      # 1833
  expect_gt(t[which.max(c1)], 10); expect_lt(t[which.max(c1)], 22)  # 16.4 h
  expect_equal(cl / 70, 0.029, tolerance = 0.06)                     # 0.0276
  expect_equal(cl / ka / 70, 0.667, tolerance = 0.06)                # 0.692 L/kg
  expect_equal(log(2) / ka, 16, tolerance = 0.1)                     # 17.4 h
  # Table 4 (simulated steady state, its units read as nmol/L): AUCtau,ss 130
  # and Cmax,ss 5.98 for degludec, here at 0.6 U/kg in a healthy 70 kg subject
  dose <- 0.6 * 70 * 6                                               # nmol
  expect_equal(dose / cl, 130, tolerance = 0.01)
  k <- cl / v; s <- (seq(0, 24, by = 0.001) - 0.4) %% 24
  css <- dose / v * ka / (ka - k) *
    (exp(-k * s) / (1 - exp(-k * 24)) - exp(-ka * s) / (1 - exp(-ka * 24)))
  expect_equal(max(css), 5.98, tolerance = 0.02)                     # 5.91
})

test_that("the CSV row: SC units, daily schedule, no pump, no band", {
  d <- getDrugDefaults("insulinDegludec")
  expect_equal(d$Concentration.Units, "mcU")
  expect_setequal(d$Units[[1]], c("U SC", "U/kg SC", "U SC qd"))
  expect_false(any(d$Units[[1]] %in% insulinRateUnits))   # the label forbids a pump
  expect_equal(c(d$Lower, d$Upper, d$Typical, d$MEAC, d$endCe), rep(0, 5))
})

test_that("the antidiabetic registry's audit holds the numbers the model uses", {
  par <- utils::read.csv(system.file("extdata", "antidiabeticParameters.csv", package = "stanpumpR"),
                         stringsAsFactors = FALSE)
  val <- function(p) as.numeric(par$value[par$drug == "insulinDegludec" & par$parameter == p])
  expect_equal(val("ka") * val("IDeg on ka") * val("T1DM on ka"), tangKa)
  expect_equal(val("CL/F") * val("T1DM on CL/F"), tangCl(70))
  expect_equal(val("Vc/F") * val("T1DM/T2DM on Vc/F"), tangV(70))
  expect_equal(val("ALAG1") * 60,
               insulinDegludec(70, 170, 35, "male", FALSE)$PK$default$tlag_SC)
  reg <- utils::read.csv(system.file("extdata", "antidiabeticRegistry.csv", package = "stanpumpR"),
                         stringsAsFactors = FALSE)
  row <- reg[reg$key == "insulin_degludec_u100_u200", ]
  expect_equal(row$stanpumpr_drug, "insulinDegludec")
  expect_equal(row$pk_status, "PK-FIT")
  expect_false(row$csii)
})

test_that("pregnancy: CL/F x 1.25, as every insulin (Shafer, 2026-11-18)", {
  off <- insulinDegludec(70, 165, 30, "female")
  on <- insulinDegludec(70, 165, 30, "female", pregnant = TRUE)
  expect_equal(on$PK$default$cl1 / off$PK$default$cl1, 1.25)
  same <- c("v1", "ka_SC", "tlag_SC", "bioavailability_SC")
  expect_identical(on$PK$default[same], off$PK$default[same])
  expect_match(on$reference, "Catalano", fixed = TRUE)
  expect_no_match(off$reference, "Catalano", fixed = TRUE)
  expect_true("insulinDegludec" %in% pregnancyDrugs())
})
