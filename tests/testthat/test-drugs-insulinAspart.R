# insulinAspart (NovoLog): see R/insulinAspart.R (the FDA review of Fiasp, NDA
# 208751, Table 5: a depot, three transit compartments at NovoLog's kt
# 16.1/h, one compartment, relative F 0.99).  Drafted by Claude Code,
# 2026-11-18.
#
# The expectations are computed from the source's own equations, in the
# source's own order (the depot first, then the three transits), by a matrix
# exponential independent of the engine's (Matrix::expm), not by the model.

novologSource <- function(t, W, dose_mU, kt = 16.1, f = 0.99, ka = 1.53,
                          cl = 67.8, v = 78.6, ref = 75)
{
  CL <- cl * W / ref; V <- v * W / ref; k <- CL / V
  A <- matrix(0, 5, 5)
  A[1, 1] <- -ka; A[2, 1] <- ka
  for (j in 2:4) { A[j, j] <- -kt; A[j + 1, j] <- kt }
  A[5, 5] <- -k
  vapply(t / 60, function(h)
    (as.matrix(Matrix::expm(A * h)) %*% c(dose_mU * f, 0, 0, 0, 0))[5] / V,
    numeric(1))
}
noEventsNovolog <- data.frame(Time = numeric(0), Event = character(0))

test_that("Table 5: NovoLog's transit rate and relative bioavailability", {
  m <- insulinAspart(75, 175, 40, "male", adjustToFFM = FALSE)
  pk <- m$PK$default
  expect_equal(pk$v1, 78.6)
  expect_equal(pk$cl1, 67.8 / 60)
  expect_equal(pk$ka_SC, 1.53 / 60)
  expect_equal(pk$bioavailability_SC, 0.99)
  expect_equal(m$transitChain$n, 4L)
  expect_equal(m$transitChain$ktr, 16.1 / 60)
  expect_equal(m$transitChain$route, "SC")
  # The disposition is the product's own in neither: shared with Fiasp
  f <- insulinAspartFiasp(75, 175, 40, "male", adjustToFFM = FALSE)$PK$default
  expect_identical(pk[c("v1", "cl1", "ka_SC")], f[c("v1", "cl1", "ka_SC")])
  # Age group from 65
  expect_equal(insulinAspart(75, 175, 70, "female", FALSE)$PK$default$cl1,
               67.8 * 0.905 / 60)
})

test_that("the engine reproduces the source's depot-first chain exactly", {
  skip_if_not_installed("Matrix")
  pk <- getDrugPK("insulinAspart", 75, 175, 40, "male", adjustToFFM = FALSE)
  s <- simCpCe(data.frame(Drug = "insulinAspart", Time = c(0, 300),
                          Dose = c(0.2, 0.1), Units = "U/kg SC"),
               noEventsNovolog, pk, 900, FALSE)
  w <- s$wide
  # two injections superpose
  ref <- novologSource(w$Time, 75, 15000) +
    ifelse(w$Time >= 300, novologSource(pmax(w$Time - 300, 0), 75, 7500), 0)
  expect_lt(max(abs(w$Plasma - ref)) / max(ref), 1e-10)
  expect_equal(s$compartments$Depot[s$compartments$Time == 0], 15 * 0.99)
})

test_that("the review's Figure 4 typical subject, 0.2 U/kg at 75 kg", {
  skip_if_not_installed("Matrix")
  # The figure (printed p 93), pixel-measured: about 556 pmol/L at about 67
  # min.  The model: 535.9 pmol/L at 63.6 min (R/insulinAspart.R, header).
  t <- seq(0, 720, by = 0.1)
  c <- novologSource(t, 75, 15000) * 6
  expect_equal(max(c), 535.9276, tolerance = 1e-6)
  expect_equal(t[which.max(c)], 63.6)
  expect_lt(abs(max(c) / 556 - 1), 0.04)
  expect_equal(c[t == 360] / max(c), 0.03166062, tolerance = 1e-6)
})

test_that("an unfitted check: the NovoLog label's 82 mU/L after 0.15 U/kg", {
  skip_if_not_installed("Matrix")
  # Section 12.3 (revised 2/2023): type 1 diabetes, 0.15 U/kg, mean maximum
  # 82 mU/L (free insulin), median time to maximum 40-50 min.  Here, at the
  # 75 kg reference: 67.0 mcU/mL of total insulin aspart at 63.6 min, 18%
  # below the label and later.  Recorded, not fitted (see the header).
  t <- seq(0, 360, by = 0.1)
  c <- novologSource(t, 75, 0.15 * 75 * 1000)
  expect_equal(max(c), 66.99095, tolerance = 1e-6)
  expect_equal(max(c) / 82, 0.817, tolerance = 1e-3)
})

test_that("a pump rate is carried exactly; NovoLog's steady state", {
  pk <- getDrugPK("insulinAspart", 75, 175, 40, "male", adjustToFFM = FALSE)
  s <- simCpCe(data.frame(Drug = "insulinAspart", Time = 0, Dose = 1,
                          Units = "U/hr SC"), noEventsNovolog, pk, 1440, FALSE)$wide
  # F x rate / CL = 0.99 x 1000 / 67.8 = 14.60 mcU/mL
  expect_equal(tail(s$Plasma, 1), 0.99 * 1000 / 67.8, tolerance = 1e-6)
  expect_true("U/hr SC" %in% getDrugDefaults("insulinAspart")$Units[[1]])
})

test_that("an oral or intramuscular dose is refused, not ignored", {
  pk <- getDrugPK("insulinAspart", 75, 175, 40, "male", adjustToFFM = FALSE)
  expect_error(
    simCpCe(data.frame(Drug = "insulinAspart", Time = 0, Dose = 10, Units = "U IM"),
            noEventsNovolog, pk, 600, FALSE))
})

test_that("pregnancy: CL/F x 1.25, absorption and V/F unchanged", {
  on  <- getDrugPK("insulinAspart", 70, 165, 30, SEX_FEMALE, pregnant = TRUE)
  off <- getDrugPK("insulinAspart", 70, 165, 30, SEX_FEMALE, pregnant = FALSE)
  expect_identical(off, getDrugPK("insulinAspart", 70, 165, 30, SEX_FEMALE))
  a <- on$PK[[1]]; b <- off$PK[[1]]
  expect_equal(a$cl1 / b$cl1, INSULIN_PREGNANCY_CL_FACTOR, tolerance = 1e-6)
  same <- c("v1", "ka_RA", "bioavailability_RA", "tlag_RA")
  expect_identical(a[same], b[same])
  expect_match(on$reference, "Catalano", fixed = TRUE)
  expect_no_match(off$reference, "Catalano", fixed = TRUE)
})
