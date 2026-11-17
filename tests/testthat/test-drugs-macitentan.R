# Macitentan and aprocitentan (R/drugs_macitentan.R, R/drugs_aprocitentan.R):
# Bartolucci 2021's model, three absorption paths and the metabolite, on the
# saturable engine's oralPaths and metabolite.  Drafted by Claude Code,
# 2026-11-16.  The expectations are computed here from Table 3 and Figure 1,
# and the curves checked against an independent matrix-exponential solution.

noEvents <- data.frame(Time = double(), Event = character())
P <- list(ka = c(0.312, 0.0502, 0.381), D1 = 2.77, Tlag2 = 11.6, D3 = 3.46,
          FPM = 0.0628, gamma = 3.73, beta = 0.866, V1 = 34.4, CL1 = 1.39,
          V2 = 23.5, CL2 = 0.321, fm = 0.62, wtRef = 68.8)

test_that("Table 3 at the median weight, a woman, switch off", {
  X <- macitentan(68.8, 163, 44, "female", adjustToFFM = FALSE, creatinine = 0.8)
  set <- X$PK$default
  expect_equal(set$v1, P$V1)
  expect_equal(set$cl1 * 60, P$CL1)
  mm <- X$michaelisMenten
  expect_equal(mm$clLinear, set$cl1)
  expect_equal(mm$metabolite$fraction, 0.62)
  expect_equal(mm$metabolite$v1, P$V2)
  expect_equal(mm$metabolite$cl * 60, P$CL2)
  expect_equal(vapply(mm$oralPaths, function(p) p$ka * 60, 1), P$ka)
  expect_equal(mm$oralPaths[[1]]$zeroOrder, P$D1 * 60)
  expect_equal(mm$oralPaths[[2]]$lag, P$Tlag2 * 60)
  expect_equal(mm$oralPaths[[3]]$zeroOrder, P$D3 * 60)
  expect_identical(mm$oralPaths[[3]]$into, "metabolite")
  # FMAC at 10 mg: 0.84 from the final model (0.81 is the base model's)
  expect_equal(round(plogis(P$gamma - P$beta * log(11)), 2), 0.84)
  expect_equal(mmPathShare(mm$oralPaths[[1]]$share, 10) +
                 mmPathShare(mm$oralPaths[[2]]$share, 10) +
                 mmPathShare(mm$oralPaths[[3]]$share, 10), 1)
  # Ftotal is 1 at 10 mg and falls above it
  expect_equal(oralSaturationFraction(10, X$oralSaturation), 1)
  expect_lt(oralSaturationFraction(100, X$oralSaturation), 1)
})

test_that("the covariates of Table 3", {
  apro <- function(...) aprocitentan(...)$PK$default
  f <- apro(68.8, 163, 44, "female", FALSE, 0.8)
  m <- apro(68.8, 175, 44, "male", FALSE, 0.9)
  expect_equal(m$cl1 / f$cl1, 1.157)
  # moderate or severe renal impairment (MDRD below 60): CL2 x 0.869
  r <- apro(68.8, 163, 44, "female", FALSE, 2.0)
  expect_equal(r$cl1 / f$cl1, 1 - 0.131)
  # weight
  w <- macitentan(100, 175, 44, "male", FALSE, 0.9)$PK$default
  expect_equal(w$cl1 * 60, P$CL1 * (100 / 68.8)^0.54)
  expect_equal(w$v1, P$V1 * (100 / 68.8)^0.81)
  expect_equal(apro(100, 175, 44, "male", FALSE, 0.9)$v1, P$V2 * (100 / 68.8)^0.86)
})

test_that("both curves agree with an independent solution of Figure 1", {
  dose <- 10                        # mg, so 10000 mcg
  s <- simulateDrugsWithCovariates(
    data.frame(Drug = "macitentan", Time = 0, Dose = dose, Units = "mg PO"),
    noEvents, 68.8, 163, 44, "female", 96 * 60, FALSE, adjustToFFM = FALSE,
    creatinine = 0.8)
  p <- s$macitentan$wide; m <- s$aprocitentan$wide
  amt <- dose * 1000
  fmac <- plogis(P$gamma - P$beta * log(1 + dose))
  a1 <- amt * (1 - P$FPM) * fmac; a2 <- amt * (1 - P$FPM) * (1 - fmac); a3 <- amt * P$FPM
  # states: depot1, depot2, depot3, macitentan, aprocitentan (amounts); the
  # zero-order inputs of paths 1 and 3 enter depots 1 and 3
  k10 <- P$CL1 / P$V1; k20 <- P$CL2 / P$V2
  A <- matrix(0, 5, 5)
  A[1, 1] <- -P$ka[1]; A[2, 2] <- -P$ka[2]; A[3, 3] <- -P$ka[3]
  A[4, 1] <- P$ka[1]; A[4, 2] <- P$ka[2]; A[4, 4] <- -k10
  A[5, 3] <- P$ka[3]; A[5, 4] <- P$fm * k10; A[5, 5] <- -k20
  # exp(A t), A lower triangular with distinct diagonal, by its eigenvectors;
  # a constant input r over dt adds A^-1 (exp(A dt) - I) r
  e <- eigen(A); EV <- e$vectors; EVi <- solve(EV)
  expAt <- function(dt) Re(EV %*% diag(exp(e$values * dt)) %*% EVi)
  step <- function(x, dt, r) {
    E <- expAt(dt)
    as.vector(E %*% x + solve(A, (E - diag(5)) %*% r))
  }
  at <- function(h) {
    edges <- sort(unique(c(0, P$D1, P$D3, P$Tlag2, h)))
    edges <- edges[edges <= h]
    x <- rep(0, 5)
    for (i in seq_len(length(edges) - 1)) {
      t <- edges[i]
      if (abs(t - P$Tlag2) < 1e-12) x[2] <- x[2] + a2
      r <- c(if (t < P$D1) a1 / P$D1 else 0, 0, if (t < P$D3) a3 / P$D3 else 0, 0, 0)
      x <- step(x, edges[i + 1] - t, r)
    }
    if (abs(h - P$Tlag2) < 1e-12) x[2] <- x[2] + a2
    c(x[4] / P$V1, x[5] / P$V2)
  }
  for (h in c(1, 2, 3, 6, 12, 24, 48, 96)) {
    i <- which.min(abs(p$Time - h * 60))
    ref <- at(p$Time[i] / 60)
    expect_equal(p$Plasma[i], ref[1], tolerance = 1e-5)
    expect_equal(m$Plasma[i], ref[2], tolerance = 1e-5)
  }
})

test_that("the abstract's typical woman with PAH and the label (10 mg daily)", {
  s <- simulateDrugsWithCovariates(
    data.frame(Drug = "macitentan", Time = 0, Dose = 10, Units = "mg PO qd"),
    noEvents, 68.8, 163, 44, "female", 31 * 1440, FALSE, adjustToFFM = FALSE)
  p <- s$macitentan$wide; m <- s$aprocitentan$wide
  auc <- function(w, a, b) {
    k <- w$Time >= a & w$Time <= b
    sum(diff(w$Time[k]) * (head(w$Plasma[k], -1) + tail(w$Plasma[k], -1)) / 2)
  }
  d <- 30 * 1440
  # "twofold accumulation" and "12.5-fold" (abstract)
  expect_equal(auc(p, d, d + 1440) / auc(p, 0, 1440), 2, tolerance = 0.1)
  expect_equal(auc(m, d, d + 1440) / auc(m, 0, 1440), 12.5, tolerance = 0.05)
  # the metabolite's exposure "3-times" macitentan's at steady state (label)
  expect_equal(auc(m, d, d + 1440) / auc(p, d, d + 1440), 3, tolerance = 0.05)
  # first-dose peaks: "after 9 h" and "after 51 h" (abstract)
  first <- p$Time <= 1440
  expect_equal(p$Time[first][which.max(p$Plasma[first])] / 60, 9, tolerance = 0.1)
})

test_that("switch on: the weight terms at the pharmacokinetic weight", {
  w <- 70 * 1.3049067               # 120 kg, 170 cm, 50 y man
  set <- macitentan(120, 170, 50, "male", creatinine = 0.9)$PK$default
  expect_equal_rounded(set$v1, P$V1 * (w / 68.8)^0.81)
  expect_equal_rounded(set$cl1 * 60, P$CL1 * (w / 68.8)^0.54)
})

test_that("aprocitentan cannot be dosed; macitentan is oral only", {
  dd <- getDrugDefaultsGlobal()
  expect_true(all(is.na(unlist(dd$Units[dd$Drug == "aprocitentan"]))))
  expect_true(is.na(dd$Category[dd$Drug == "aprocitentan"]))
})
