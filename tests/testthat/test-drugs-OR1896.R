# OR-1896, levosimendan's active metabolite, from Bertin 2026's chain on the
# transit-chain engine (R/drugs_OR1896.R).  Checked three ways: the published
# numbers at 70 kg with the size switch off, an independent solution of the
# same linear system by eigen-decomposition, and two measured studies the
# model was never fitted to: Antila 2004's 7-day infusions and Kivikko 2002's
# 24-hour infusion.

noEvents <- data.frame(Time = numeric(0), Event = character(0),
                       stringsAsFactors = FALSE)

runLevo <- function(rate, hours = 168, followHours = 15 * 24, wt = 84,
                    ht = 176, age = 60, sex = "male", ffm = TRUE)
{
  dose <- data.frame(Drug = "levosimendan", Time = c(0, hours * 60),
                     Dose = c(rate, 0), Units = "mcg/kg/min",
                     stringsAsFactors = FALSE)
  simulateDrugsWithCovariates(dose, noEvents, wt, ht, age, sex,
                              (hours + followHours) * 60, FALSE,
                              adjustToFFM = ffm)
}

test_that("the chain is Bertin 2026's supplement estimates at 70 kg, switch off", {
  ch <- levosimendanOR1896Chain(70, 170, 35, "male", adjustToFFM = FALSE)
  expect_equal(ch$ktr * 60, 0.0127)
  expect_equal(ch$kel * 60, 0.01)
  expect_equal(ch$k12 * 60, 0.0722)
  expect_equal(ch$k21 * 60, 0.012)
  expect_equal(ch$kelPeripheral * 60, 0.01)
  expect_equal(ch$v, 15.9)
  expect_equal(ch$vPeripheral, 15.9)
  # K13 / (CL / V1): the share of the parent's elimination that forms OR-1855
  expect_equal_rounded(ch$fm, 0.0127 / (13.9 / 15.9))
  expect_equal_rounded(ch$mwRatio, 245.28 / 280.29)
  # keM1 carries Bertin's own weight term
  ch100 <- levosimendanOR1896Chain(100, 170, 35, "male", adjustToFFM = FALSE)
  expect_equal(ch100$kel * 60, 0.01 * (100 / 70)^-0.611)
  expect_equal(ch100$v, 15.9 * (100 / 70)^0.574)
})

test_that("the chain is pinned with the size switch on, away from the reference", {
  # The patient test-drugs-levosimendan.R pins the parent at: 120 kg, 175 cm,
  # 50 years, a man; pharmacokinetic weight 94.3906 kg, and the fat-free-mass
  # clearance factor over the volume factor 0.927988.  Bertin's own weight
  # terms are evaluated at the pharmacokinetic weight; the size-free rate
  # constants take that ratio.
  ch <- levosimendanOR1896Chain(120, 175, 50, "male", adjustToFFM = TRUE)
  w <- 94.3906332567 / 70
  rate <- 0.9279878392
  expect_equal_rounded(15.9 * w^0.574, ch$v)
  expect_equal_rounded(18.87643765, ch$v)
  expect_equal_rounded(ch$v, ch$vPeripheral)
  # K13 has no weight term and equals ktr in the source: it takes the ratio too
  expect_equal_rounded(0.0127 * rate / (13.9 * w^0.75 / ch$v), ch$fm)
  expect_equal_rounded(0.01378274794 * 0.9279878392, ch$fm)
  expect_equal_rounded(0.0127 * rate / 60, ch$ktr)
  expect_equal_rounded(1.964240926e-04, ch$ktr)
  expect_equal_rounded(0.01 * w^-0.611 / 60, ch$kel)
  expect_equal_rounded(1.388423943e-04, ch$kel)
  expect_equal_rounded(0.0722 * rate / 60, ch$k12)
  expect_equal_rounded(1.116678700e-03, ch$k12)
  expect_equal_rounded(0.012 * rate / 60, ch$k21)
  expect_equal_rounded(1.855975678e-04, ch$k21)
  expect_equal_rounded(0.01 * rate / 60, ch$kelPeripheral)
  expect_equal_rounded(1.546646399e-04, ch$kelPeripheral)
})

test_that("levosimendan forms OR1896 through an intravenous transit chain", {
  X <- getDrugPK("levosimendan", 70, 170, 35, "male", adjustToFFM = FALSE)
  expect_identical(X$transitChain$n, 0L)
  expect_identical(X$transitChain$metabolite$name, "OR1896")
  expect_identical(X$transitChain$metabolite$plotted, "peripheral")
  set <- X$PK$default
  expect_equal_rounded(X$transitChain$kMetabolic,
                       0.0127 / (13.9 / 15.9) * set$k10)
  # No peripheral panel for OR1896: OR-1855 is not drawn
  expect_equal(set$metabolite$k21, 0)
  # An oral dose has nowhere to go
  bad <- data.frame(Drug = "levosimendan", Time = 0, Dose = 1, Units = "mg PO",
                    stringsAsFactors = FALSE)
  expect_error(simulateDrugsWithCovariates(bad, noEvents, 70, 170, 35, "male",
                                           600, FALSE))
})

test_that("OR1896 matches an independent eigen-decomposition of the system", {
  wt <- 84; ht <- 176; age <- 60; sex <- "male"
  X <- getDrugPK("levosimendan", wt, ht, age, sex)
  set <- X$PK$default
  tc <- X$transitChain; m <- tc$metabolite
  # States: parent central, parent peripheral, transit, OR-1855, OR-1896
  A <- matrix(0, 5, 5)
  A[1, 1] <- -(set$k10 + set$k12); A[1, 2] <- set$k21
  A[2, 1] <- set$k12; A[2, 2] <- -set$k21
  A[3, 1] <- tc$kMetabolic; A[3, 3] <- -m$ktr
  A[4, 3] <- m$ktr; A[4, 4] <- -(m$kel + m$k12); A[4, 5] <- m$k21
  A[5, 4] <- m$k12; A[5, 5] <- -(m$k21 + m$kelPeripheral)
  e <- eigen(A); V <- e$vectors; Vi <- solve(V); lam <- e$values
  rate <- 0.1 * wt                                   # mcg/min
  b <- c(rate, 0, 0, 0, 0)
  Tinf <- 168 * 60
  # Constant infusion from 0 to Tinf, then free decay
  during <- function(t) Re(V %*% (((exp(lam * t) - 1) / lam) * (Vi %*% b)))
  xT <- during(Tinf)
  state <- function(t) if (t <= Tinf) during(t) else
    Re(V %*% (exp(lam * (t - Tinf)) * (Vi %*% xT)))
  out <- runLevo(0.1, wt = wt, ht = ht, age = age, sex = sex)
  w <- out$OR1896$wide
  pick <- which(w$Time %in% c(600, 4800, 9000, Tinf, 12000, 16000, 25000))
  ref <- vapply(w$Time[pick], function(t) state(t)[5], 0) / m$vPeripheral *
    m$mwRatio
  expect_equal(w$Plasma[pick], ref, tolerance = 1e-8)
  # The parent too
  pw <- out$levosimendan$wide
  pp <- which(pw$Time %in% c(600, 4800, Tinf, 12000))
  pref <- vapply(pw$Time[pp], function(t) state(t)[1], 0) / set$v1
  expect_equal(pw$Plasma[pp], pref, tolerance = 1e-8)
})

test_that("OR1896 reproduces Antila 2004's 7-day infusions", {
  # Antila 2004 (Br J Clin Pharmacol 57:412-415, Table 2): OR-1896 Cmax 9.9
  # and 17.1 ng/mL, AUC(0-inf) 2370 and 3890 ng.h/mL, half-life 81 h, at
  # 0.05 and 0.1 mcg/kg/min for 7 days; about 84 kg (Table 1's CL in L/h
  # over L/h/kg).  Never fitted to this model.
  for (case in list(list(rate = 0.05, cmax = 9.9, auc = 2370),
                    list(rate = 0.1, cmax = 17.1, auc = 3890)))
  {
    w <- runLevo(case$rate)$OR1896$wide
    t <- w$Time / 60; c <- w$Plasma
    tail <- t > max(t) - 120
    slope <- unname(coef(lm(log(c[tail]) ~ t[tail]))[2])
    auc <- sum(diff(t) * (head(c, -1) + tail(c, -1)) / 2) + c[length(c)] / -slope
    expect_lt(abs(max(c) / case$cmax - 1), 0.2)
    expect_lt(abs(auc / case$auc - 1), 0.3)
    expect_lt(abs(log(2) / -slope / 81 - 1), 0.15)
    # The peak follows the end of the 7-day infusion (the measured medians
    # 2 and 12 h after it; the model's about 2 days after)
    expect_gt(t[which.max(c)], 168)
  }
  # Pinned at 0.1 mcg/kg/min
  w <- runLevo(0.1)$OR1896$wide
  expect_equal(max(w$Plasma), 16.43, tolerance = 1e-3)
})

test_that("OR1896 reproduces Kivikko 2002's 24-hour infusion", {
  # Kivikko 2002 (Int J Clin Pharmacol Ther 40:465-471, Table 2, read on the
  # PDF image 2026-11-27): 12 patients, 78 kg, 173 cm, 62 years, 10 men,
  # 0.2 mcg/kg/min for 24 h (the text layer's "mg" is the Symbol font's mu;
  # the figure captions print it).  OR-1896 Cmax 5.5 +/- 2.2 ng/mL, Tmax
  # 77.2 +/- 25.9 h (range 47.7-120), AUC 948 +/- 535 ng.h/mL, half-life
  # 77.4 +/- 30.6 h; levosimendan at 24 h 62.6 +/- 29.2.  Never fitted.
  dose <- data.frame(Drug = "levosimendan", Time = c(0, 24 * 60),
                     Dose = c(0.2, 0), Units = "mcg/kg/min",
                     stringsAsFactors = FALSE)
  out <- simulateDrugsWithCovariates(dose, noEvents, 78, 173, 62, "male",
                                     (24 + 360) * 60, FALSE)
  w <- out$OR1896$wide
  t <- w$Time / 60; c <- w$Plasma
  tail <- t > max(t) - 120
  slope <- unname(coef(lm(log(c[tail]) ~ t[tail]))[2])
  auc <- sum(diff(t) * (head(c, -1) + tail(c, -1)) / 2) + c[length(c)] / -slope
  expect_lt(abs(max(c) / 5.5 - 1), 0.1)
  expect_lt(abs(auc - 948), 535)               # within one SD (40% high)
  expect_lt(abs(log(2) / -slope - 77.4), 30.6)
  # The peak is late, at the top of the measured range, as against Antila
  expect_gt(t[which.max(c)], 77.2)
  expect_lt(t[which.max(c)], 121)
  p <- out$levosimendan$wide
  expect_lt(abs(approx(p$Time / 60, p$Plasma, 24)$y / 62.6 - 1), 0.15)
})

test_that("OR1896 is metabolite-only and has its own model", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "OR1896", ]
  expect_equal(nrow(row), 1)
  expect_true(is.na(row$Category) || !nzchar(row$Category))
  X <- getDrugPK("OR1896", 70, 170, 35, "male", adjustToFFM = FALSE)
  set <- X$PK$default
  expect_equal(set$v1, 15.9)
  expect_equal_rounded(set$cl1 * 60, (0.01 + 0.012) * 15.9)
})
