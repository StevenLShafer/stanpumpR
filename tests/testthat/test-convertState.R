# convertState(): carrying the drug across a change of PK set at a clinical
# event, including a change in the number of compartments.
#
# The references share no code with the package's PK.  Amounts come from a
# matrix exponential of the compartmental rate equations, and the total in the
# body from clearance times the area still to come (all of it is eventually
# cleared).  The closed form convertState() used until 2026-10-07 is kept
# verbatim below, so that three-to-three and two-to-two can be pinned to it.
#
# (Claude Code, Claude Opus 5.5, 2026-10-07, at the request of Steven L. Shafer.)

dd <- getDrugDefaultsGlobal()

pkSet <- function(drug, weight = 70, age = 50) {
  getDrugPK(drug, weight, 170, age, "male", dd[dd$Drug == drug, ])$PK$default
}

# exp(M) by scaling and squaring a Taylor series; M is small and well scaled.
expmTaylor <- function(M) {
  s <- max(0, ceiling(log2(max(abs(M)) * 4 + 1e-300)))
  A <- M / 2^s
  E <- term <- diag(nrow(M))
  for (k in 1:20) { term <- term %*% A / k; E <- E + term }
  for (i in seq_len(s)) E <- E %*% E
  E
}

# Rate equations for the amounts in central, peripheral 2 and peripheral 3.  A
# compartment the set lacks has zero rate constants, so its row and column are
# zero and it stays empty.
rates <- function(s) {
  matrix(c(-(s$k10 + s$k12 + s$k13), s$k12, s$k13,
           s$k21, -s$k21, 0,
           s$k31, 0, -s$k31), 3, 3)
}
nCompartments <- function(s) 1 + (s$k21 > 0) + (s$k31 > 0)

# Where the drug goes at the event: a compartment the new set adds starts empty;
# a compartment it lacks empties into its remaining peripheral compartment, or
# the central one if it has none.
mapAmounts <- function(x, n) {
  switch(n,
         c(sum(x), 0, 0),
         c(x[1], x[2] + x[3], 0),
         x)
}

# After a bolus D at time 0 and an infusion R from 0 to T: the states the
# engine holds for this set, and, independently, the amount in each compartment.
history <- function(s, D, R, T) {
  b <- c(s$p_coef_bolus_l1, s$p_coef_bolus_l2, s$p_coef_bolus_l3)
  lambda <- c(s$lambda_1, s$lambda_2, s$lambda_3)
  infused <- ifelse(lambda > 0, R * b / lambda * (1 - exp(-lambda * T)), 0)
  M <- rbind(cbind(rates(s), c(R, 0, 0)), 0)      # the 1 in the last slot carries R
  x <- expmTaylor(M * T) %*% c(D, 0, 0, 1)
  list(state = D * b * exp(-lambda * T) + infused, amount = x[1:3])
}

plasmaFromStates <- function(s, state, t) {
  sum(state * exp(-c(s$lambda_1, s$lambda_2, s$lambda_3) * t))
}
plasmaFromAmounts <- function(s, amount, t) {
  (expmTaylor(rates(s) * t) %*% amount)[1] / s$v1
}
# Everything in the body is eventually cleared, so the amount in it is
# CL x (area under the plasma curve from now on).
bodyAmount <- function(s, state) {
  lambda <- c(s$lambda_1, s$lambda_2, s$lambda_3)
  s$v1 * s$k10 * sum((state / lambda)[lambda > 0])
}

# convertState() as it was until 2026-10-07, fed the v2 and v3 that
# advanceClosedForm1() computed when every set in the run had the compartment.
convertStateClosedForm <- function(oldState, oldPK, newPK)
{
  withVolumes <- function(s) {
    s$v2 <- if (s$k21 > 0) s$v1 * s$k12 / s$k21 else 0
    s$v3 <- if (s$k31 > 0) s$v1 * s$k13 / s$k31 else 1
    s
  }
  oldPK <- withVolumes(oldPK)
  newPK <- withVolumes(newPK)

  if (oldPK$lambda_2 == 0)
  {
    return(state = oldState) # In a one compartment model, state variable doesn't change
  }

  if (oldPK$lambda_3 == 0)
  {
    a1 <- (oldState[1] + oldState[2]) * oldPK$v1

    a2 <- (
      oldState[1] / (oldPK$k21 - oldPK$lambda_1) * oldPK$k21 +
      oldState[2] / (oldPK$k21 - oldPK$lambda_2) * oldPK$k21
    ) * oldPK$v2
    f1 <- newPK$k21 / (newPK$k21 - newPK$lambda_1)
    f2 <- newPK$k21 / (newPK$k21 - newPK$lambda_2)
    newState2 = (a2 / newPK$v2 -a1 / newPK$v1 * f1) / (f2 - f1)
    newState1 = a1 / newPK$v1 - newState2
    return(state = c(newState1, newState2, 0))
  }

  a1 <- (oldState[1] + oldState[2] + oldState[3]) * oldPK$v1

  a2 <- (
    oldState[1] / (oldPK$k21 - oldPK$lambda_1) * oldPK$k21 +
    oldState[2] / (oldPK$k21 - oldPK$lambda_2) * oldPK$k21 +
    oldState[3] / (oldPK$k21 - oldPK$lambda_3) * oldPK$k21
    ) * oldPK$v2

  a3 <- (
    oldState[1] / (oldPK$k31 - oldPK$lambda_1) * oldPK$k31 +
    oldState[2] / (oldPK$k31 - oldPK$lambda_2) * oldPK$k31 +
    oldState[3] / (oldPK$k31 - oldPK$lambda_3) * oldPK$k31
    ) * (oldPK$v1 * oldPK$k13 / oldPK$k31) # oldPK$v3


  # Set up intermediate variables
  f1 = newPK$v2 * newPK$k21 / (newPK$k21 - newPK$lambda_1)
  f2 = newPK$v2 * newPK$k21 / (newPK$k21 - newPK$lambda_2)
  f3 = newPK$v2 * newPK$k21 / (newPK$k21 - newPK$lambda_3)
  f4 = newPK$v3 * newPK$k31 / (newPK$k31 - newPK$lambda_1)
  f5 = newPK$v3 * newPK$k31 / (newPK$k31 - newPK$lambda_2)
  f6 = newPK$v3 * newPK$k31 / (newPK$k31 - newPK$lambda_3)
  f7 = f5 / f4
  f8 = f6 / f4
  f9 = a3 / f4
  f10 = f1 * f9
  f11 = f1 * f7
  f12 = f1 * f8
  f13 = (f3 - f12) / (f11 - f2)
  f14 = (f10 - a2) / (f11 - f2)
  f15 = a1 / newPK$v1 - f9 - f14 + f7 * f14
  f16 = 1 + f13 - f7 * f13 - f8

  newState3 = f15 / f16
  newState2 = newState3 * f13 + f14
  newState1 = a1 / newPK$v1 - newState2 - newState3

  return(state = c(newState1, newState2, newState3))
}

oneCompartment   <- c("clindamycin", "metronidazole", "cefalexin")
twoCompartment   <- c("vancomycin", "cefazolin", "gentamicin", "rocuronium", "lidocaine")
threeCompartment <- c("propofol", "fentanyl", "remifentanil", "morphine",
                      "ketamine", "dexmedetomidine", "midazolam")


test_that("the drugs chosen here have the compartments they are listed under", {
  for (d in oneCompartment)   expect_equal(nCompartments(pkSet(d)), 1, label = d)
  for (d in twoCompartment)   expect_equal(nCompartments(pkSet(d)), 2, label = d)
  for (d in threeCompartment) expect_equal(nCompartments(pkSet(d)), 3, label = d)
})


test_that("three to three and two to two give what the old closed form gave", {
  set.seed(20261007)
  for (group in list(twoCompartment, threeCompartment)) {
    # Each drug to a heavier, younger patient on the same drug, and to the next
    # drug in the list, so that the eigenvalues move both a little and a lot.
    for (i in seq_along(group)) {
      old <- pkSet(group[i])
      for (new in list(pkSet(group[i], 130, 30),
                       pkSet(group[i %% length(group) + 1]))) {
        states <- list(
          history(old, 100, 1, 2)$state,
          history(old, 100, 1, 60)$state,
          history(old, 100, 0, 600)$state,
          c(stats::rnorm(nCompartments(old)), 0, 0)[1:3]
        )
        for (state in states) {
          expect_equal(convertState(state, old, new),
                       convertStateClosedForm(state, old, new),
                       tolerance = 1e-10)
        }
      }
    }
  }
})


test_that("one to one carries the amount, not the concentration", {
  old <- pkSet("clindamycin")
  new <- pkSet("clindamycin", 130)
  expect_equal(convertState(c(5, 0, 0), old, new), c(5 * old$v1 / new$v1, 0, 0))
  expect_equal(convertState(c(5, 0, 0), old, old), c(5, 0, 0))
  # A one-compartment set needs nothing but its volume and lambda_2
  expect_equal(convertState(c(5, 0, 0), list(lambda_2 = 0, v1 = 10),
                            list(lambda_2 = 0, v1 = 25)), c(2, 0, 0))
})


test_that("a change in the number of compartments conserves the drug and matches a matrix exponential", {
  # 1 -> 2, 1 -> 3, 2 -> 1, 3 -> 1, and 2 -> 3, 3 -> 2, each twice over
  groups <- list(oneCompartment[1:2], twoCompartment[1:2], threeCompartment[1:2])
  for (from in 1:3) for (to in setdiff(1:3, from)) for (k in 1:2) {
    old <- pkSet(groups[[from]][k])
    new <- pkSet(groups[[to]][k])
    for (h in list(history(old, 100, 0, 1),        # a bolus a minute ago
                   history(old, 100, 1, 30),       # mid-infusion
                   history(old, 0, 1, 600))) {     # a long infusion
      what <- paste0(from, " -> ", to, " (", groups[[from]][k], " -> ",
                     groups[[to]][k], ")")
      # The states and the amounts are the same drug in the old set
      expect_equal(sum(h$state) * old$v1, h$amount[1], tolerance = 1e-9,
                   label = paste(what, "old central amount"))
      expect_equal(bodyAmount(old, h$state), sum(h$amount), tolerance = 1e-9,
                   label = paste(what, "old total"))

      state <- convertState(h$state, old, new)
      expect_true(all(is.finite(state)), label = paste(what, "finite"))
      expect_equal(state[-seq_len(to)], rep(0, 3 - to),
                   label = paste(what, "states beyond the new set's compartments"))

      # Mass balance
      expect_equal(bodyAmount(new, state), sum(h$amount), tolerance = 1e-9,
                   label = paste(what, "total amount"))

      # From here the plasma is what the new set makes of the mapped amounts.
      # Agreement at as many times as there are exponentials pins every
      # amount, not only the total.
      mapped <- mapAmounts(h$amount, to)
      for (t in c(0, 1, 10, 100, 1000)) {
        expect_equal(plasmaFromStates(new, state, t),
                     plasmaFromAmounts(new, mapped, t), tolerance = 1e-8,
                     label = paste(what, "plasma", t, "minutes on"))
      }
    }
  }
})


test_that("the switches that were wrong before 2026-10-07", {
  one <- pkSet("clindamycin")
  two <- pkSet("vancomycin")
  three <- pkSet("propofol")

  # One to two left the state alone, which in the two-compartment set implies
  # drug in its peripheral compartment: drug created at the event.  All of it
  # is in the central compartment, so plasma and total agree.
  s <- convertState(c(5, 0, 0), one, two)
  expect_equal(sum(s) * two$v1, 5 * one$v1)
  expect_equal(bodyAmount(two, s), 5 * one$v1)

  s <- convertState(c(5, 0, 0), one, three)
  expect_equal(sum(s) * three$v1, 5 * one$v1)
  expect_equal(bodyAmount(three, s), 5 * one$v1)

  # Two or three to one returned NaN.  Everything ends up central.
  s <- convertState(c(5, 1, 0), two, one)
  expect_equal(s, c(bodyAmount(two, c(5, 1, 0)) / one$v1, 0, 0))

  s <- convertState(c(5, 1, 0.2), three, one)
  expect_equal(s, c(bodyAmount(three, c(5, 1, 0.2)) / one$v1, 0, 0))
})


test_that("the simulation carries the drug across an event that changes the number of compartments", {
  # A drug whose PK set changes structure at 50 minutes, simulated through
  # simCpCe(), against the same thing solved piecewise by matrix exponential.
  # Every set is given clindamycin's units (mg in, mg/L out) and no effect
  # site, since only the plasma is compared.
  noCe <- function(s) { s$ke0 <- 0; s$lambda_4 <- 0; s }
  switching <- function(before, after) {
    PK <- getDrugPK("clindamycin", 70, 170, 50, "male", dd[dd$Drug == "clindamycin", ])
    PK$PK <- list(default = noCe(before), Switch = noCe(after))
    PK$pkEvents <- c(PK_EVENT_DEFAULT, "Switch")
    PK
  }
  switchAt <- 50
  events <- data.frame(Time = switchAt, Event = "Switch")
  # A bolus, an infusion running across the event, a bolus at the event itself
  # and one after it
  DT <- data.frame(Drug = "clindamycin",
                   Time  = c(0,   10,   50,  80,  120),
                   Dose  = c(600, 1200, 300, 300, 0),
                   Units = c("mg", "mg/hr", "mg", "mg", "mg/hr"))
  bolus <- DT[DT$Units == "mg", ]
  rate  <- DT[DT$Units == "mg/hr", ]

  exactPlasma <- function(before, after, times) {
    rateAt <- function(t) {
      i <- which(rate$Time <= t)
      if (length(i) == 0) 0 else rate$Dose[max(i)] / 60
    }
    breaks <- sort(unique(c(0, DT$Time, switchAt, times)))
    x <- c(0, 0, 0, 1)
    out <- numeric(length(times))
    for (k in seq_along(breaks)) {
      t <- breaks[k]
      s <- if (t < switchAt) before else after
      if (t == switchAt) x[1:3] <- mapAmounts(x[1:3], nCompartments(after))
      x[1] <- x[1] + sum(bolus$Dose[bolus$Time == t])
      out[times == t] <- x[1] / s$v1
      if (k < length(breaks)) {
        M <- rbind(cbind(rates(s), c(rateAt(t), 0, 0)), 0)
        x <- as.vector(expmTaylor(M * (breaks[k + 1] - t)) %*% x)
      }
    }
    out
  }

  pairs <- list(c("clindamycin", "vancomycin"), c("vancomycin", "clindamycin"),
                c("clindamycin", "propofol"),   c("propofol", "clindamycin"),
                c("vancomycin", "propofol"),    c("propofol", "vancomycin"))
  for (p in pairs) {
    before <- pkSet(p[1])
    after  <- pkSet(p[2])
    sim <- simCpCe(DT, events, switching(before, after), 300, FALSE)$wide
    expect_false(anyNA(sim$Plasma))
    ref <- exactPlasma(before, after, sim$Time)
    err <- abs(sim$Plasma - ref) / max(ref)
    what <- paste(p, collapse = " -> ")
    # Exact on both sides.  Until 2026-10-07 the 0.01-minute step into the
    # event ran on the new set's eigenvalues, which left errors of up to 2.4%
    # here after clindamycin or vancomycin switched to propofol's set.
    expect_lt(max(err[sim$Time < switchAt]), 1e-9, label = paste(what, "before the event"))
    expect_lt(max(err[sim$Time >= switchAt]), 1e-9, label = paste(what, "after the event"))
  }
})
