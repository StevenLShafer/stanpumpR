# Physiology events (R/physiologyEvents.R): the physiology panel recorded at
# a time, physiology normal from 0 and each event's from its time, the
# amounts in the compartments carried across each step (convertState()).

# An independent reference: the compartment amounts and the effect site
# advanced by one augmented matrix exponential per piece, the amounts carried
# across every switch of parameters, a bolus added to the central amount.
physiologyEventReference <- function(pieces, boluses, t)
{
  K <- function(p, rate) {
    M <- matrix(0, 5, 5)
    M[1, 1:3] <- c(-(p$k10 + p$k12 + p$k13), p$k21, p$k31)
    M[2, 1:2] <- c(p$k12, -p$k21)
    M[3, c(1, 3)] <- c(p$k13, -p$k31)
    M[4, 1] <- p$ke0 / p$v1
    M[4, 4] <- -p$ke0
    M[1, 5] <- rate
    M
  }
  x <- c(0, 0, 0, 0, 1)
  last <- pieces[[1]]
  for (pc in pieces) {
    if (t <= pc$from) break
    x[1] <- x[1] + sum(boluses$amount[boluses$time == pc$from])
    dt <- min(t, pc$to) - pc$from
    x <- as.vector(as.matrix(Matrix::expm(Matrix::Matrix(K(pc$pk, pc$rate) * dt))) %*% x)
    last <- pc
  }
  c(Cp = x[1] / last$pk$v1, Ce = x[4])
}

test_that("a physiology event's settings survive the event table and refuse what is out of range", {
  x <- physiologyEventNormal()
  x[["cardiac"]] <- 0.5
  x[["temperature"]] <- 33.5
  x[["altitude"]] <- 2400
  x[["endobronchial"]] <- 1
  s <- physiologyEventEncode(x)
  expect_equal(physiologyEventDecode(s), checkPhysiologyEvent(x))
  expect_null(physiologyEventDecode("liver=1;liver=2"))
  expect_null(physiologyEventDecode("cardiac=50"))
  expect_null(physiologyEventDecode("altitude=-1"))
  expect_null(physiologyEventDecode("endobronchial=0.5"))
  expect_null(physiologyEventDecode("nonsense"))
  expect_null(physiologyEventDecode(strrep("a", 1000)))
  expect_null(physiologyEventDecode(c("liver=1", "liver=1")))
  expect_null(physiologyEventDecode(NA_character_))
  # A missing field is normal
  expect_equal(physiologyEventDecode("cardiac=0.5")[["liver"]], 1)
  expect_match(physiologyEventSummary(x), "Cardiac output x0.5")
  expect_match(physiologyEventSummary(x), "Endobronchial intubation")
  expect_false(grepl("Altitude", physiologyEventSummary(x, gas = FALSE)))
})

test_that("physiologyEventsFrom() reads only readable Physiology rows, in time order, the last of a tie", {
  a <- physiologyEventNormal(); a[["liver"]] <- 0.5
  b <- physiologyEventNormal(); b[["liver"]] <- 2
  ET <- data.frame(Time = c(90, 30, 30, 10, 20),
                   Event = c("Physiology", "Physiology", "Physiology", "CPB Start", "Physiology"),
                   Settings = c(physiologyEventEncode(a), physiologyEventEncode(a),
                                physiologyEventEncode(b), "", "liver=999"))
  pe <- physiologyEventsFrom(ET)
  expect_equal(pe$Time, c(30, 90))
  expect_equal(pe$liver, c(2, 0.5))
  expect_equal(nrow(physiologyEventsFrom(ET[, c("Time", "Event")])), 0)
  expect_equal(nrow(physiologyEventsFrom(NULL)), 0)
})

test_that("the clinical and physiology events interleave on one time line", {
  tl <- physiologyEventTimeline(
    data.frame(Time = c(30, 70), Event = c("CPBStart", "CPBEnd")),
    data.frame(Time = c(50, 70), State = 1:2))
  expect_equal(tl$Time, c(0, 30, 50, 70))
  expect_equal(tl$Event, c("default", "CPBStart", "CPBStart@physiology1",
                           "CPBEnd@physiology2"))
})

test_that("propofol steps to an event's physiology with the amounts carried across (matrix exponential)", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "propofol", ]
  s <- physiologyEventNormal()
  s[["cardiac"]] <- 0.5
  s[["liver"]] <- 0.3
  ET <- data.frame(Time = 40, Event = "Physiology", Settings = physiologyEventEncode(s))
  build <- function(p) getDrugPK("propofol", 70, 170, 50, "male", row, physiology = p)
  PK <- physiologyEventPK(build, physiologyEventsFrom(ET))
  expect_equal(names(PK$PK), c("default", "default@physiology1"))
  p0 <- PK$PK[["default"]]
  p1 <- PK$PK[["default@physiology1"]]
  # Normal physiology before the event: the set built with no physiology
  expect_equal(p0$v1, getDrugPK("propofol", 70, 170, 50, "male", row)$PK[[1]]$v1)
  expect_false(isTRUE(all.equal(p0$v1, p1$v1)))
  expect_false(isTRUE(all.equal(p0$ke0, p1$ke0)))

  DT <- data.frame(Drug = "propofol", Time = c(0, 0, 60, 90), Dose = c(2, 100, 1, 0),
                   Units = c("mg/kg", "mcg/kg/min", "mg/kg", "mcg/kg/min"))
  sim <- simCpCe(DT, ET, PK, maximum = 180, plotRecovery = FALSE)
  w <- sim$wide
  r <- 100 * 70 / 1000
  pieces <- list(list(from = 0, to = 40, pk = p0, rate = r),
                 list(from = 40, to = 60, pk = p1, rate = r),
                 list(from = 60, to = 90, pk = p1, rate = r),
                 list(from = 90, to = 1e9, pk = p1, rate = 0))
  boluses <- data.frame(time = c(0, 60), amount = c(140, 70))
  # Points either side of each switch, not on it (the plotted line carries
  # both values at a switch)
  g <- w$Time[w$Time > 0 & !(w$Time %in% c(40, 60, 90))]
  ref <- vapply(g, function(t) physiologyEventReference(pieces, boluses, t), numeric(2))
  i <- match(g, w$Time)
  expect_lt(max(abs(w$Plasma[i] / ref["Cp", ] - 1)), 1e-9)
  expect_lt(max(abs(w$`Effect Site`[i] / ref["Ce", ] - 1)), 1e-7)
  # The plasma concentration steps at the event (V1 halves); the amount does not
  before <- physiologyEventReference(pieces, boluses, 40 - 1e-9)[["Cp"]]
  after <- physiologyEventReference(pieces, boluses, 40 + 1e-9)[["Cp"]]
  expect_equal(after / before, p0$v1 / p1$v1, tolerance = 1e-6)
})

test_that("physiology events and clinical events switch together (dexmedetomidine on bypass)", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "dexmedetomidine", ]
  s <- physiologyEventNormal()
  s[["cardiac"]] <- 2
  s[["liver"]] <- 0.5
  ET <- data.frame(Time = c(30, 50, 70), Event = c("CPB Start", "Physiology", "CPB End"),
                   Settings = c("", physiologyEventEncode(s), ""))
  build <- function(p) getDrugPK("dexmedetomidine", 7, 65, 0.5, "male", row, physiology = p)
  PK <- physiologyEventPK(build, physiologyEventsFrom(ET))
  sets <- names(PK$PK)
  pre <- PK$PK[[sets[1]]]
  on <- PK$PK[[grep("^CPBStart$", sets, value = TRUE)]]
  onP <- PK$PK[[grep("^CPBStart@physiology1$", sets, value = TRUE)]]
  postP <- PK$PK[[grep("^CPBEnd@physiology1$", sets, value = TRUE)]]
  DT <- data.frame(Drug = "dexmedetomidine", Time = c(0, 0, 90), Dose = c(1, 0.7, 0),
                   Units = c("mcg/kg", "mcg/kg/hr", "mcg/kg/hr"))
  w <- simCpCe(DT, ET, PK, maximum = 240, plotRecovery = FALSE)$wide
  r <- 0.7 * 7 / 60
  pieces <- list(list(from = 0, to = 30, pk = pre, rate = r),
                 list(from = 30, to = 50, pk = on, rate = r),
                 list(from = 50, to = 70, pk = onP, rate = r),
                 list(from = 70, to = 90, pk = postP, rate = r),
                 list(from = 90, to = 1e9, pk = postP, rate = 0))
  boluses <- data.frame(time = 0, amount = 7)
  g <- w$Time[w$Time > 0 & !(w$Time %in% c(30, 50, 70, 90))]
  ref <- vapply(g, function(t) physiologyEventReference(pieces, boluses, t)[["Cp"]], numeric(1))
  expect_lt(max(abs(w$Plasma[match(g, w$Time)] / ref - 1)), 1e-9)
})

test_that("without a physiology event nothing changes, and an event at normal physiology changes nothing", {
  DT <- data.frame(Drug = "propofol", Time = c(0, 0, 60), Dose = c(2, 100, 0),
                   Units = c("mg/kg", "mcg/kg/min", "mcg/kg/min"))
  ET0 <- data.frame(Time = 30, Event = "Intubation", Settings = "")
  plain <- simulateDrugsWithCovariates(DT, ET0, 70, 170, 50, "male", maximum = 120,
                                       plotRecovery = FALSE)
  ET1 <- rbind(ET0, data.frame(Time = 45, Event = "Physiology",
                               Settings = physiologyEventEncode(physiologyEventNormal())))
  same <- simulateDrugsWithCovariates(DT, ET1, 70, 170, 50, "male", maximum = 120,
                                      plotRecovery = FALSE)
  a <- plain$propofol$wide
  b <- same$propofol$wide
  b <- b[b$Time %in% a$Time & !duplicated(b$Time), ]
  a <- a[a$Time %in% b$Time & !duplicated(a$Time), ]
  expect_equal(b$Plasma, a$Plasma, tolerance = 1e-12)
  # With an event, the panel's physiology is not used: normal from 0
  s <- physiologyEventNormal(); s[["liver"]] <- 0.2
  phys <- PHYSIOLOGY_DEFAULT; phys[["liver"]] <- 0.2
  ET2 <- rbind(ET0, data.frame(Time = 45, Event = "Physiology", Settings = physiologyEventEncode(s)))
  ev <- simulateDrugsWithCovariates(DT, ET2, 70, 170, 50, "male", maximum = 120,
                                    plotRecovery = FALSE, physiology = phys)
  w <- ev$propofol$wide
  early <- w[w$Time > 0 & w$Time < 45, ]
  # (on the same time line: the event engine's, with an event at 45)
  ref <- same$propofol$wide
  expect_equal(early$Plasma, ref$Plasma[match(early$Time, ref$Time)], tolerance = 1e-12)
  late <- w[w$Time > 50, ]
  expect_gt(min(late$Plasma / ref$Plasma[match(late$Time, ref$Time)]), 1)
})

test_that("a drug the sliders cannot switch during a run says so and keeps normal physiology", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "phenytoin", ]
  s <- physiologyEventNormal(); s[["liver"]] <- 0.5
  build <- function(p) getDrugPK("phenytoin", 70, 170, 50, "male", row, physiology = p)
  PK <- physiologyEventPK(build, physiologyEventsFrom(
    data.frame(Time = 60, Event = "Physiology", Settings = physiologyEventEncode(s))))
  expect_type(PK$physiologyStepsSkipped, "character")
  expect_null(PK[["physiologyEvents"]])
})
