# mirabegron: see R/drugs_mirabegron.R (FDA Clinical Pharmacology Review of
# NDA 202611, Tables 7 and 8, the sponsor's population model 178-PK-015).
noEvents <- data.frame(Time = numeric(0), Event = character(0))
# the serum creatinine giving the reference Cockcroft-Gault clearance, 92
# mL/min, for a 70 kg woman of 60
scrRef <- (140 - 60) * 70 * 0.85 / (72 * 92)

test_that("Tables 7 and 8 at the reference woman (70 kg, 60 y, CrCL 92 mL/min)", {
  m <- mirabegron(70, 165, 60, "female", adjustToFFM = FALSE, creatinine = scrRef)
  p <- m$PK$default
  expect_equal(p$v1, 19.8)
  expect_equal(p$cl1 * 60, 50.5)
  # the library's v2 is the fast compartment (K13/K31), v3 the slow (K12/K21)
  expect_equal(p$v2, 19.8 * 3.15 / 0.374)
  expect_equal(p$cl2 * 60, 19.8 * 3.15)
  expect_equal(p$v3, 19.8 * 2.39 / 0.0399)
  expect_equal(p$cl3 * 60, 19.8 * 2.39)
  expect_equal(p$ka_PO * 60, 0.109)
  expect_equal(p$bioavailability_PO, 0.220)
  expect_equal(p$tlag_PO, 0)
  expect_equal(m$zeroOrderPO$duration, 2.79 * 60)
  expect_equal(m$oralSaturation[c("form", "exponent", "Dref")],
               list(form = "power", exponent = 0.443, Dref = 50))
  expect_equal(c(m$tPeak, m$MEAC, m$typical, m$lowerTypical, m$upperTypical), rep(0, 5))
  # Vss at the reference: 1373 L for a woman (label, IV: about 1670 L)
  expect_equal(round(p$v1 + p$v2 + p$v3, 1), 1372.6)
  # the terminal half-life at 50 mg, 35.0 h
  k10 <- p$cl1 / p$v1; k12 <- p$cl2 / p$v1; k21 <- p$cl2 / p$v2
  k13 <- p$cl3 / p$v1; k31 <- p$cl3 / p$v3
  A <- matrix(c(-(k10 + k12 + k13), k12, k13, k21, -k21, 0, k31, 0, -k31), 3)
  expect_equal(round(log(2) / min(-eigen(A)$values) / 60, 1), 35.0)
})

test_that("covariates: weight and sex on CL and V1, sex, age and CrCL on F1", {
  f <- mirabegron(70, 165, 60, "female", FALSE, creatinine = scrRef)$PK$default
  # a man: CL and V1 x 1.142, F1 x 0.830 (at the same creatinine clearance)
  scrMale <- (140 - 60) * 70 / (72 * 92)
  m <- mirabegron(70, 178, 60, "male", FALSE, creatinine = scrMale)$PK$default
  expect_equal(m$cl1 / f$cl1, 1.142)
  expect_equal(m$v1 / f$v1, 1.142)
  expect_equal(m$v3 / f$v3, 1.142)
  expect_equal(m$bioavailability_PO, 0.220 * 0.830)
  # the review's own statements (page 144): AUC 27.5% lower in men
  expect_equal(round(1 - (m$bioavailability_PO / m$cl1) / (f$bioavailability_PO / f$cl1), 3), 0.273)
  # weight 30 kg: AUC +52.8%
  w30 <- mirabegron(30, 140, 60, "female", FALSE, creatinine = (80 * 30 * 0.85) / (72 * 92))$PK$default
  expect_equal(round(f$cl1 / w30$cl1, 3), 1.528)
  # creatinine clearance 25 and 142 mL/min: AUC +39.9% and -10.6% (through F1)
  crclF <- function(crcl) mirabegron(70, 165, 60, "female", FALSE,
                                     creatinine = 80 * 70 * 0.85 / (72 * crcl))$PK$default$bioavailability_PO
  # (the review's 25 mL/min lies below the studied 27, where the model holds
  # the term, so its arithmetic is checked on the exponent itself)
  expect_equal(round((25 / 92)^-0.258, 4), 1.3996)   # "increased by 39.9%"
  expect_equal(crclF(30) / 0.220, (30 / 92)^-0.258)
  expect_equal(round(crclF(142) / 0.220, 3), 0.894)
  # age on F1, (age/60)^0.245, the creatinine held at the reference clearance
  ageF <- function(age) mirabegron(70, 165, age, "female", FALSE,
                                   creatinine = (140 - age) * 70 * 0.85 / (72 * 92))$PK$default$bioavailability_PO
  expect_equal(ageF(90) / 0.220, (90 / 60)^0.245)
  expect_equal(ageF(30) / 0.220, (30 / 60)^0.245)
})

test_that("age and creatinine clearance are held to the studied ranges", {
  # age 10 reads as 18 (and the age floor keeps F1 above 0 at age 0)
  at <- function(age, scr) mirabegron(40, 150, age, "female", FALSE, creatinine = scr)$PK$default$bioavailability_PO
  crcl18 <- creatinineClearanceCG(40, 10, "female", adultEquivalentCreatinine(0.5, 10, "female"))
  expect_equal(at(10, 0.5), 0.220 * (18 / 60)^0.245 *
                 (min(max(crcl18, 27), 262) / 92)^-0.258)
  expect_true(at(0, NULL) > 0 && is.finite(at(0, NULL)))
  # a very high or very low clearance reads as 262 or 27 mL/min
  hi <- mirabegron(70, 165, 60, "female", FALSE, creatinine = 0.1)$PK$default$bioavailability_PO
  lo <- mirabegron(70, 165, 60, "female", FALSE, creatinine = 15)$PK$default$bioavailability_PO
  expect_equal(hi, 0.220 * (262 / 92)^-0.258)
  expect_equal(lo, 0.220 * (27 / 92)^-0.258)
})

test_that("the switch on evaluates the weight covariate at the pharmacokinetic weight", {
  size <- pkSizeFactors(120, 175, 50, "male", TRUE)
  on  <- mirabegron(120, 175, 50, "male", creatinine = 1)$PK$default
  off <- mirabegron(120, 175, 50, "male", FALSE, creatinine = 1)$PK$default
  expect_equal(on$cl1 * 60, 50.5 * 1.142 * (size$pkWeight / 70)^0.5)
  expect_equal(off$cl1 * 60, 50.5 * 1.142 * (120 / 70)^0.5)
  expect_equal(on$bioavailability_PO, 0.220 * 0.830 * (50 / 60)^0.245 *
                 (creatinineClearanceCG(size$pkWeight, 50, "male", 1) / 92)^-0.258)
  expect_equal_rounded(mirabegron(70, 170, 40, "male")$PK$default$cl1 * 60, 57.67101431)
})

test_that("each dose is scaled by (dose/50)^0.443 and released over 2.79 h: AUC = F D / CL", {
  PK <- getDrugPK("mirabegron", 70, 165, 60, "female", adjustToFFM = FALSE, creatinine = scrRef)
  expect_true(isTRUE(PK$zeroOrderPO))
  run <- function(d) simCpCe(data.frame(Drug = "mirabegron", Time = 0, Dose = d, Units = "mg PO"),
                             noEvents, PK, 40 * 1440, FALSE)$wide
  auc <- function(w) sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2) / 60
  for (d in c(25, 50, 100))
    expect_equal(auc(run(d)), 0.220 * (d / 50)^0.443 * d * 1000 / 50.5, tolerance = 0.002)
  # the review: AUC rises 20-fold from 25 to 200 mg (8^1.443)
  expect_equal(round(auc(run(200)) / auc(run(25)), 1), 20.1)
})

test_that("the curve matches a matrix-exponential solution of the source's equations", {
  PK <- getDrugPK("mirabegron", 70, 165, 60, "female", adjustToFFM = FALSE, creatinine = scrRef)
  w <- simCpCe(data.frame(Drug = "mirabegron", Time = 0, Dose = 50, Units = "mg PO"),
               noEvents, PK, 3 * 1440, FALSE)$wide
  w <- w[w$Time > 0, ]
  expm <- function(M) { e <- eigen(M); Re(e$vectors %*% diag(exp(e$values)) %*% solve(e$vectors)) }
  # gut, central, fast and slow peripheral; per hour
  k10 <- 50.5 / 19.8
  A <- matrix(0, 4, 4)
  A[1, 1] <- -0.109; A[2, 1] <- 0.109
  A[2, 2] <- -(k10 + 2.39 + 3.15); A[2, 3] <- 0.374; A[2, 4] <- 0.0399
  A[3, 2] <- 3.15; A[3, 3] <- -0.374; A[4, 2] <- 2.39; A[4, 4] <- -0.0399
  aug <- rbind(cbind(A, c(1, 0, 0, 0)), 0)          # a unit-rate input into the gut
  rate <- 50 * 0.220 * 1000 / 2.79                    # mcg/h released over D1
  exact <- vapply(w$Time / 60, function(t) {
    if (t <= 2.79) return((expm(aug * t) %*% c(0, 0, 0, 0, 1))[2] * rate / 19.8)
    x <- expm(aug * 2.79) %*% c(0, 0, 0, 0, 1)
    (expm(A * (t - 2.79)) %*% x[1:4])[2] * rate / 19.8
  }, numeric(1))
  expect_lt(max(abs(w$Plasma - exact)) / max(exact), 1e-10)
  expect_true(all(is.na(w$`Effect Site`)))
  # Tmax 3.6 h (label: about 3.5 h)
  expect_equal(round(w$Time[which.max(w$Plasma)] / 60, 1), 3.6)
})

test_that("steady state at 50 mg once daily reproduces the sponsor's simulation (Figure 20)", {
  ss <- function(sex, height, scr) {
    PK <- getDrugPK("mirabegron", 70, height, 60, sex, adjustToFFM = FALSE, creatinine = scr)
    w <- simCpCe(data.frame(Drug = "mirabegron", Time = 0, Dose = 50, Units = "mg PO qd"),
                 noEvents, PK, 21 * 1440, FALSE)$wide
    last <- w[w$Time >= 20 * 1440, ]
    c(max(last$Plasma), min(last$Plasma))
  }
  # read off the review's Figure 2.3.1.2.1 and Figure 20: woman peaks near
  # 12.8 ng/mL, trough near 5.8; man near 9.4 and 4.1
  woman <- ss("female", 165, scrRef)
  man   <- ss("male", 178, (140 - 60) * 70 / (72 * 92))
  expect_equal(woman, c(12.8, 5.8), tolerance = 0.05)
  expect_equal(man, c(9.4, 4.1), tolerance = 0.05)
})

test_that("units: oral tablet, once daily; no effect site; finite for any legal patient", {
  expect_equal(getDrugDefaults("mirabegron")$Units[[1]], c("mg PO", "mg PO qd"))
  expect_true("creatinine" %in% names(formals(mirabegron)))
  for (p in list(c(0.5, 45, 0), c(3, 50, 0.1), c(250, 220, 90), c(40, 150, 18)))
    for (sex in c("male", "female"))
      for (switch in c(TRUE, FALSE)) {
        s <- mirabegron(p[1], p[2], p[3], sex, switch)$PK$default
        expect_true(all(is.finite(unlist(s))) && all(unlist(s[c("v1", "v2", "v3")]) > 0))
        expect_true(s$bioavailability_PO > 0 && s$bioavailability_PO < 1)
      }
})
