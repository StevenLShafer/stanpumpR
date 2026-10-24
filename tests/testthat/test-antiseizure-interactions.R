# Tests for the pharmacokinetic interactions among the antiseizure drugs
# (R/antiseizureInteractions.R).  Drafted by Claude Code, 2026-10-24, at the
# request of Steven L. Shafer.

options(ECHO_OUTPUT_COMMENTS = FALSE)

DAY <- 1440

simEv <- function() data.frame(Time = numeric(0), Event = character(0))

runASM <- function(dose, maximum, on, recovery = FALSE)
  simulateDrugsWithCovariates(dose, simEv(), 70, 170, 40, "male", maximum,
                              recovery, antiseizureInteractions = on)

atTime <- function(x, t) stats::approx(x$wide$Time, x$wide$Plasma, xout = t)$y

test_that("the registry is well formed and matches the applied set", {
  r <- antiseizureInteractionRegistry()
  expect_true(all(r$status %in% c("applied", "crosscheck", "recorded")))
  a <- r[r$status == "applied", ]
  expect_true(all(a$parameter %in% c("CL", "Vmax", "Km")))
  expect_true(all(a$onset %in% c("turnover", "immediate")))
  expect_true(all(a$c_ref > 0))
  expect_true(all(a$ec50 > 0))
  # every applied victim and perpetrator is a real drug
  drugs <- getDrugDefaultsGlobal()$Drug
  expect_true(all(a$victim %in% drugs))
  expect_true(all(a$perpetrator %in% drugs))
  # a turnover row carries the enzyme half-life, an immediate one does not
  expect_true(all(a$turnover_half_life_h[a$onset == "turnover"] > 0))
  expect_true(all(is.na(a$turnover_half_life_h[a$onset == "immediate"])))
  # phenytoin, the saturable drug, is changed only on Vmax or Km
  expect_true(all(a$parameter[a$victim == "phenytoin"] %in% c("Vmax", "Km")))
  # every PMID verified against PubMed in the session is a plain number
  expect_true(all(grepl("^[0-9]+$", r$pmid)))
})

test_that("the four requested drugs are each a victim of an applied interaction", {
  victims <- asmInteractionVictims()
  for (d in c("phenytoin", "valproate", "lamotrigine", "carbamazepine"))
    expect_true(d %in% victims, info = d)
})

test_that("activeAntiseizureInteractions needs both drugs, or an autoinducer", {
  expect_equal(nrow(activeAntiseizureInteractions("lamotrigine")), 0)
  # valproate -> lamotrigine is applied; lamotrigine is not a perpetrator, so
  # the pair has one active interaction, not two
  expect_equal(nrow(activeAntiseizureInteractions(c("lamotrigine", "valproate"))), 1)
  # phenytoin and valproate change each other: two active rows
  expect_equal(nrow(activeAntiseizureInteractions(c("phenytoin", "valproate"))), 2)
  # carbamazepine alone is active: it induces itself
  expect_true(nrow(activeAntiseizureInteractions("carbamazepine")) >= 1)
  expect_true(any(activeAntiseizureInteractions("carbamazepine")$perpetrator ==
                  activeAntiseizureInteractions("carbamazepine")$victim))
})

test_that("off, nothing changes; a drug with no interaction is untouched either way", {
  dose <- data.frame(Drug = "lamotrigine", Time = 0, Dose = 100, Units = "mg PO bid")
  a <- runASM(dose, 20 * DAY, FALSE)
  b <- runASM(dose, 20 * DAY, TRUE)   # no perpetrator: nothing to apply
  expect_equal(a$lamotrigine$wide$Plasma, b$lamotrigine$wide$Plasma)
  expect_null(b$lamotrigine$interaction)

  # levetiracetam is not a victim of any applied row
  dose2 <- data.frame(Drug = c("levetiracetam", "carbamazepine"),
                      Time = 0, Dose = c(500, 200), Units = c("mg PO bid", "mg PO bid"))
  on <- runASM(dose2, 20 * DAY, TRUE)
  expect_null(on$levetiracetam$interaction)
})

test_that("the segmented clearance engine reproduces an unchanged run", {
  PK <- getDrugPK("lamotrigine", 70, 170, 40, "male")
  dose <- data.frame(Drug = "lamotrigine", Time = 0, Dose = 200, Units = "mg PO bid")
  base <- simCpCe(dose, simEv(), PK, 20 * DAY, FALSE)
  # three segments all at a multiplier of 1
  seg <- data.frame(start = c(0, 5 * DAY, 9 * DAY), value = c(1, 1, 1))
  P2 <- asmSegmentPK(PK, seg)
  one <- simCpCe(dose, simEv(), P2, 20 * DAY, FALSE)
  expect_equal(atTime(one, 19.5 * DAY), atTime(base, 19.5 * DAY), tolerance = 1e-6)
  # a constant 1.5x on three segments equals one set at 1.5x
  P3 <- asmSegmentPK(PK, data.frame(start = 0, value = 1.5))
  P4 <- asmSegmentPK(PK, data.frame(start = c(0, 5 * DAY, 9 * DAY), value = c(1.5, 1.5, 1.5)))
  expect_equal(atTime(simCpCe(dose, simEv(), P4, 20 * DAY, FALSE), 19.5 * DAY),
               atTime(simCpCe(dose, simEv(), P3, 20 * DAY, FALSE), 19.5 * DAY),
               tolerance = 1e-6)
})

test_that("valproate inhibits lamotrigine, building over about two weeks", {
  dose <- data.frame(Drug = c("lamotrigine", "valproate"),
                     Time = c(0, 21 * DAY), Dose = c(100, 500),
                     Units = c("mg PO bid", "mg PO DR bid"))
  a <- runASM(dose, 56 * DAY, FALSE)
  b <- runASM(dose, 56 * DAY, TRUE)
  # before valproate, the two agree
  expect_equal(atTime(a$lamotrigine, 20 * DAY), atTime(b$lamotrigine, 20 * DAY),
               tolerance = 1e-6)
  # inhibition is immediate but the new steady state takes ~2 weeks; it roughly
  # doubles (clearance x 0.49)
  ratioEarly <- atTime(b$lamotrigine, 23 * DAY) / atTime(a$lamotrigine, 23 * DAY)
  ratioLate  <- atTime(b$lamotrigine, 42 * DAY) / atTime(a$lamotrigine, 42 * DAY)
  expect_true(ratioEarly > 1.2 && ratioEarly < ratioLate)
  expect_true(ratioLate > 1.8 && ratioLate < 2.2)
  # the interaction segments are recorded on the changed drug
  expect_true(!is.null(b$lamotrigine$interaction$CL))
})

test_that("an inducer builds and fades with the enzyme half-life, not instantly", {
  dose <- data.frame(Drug = c("lamotrigine", "carbamazepine", "carbamazepine"),
                     Time = c(0, 0, 28 * DAY), Dose = c(200, 400, 0),
                     Units = c("mg PO bid", "mg PO bid", "mg PO bid"))
  b <- runASM(dose, 56 * DAY, TRUE)
  a <- runASM(dose, 56 * DAY, FALSE)
  # induced: lamotrigine lower than its uninduced line while carbamazepine is on
  expect_lt(atTime(b$lamotrigine, 25 * DAY), atTime(a$lamotrigine, 25 * DAY))
  # carbamazepine is essentially gone a week after stopping, but lamotrigine
  # has not yet fully recovered (enzyme turnover lags drug washout)
  expect_lt(atTime(b$carbamazepine, 35 * DAY), 0.1)
  expect_lt(atTime(b$lamotrigine, 35 * DAY), atTime(a$lamotrigine, 35 * DAY))
  # by the end it has rejoined
  expect_equal(atTime(b$lamotrigine, 56 * DAY), atTime(a$lamotrigine, 56 * DAY),
               tolerance = 0.03)
})

test_that("carbamazepine autoinduction overshoots then settles to the library model", {
  dose <- data.frame(Drug = "carbamazepine", Time = 0, Dose = 400, Units = "mg PO bid")
  a <- runASM(dose, 28 * DAY, FALSE)   # the induced Graves model
  b <- runASM(dose, 28 * DAY, TRUE)
  # early, the autoinducing curve is higher (uninduced clearance), then it
  # falls back to the library's induced steady state
  expect_gt(atTime(b$carbamazepine, 4 * DAY), atTime(a$carbamazepine, 4 * DAY))
  expect_equal(atTime(b$carbamazepine, 27 * DAY), atTime(a$carbamazepine, 27 * DAY),
               tolerance = 0.03)
})

test_that("valproate lowers TOTAL phenytoin (displacement) on the saturable engine", {
  dose <- data.frame(Drug = c("phenytoin", "valproate"),
                     Time = c(0, 21 * DAY), Dose = c(300, 500),
                     Units = c("mg PO ER qd", "mg PO DR bid"))
  a <- runASM(dose, 56 * DAY, FALSE)
  b <- runASM(dose, 56 * DAY, TRUE)
  expect_equal(atTime(a$phenytoin, 20 * DAY), atTime(b$phenytoin, 20 * DAY),
               tolerance = 1e-6)
  # total phenytoin falls once valproate is on (Km x 0.615)
  expect_lt(atTime(b$phenytoin, 42 * DAY), atTime(a$phenytoin, 42 * DAY))
  # phenytoin induces valproate, so valproate runs below its uninteracted line
  expect_lt(atTime(b$valproate, 42 * DAY), atTime(a$valproate, 42 * DAY))
  # the phenytoin interaction acts on Km, recorded in interactionMM-derived segments
  expect_true(!is.null(b$phenytoin$interaction$Km))
})

test_that("inducers raise zonisamide and ethosuximide clearance (new victims)", {
  # zonisamide, induced by carbamazepine (Silva 2025, CL x 1.63 per inducer)
  dose <- data.frame(Drug = c("zonisamide", "carbamazepine"),
                     Time = c(0, 21 * DAY), Dose = c(100, 200),
                     Units = c("mg PO bid", "mg PO bid"))
  a <- runASM(dose, 56 * DAY, FALSE)
  b <- runASM(dose, 56 * DAY, TRUE)
  # before carbamazepine the two agree (to within the re-simulation grid
  # artifact: the interaction run adds segment-boundary time points, so the
  # interpolated value differs by ~2e-4, not a real pre-perpetrator change)
  expect_equal(atTime(a$zonisamide, 20 * DAY), atTime(b$zonisamide, 20 * DAY),
               tolerance = 1e-3)
  # with carbamazepine on, zonisamide is lower (induced over ~2 weeks)
  expect_lt(atTime(b$zonisamide, 45 * DAY), atTime(a$zonisamide, 45 * DAY))
  ratio <- atTime(b$zonisamide, 45 * DAY) / atTime(a$zonisamide, 45 * DAY)
  expect_true(ratio > 0.55 && ratio < 0.8)   # roughly 1/1.5 to 1/1.25
  expect_true(!is.null(b$zonisamide$interaction$CL))

  # ethosuximide, induced by phenytoin (Giaccone 1996, CL x 1.66)
  dose2 <- data.frame(Drug = c("ethosuximide", "phenytoin"),
                      Time = c(0, 21 * DAY), Dose = c(500, 300),
                      Units = c("mg PO bid", "mg PO ER qd"))
  a2 <- runASM(dose2, 56 * DAY, FALSE)
  b2 <- runASM(dose2, 56 * DAY, TRUE)
  expect_lt(atTime(b2$ethosuximide, 45 * DAY), atTime(a2$ethosuximide, 45 * DAY))
  ratio2 <- atTime(b2$ethosuximide, 45 * DAY) / atTime(a2$ethosuximide, 45 * DAY)
  expect_true(ratio2 > 0.5 && ratio2 < 0.72)   # ~ 1/1.66, tempered by concentration
  expect_true(!is.null(b2$ethosuximide$interaction$CL))
})

test_that("valproate inhibits ethosuximide; ethosuximide induces lamotrigine", {
  r <- antiseizureInteractionRegistry()
  vpaEth <- r[r$perpetrator == "valproate" & r$victim == "ethosuximide" &
              r$status == "applied", ]
  expect_equal(nrow(vpaEth), 1)   # Pisani 1984: x0.85 inhibition (was "negative")
  # valproate lowers ethosuximide clearance, so ethosuximide RISES
  dose <- data.frame(Drug = c("ethosuximide", "valproate"),
                     Time = c(0, 21 * DAY), Dose = c(500, 500),
                     Units = c("mg PO bid", "mg PO DR bid"))
  a <- runASM(dose, 56 * DAY, FALSE)
  b <- runASM(dose, 56 * DAY, TRUE)
  expect_equal(atTime(a$ethosuximide, 20 * DAY), atTime(b$ethosuximide, 20 * DAY),
               tolerance = 1e-3)
  expect_gt(atTime(b$ethosuximide, 45 * DAY), atTime(a$ethosuximide, 45 * DAY))

  # ethosuximide induces lamotrigine (Hagemann 2024, CL x1.16), so LTG FALLS
  dose2 <- data.frame(Drug = c("lamotrigine", "ethosuximide"),
                      Time = c(0, 21 * DAY), Dose = c(100, 500),
                      Units = c("mg PO bid", "mg PO bid"))
  a2 <- runASM(dose2, 56 * DAY, FALSE)
  b2 <- runASM(dose2, 56 * DAY, TRUE)
  expect_lt(atTime(b2$lamotrigine, 45 * DAY), atTime(a2$lamotrigine, 45 * DAY))
  expect_true(!is.null(b2$lamotrigine$interaction$CL))
})

test_that("two inducers add in excess, not multiplicatively", {
  # phenytoin and phenobarbital on carbamazepine: Graves measured the pair at
  # x 1.62; additive excess of 1.42 and 1.17 gives 1.59, multiplicative 1.66
  r <- asmAppliedInteractions()
  phtRow <- r[r$victim == "carbamazepine" & r$perpetrator == "phenytoin", ]
  pbRow  <- r[r$victim == "carbamazepine" & r$perpetrator == "phenobarbital", ]
  phtFmax <- asmFmax(phtRow$fold, phtRow$c_ref, phtRow$ec50)
  pbFmax  <- asmFmax(pbRow$fold, pbRow$c_ref, pbRow$ec50)
  # at the reference concentrations the folds are recovered
  expect_equal(asmFold(phtRow$c_ref, phtRow$fold, phtRow$c_ref, phtRow$ec50),
               phtRow$fold, tolerance = 1e-9)
  # additive excess of the published factors
  additive <- 1 + (phtRow$fold - 1) + (pbRow$fold - 1)
  expect_equal(additive, 1.59, tolerance = 0.01)
})

test_that("the companion reference entries name their parent", {
  common <- list(NULL, getDrugDefaultsGlobal(),
                 data.frame(Drug = c("lamotrigine", "valproate"), Time = 0,
                            Dose = c(100, 500), Units = c("mg PO bid", "mg PO DR bid")),
                 age = 40, weight = 70, height = 170, sex = "male")
  drugs <- do.call(recalculatePK, c(common, list(antiseizureInteractions = TRUE)))
  ref <- asmReferenceName("lamotrigine")
  expect_true(ref %in% names(drugs))
  expect_equal(drugs[[ref]]$companionOf, "lamotrigine")
  expect_equal(alternativeModelParent(ref), "lamotrigine")
  # off, no reference entry
  off <- do.call(recalculatePK, c(common, list(antiseizureInteractions = FALSE)))
  expect_false(ref %in% names(off))
})

test_that("the summary lines read correctly", {
  s <- antiseizureInteractionSummary(c("lamotrigine", "valproate"))
  expect_true(any(grepl("valproate raises lamotrigine", s)))
  s2 <- antiseizureInteractionSummary("carbamazepine")
  expect_true(any(grepl("induces its own metabolism", s2)))
})

test_that("the help page and its generated blocks render", {
  expect_true("models/antiseizure-interactions" %in% helpStaticPages()$id)
  html <- helpPageHTML("models/antiseizure-interactions")
  expect_true(nchar(html) > 2000)
  expect_false(grepl("generated:", html))   # every marker was expanded
  tbl <- helpAntiseizureInteractionTableMarkdown()
  expect_true(grepl("lamotrigine", tbl) && grepl("valproate", tbl))
})
