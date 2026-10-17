# The Minto / Schnider switch (R/alternativeModels.R): Schnider's propofol
# and Minto's remifentanil models run beside the library's Eleveld models as
# companion entries of the drug list, drawn as a second line in the same
# panel and, under TCI, as a second pump on the same rate panel.  Drafted by
# Claude Code (Claude Fable 5.1), 2026-10-12, at the request of Steven L.
# Shafer.

local_mocked_bindings(outputComments = function(...) {})

dd <- getDrugDefaultsGlobal(FALSE)
ddHelp <- getDrugDefaultsGlobal()
noEvents <- data.frame(Time = double(), Event = character())
rowOf <- function(drug) dd[dd$Drug == drug, ][1, ]

# A bolus and an hour's infusion of each drug, and the two TCI targets
dosesIV <- data.frame(
  Drug  = c("propofol", "propofol", "propofol", "remifentanil", "remifentanil", "remifentanil"),
  Time  = c(0, 0, 60, 0, 0, 60),
  Dose  = c(2, 100, 0, 1, 0.2, 0),
  Units = c("mg/kg", "mcg/kg/min", "mcg/kg/min", "mcg/kg", "mcg/kg/min", "mcg/kg/min")
)
dosesTCI <- data.frame(
  Drug  = c("propofol", "propofol", "remifentanil", "remifentanil"),
  Time  = c(0, 60, 0, 60),
  Dose  = c(3, 0, 4, 0),
  Units = TCI_UNIT_EFFECT
)

simulate <- function(DT, maximum = 120, recovery = TRUE, alternativeModels = TRUE,
                     age = 40, weight = 70, height = 170, sex = "male", cache = NULL) {
  drugs <- recalculatePK(NULL, dd, DT, age, weight, height, sex,
                         alternativeModels = alternativeModels)
  processdoseTable(DT, noEvents, drugs, maximum, recovery, cache = cache)
}
plotOf <- function(drugs, maximum = 120, ...) {
  args <- utils::modifyList(
    list(
      drugs = drugs, events = noEvents, drugDefaults = dd, eventDefaults = getEventDefaults(),
      xBreaks = seq(0, maximum, by = 10), xLabels = seq(0, maximum, by = 10), xMaximum = maximum,
      plasmaLinetype = "dashed", effectsiteLinetype = "solid"
    ),
    list(...)
  )
  do.call(simulationPlot, args)
}
seriesAtTime <- function(entry, site, t) {
  r <- entry$results[entry$results$Site == site, ]
  stats::approx(r$Time, r$Y, t)$y
}

# --- The models ---------------------------------------------------------------

test_that("the Schnider propofol model is the one STANPUMP ran, with Schnider 1999's tPeak", {
  X <- propofolSchnider(70, 170, 40, "male")
  k <- X$PK[[PK_EVENT_DEFAULT]]
  lbm <- lbmJames(70, 170, "male")
  expect_equal_rounded(4.27, k$v1)
  expect_equal_rounded(18.9 - 0.391 * (40 - 53), k$v2)
  expect_equal_rounded(238, k$v3)
  expect_equal_rounded(1.89 + 0.0456 * (70 - 77) - 0.0681 * (lbm - 59) + 0.0264 * (170 - 177), k$cl1)
  expect_equal_rounded(1.638135, k$cl1, tolerance = 1e-6)
  expect_equal_rounded(1.29 - 0.024 * (40 - 53), k$cl2)
  expect_equal_rounded(0.836, k$cl3)
  expect_equal(X$tPeak, 1.6)
  expect_null(X$ke0)
  expect_match(X$reference, "Schnider TW et al., Anesthesiology 1998")
  # A woman's lean body mass is James's other line
  Xf <- propofolSchnider(60, 165, 40, "female")
  expect_equal_rounded(1.89 + 0.0456 * (60 - 77) - 0.0681 * (lbmJames(60, 165, "female") - 59) + 0.0264 * (165 - 177),
                       Xf$PK[[1]]$cl1)
  # Solved through getDrugPK(): ke0 near the 0.456/min Schnider 1999 reported
  entry <- alternativeModelEntry("propofol", 70, 170, 40, "male", rowOf("propofol"))
  expect_equal_rounded(0.4079446, entry$PK[[1]]$ke0, tolerance = 1e-5)
  expect_equal(entry$tPeak, 1.6)
})

test_that("the Minto remifentanil model is the one STANPUMP ran, with its age-dependent ke0", {
  X <- remifentanilMinto(70, 170, 40, "male")
  k <- X$PK[[PK_EVENT_DEFAULT]]
  lbm <- lbmJames(70, 170, "male")
  expect_equal_rounded(5.1 - 0.0201 * (40 - 40) + 0.072 * (lbm - 55), k$v1)
  expect_equal_rounded(9.82 - 0.0811 * (40 - 40) + 0.108 * (lbm - 55), k$v2)
  expect_equal_rounded(5.42, k$v3)
  expect_equal_rounded(2.6 - 0.0162 * (40 - 40) + 0.0191 * (lbm - 55), k$cl1)
  expect_equal_rounded(2.05, k$cl2)
  expect_equal_rounded(0.076, k$cl3)
  expect_equal_rounded(0.595, X$ke0)
  expect_null(X$tPeak)
  expect_match(X$reference, "Minto CF et al., Anesthesiology 1997")
  expect_equal_rounded(0.595 - 0.007 * 40, remifentanilMinto(70, 170, 80, "male")$ke0)
  expect_equal_rounded(0.595 + 0.007 * 20, remifentanilMinto(70, 170, 20, "male")$ke0)
})

test_that("getDrugPK() takes Minto's ke0 as given and solves the time to peak effect from it", {
  entry <- alternativeModelEntry("remifentanil", 70, 170, 40, "male", rowOf("remifentanil"))
  k <- entry$PK[[1]]
  expect_equal_rounded(0.595, k$ke0)
  expect_equal_rounded(1.433857, entry$tPeak, tolerance = 1e-5)
  # tPeak is where the unit-bolus effect site peaks
  ce <- function(t) CE(t, k$e_coef_bolus_l1, k$e_coef_bolus_l2, k$e_coef_bolus_l3, k$e_coef_bolus_ke0,
                       k$lambda_1, k$lambda_2, k$lambda_3, k$ke0)
  expect_gt(ce(entry$tPeak), ce(entry$tPeak - 0.1))
  expect_gt(ce(entry$tPeak), ce(entry$tPeak + 0.1))
  # A slower ke0 at 80 years peaks later
  older <- alternativeModelEntry("remifentanil", 70, 170, 80, "male", rowOf("remifentanil"))
  expect_equal_rounded(0.315, older$PK[[1]]$ke0)
  expect_gt(older$tPeak, 2)
  # vdPeakEffect is read at the solved tPeak, not at a placeholder of zero
  expect_gt(1 / ce(entry$tPeak), 0)
  # The TCI controller searches out to it
  expect_equal(tciKinetics(entry)$tPeak, entry$tPeak)
})

test_that("a model may leave tPeak out only when it supplies ke0", {
  model <- remifentanilMinto(70, 170, 40, "male")
  model$ke0 <- NULL
  expect_error(
    getDrugPK("remifentanil", 70, 170, 40, "male", rowOf("remifentanil"), model = model),
    "may leave tPeak out only when it supplies ke0"
  )
  # A tPeak of 0 beside a ke0 still means "no tPeak reported" (desmetramadol)
  model <- remifentanilMinto(70, 170, 40, "male")
  model$tPeak <- 0
  pk <- getDrugPK("remifentanil", 70, 170, 40, "male", rowOf("remifentanil"), model = model)
  expect_equal(pk$tPeak, 0)
  expect_equal_rounded(0.595, pk$PK[[1]]$ke0)
})

test_that("the library's own propofol and remifentanil are unchanged", {
  # The companion is a separate entry; getDrugPK() on the drug itself still
  # returns Eleveld's parameters and solves ke0 from tPeak as before.
  pk <- getDrugPK("propofol", 70, 170, 40, "male", rowOf("propofol"))
  expect_equal(pk$tPeak, 1.6)
  expect_match(pk$reference, "Eleveld")
  expect_gt(pk$PK[[1]]$v1, 6)
  pk <- getDrugPK("remifentanil", 70, 170, 40, "male", rowOf("remifentanil"))
  expect_equal(pk$tPeak, 1.6)
  expect_match(pk$reference, "Eleveld")
})

# --- Names, colours, validity ------------------------------------------------

test_that("companion names, their parents and their colours", {
  expect_equal(alternativeModelName("propofol"), "propofol (Schnider)")
  expect_equal(alternativeModelName("remifentanil"), "remifentanil (Minto)")
  expect_true(is.na(alternativeModelName("fentanyl")))
  expect_equal(
    alternativeModelParent(c("propofol (Schnider)", "remifentanil (Minto) TCI", "propofol", "fentanyl TCI")),
    c("propofol", "remifentanil TCI", "propofol", "fentanyl TCI")
  )
  # A light colour is darkened and a dark one lightened; both stay valid
  yellow <- alternativeModelColor("#FFCC00")
  blue   <- alternativeModelColor("#0000C0")
  expect_match(yellow, "^#[0-9A-Fa-f]{6}$")
  expect_match(blue, "^#[0-9A-Fa-f]{6}$")
  expect_lt(sum(grDevices::col2rgb(yellow)), sum(grDevices::col2rgb("#FFCC00")))
  expect_gt(sum(grDevices::col2rgb(blue)), sum(grDevices::col2rgb("#0000C0")))
  expect_false(identical(yellow, "#FFCC00"))
  expect_equal(ALTERNATIVE_MODELS_LABEL, "Minto / Schnider")
})

test_that("a model whose terms give a non-positive parameter is reported, not simulated", {
  expect_length(alternativeModelProblems(c("propofol", "remifentanil", "fentanyl"), 70, 170, 40, "male"), 0)
  # A six-month-old infant: Schnider's clearance intercepts make CL1 negative.
  # (A five-year-old of 20 kg is still, just, positive: 0.33 L/min.)
  problems <- alternativeModelProblems(c("propofol", "remifentanil"), 7, 65, 0.5, "male")
  expect_length(problems, 1)
  expect_match(problems, "Schnider's propofol model is not defined")
  expect_match(problems, "CL1")
  expect_null(alternativeModelEntry("propofol", 7, 65, 0.5, "male", rowOf("propofol")))
  expect_false(is.null(alternativeModelEntry("remifentanil", 7, 65, 0.5, "male", rowOf("remifentanil"))))
  expect_gt(propofolSchnider(20, 110, 5, "male")$PK[[1]]$cl1, 0)
  # James's lean body mass at 500 kg: Minto's volumes go negative too
  problems <- alternativeModelProblems(c("propofol", "remifentanil"), 500, 200, 40, "male")
  expect_match(problems, "Minto's remifentanil model is not defined", all = FALSE)
  expect_match(problems, "V1, V2 and CL1", all = FALSE)
  # Drugs without an alternative model are ignored
  expect_length(alternativeModelProblems("fentanyl", 7, 65, 0.5, "male"), 0)
  # ... and the pipeline skips the undefined model silently
  drugs <- simulate(dosesIV, age = 0.5, weight = 7, height = 65)
  expect_false("propofol (Schnider)" %in% names(drugs))
  expect_true("remifentanil (Minto)" %in% names(drugs))
})

test_that("the James lean body mass is kept, and fails the obese patient as the pumps do", {
  # A 160 cm, 40-year-old woman: James's lean body mass peaks at 92.5 kg
  # (BMI 36) and then falls, so past it Schnider's clearance climbs steeply
  # and Minto's volumes shrink
  cl1 <- function(w) propofolSchnider(w, 160, 40, "female")$PK[[1]]$cl1
  v1 <- function(w) remifentanilMinto(w, 160, 40, "female")$PK[[1]]$v1
  expect_equal_rounded(c(cl1(60), cl1(92.5), cl1(150)), c(1.729211, 2.794325, 6.716170))
  expect_equal_rounded(c(v1(60), v1(92.5), v1(150)), c(4.263900, 4.704661, 3.330375))
  expect_lt(v1(150), v1(92.5))
  # At 200 kg (BMI 78) James's lean body mass is negative and so is Minto's V1
  expect_lt(lbmJames(200, 160, "female"), 0)
  problems <- alternativeModelProblems(c("propofol", "remifentanil"), 200, 160, 40, "female")
  expect_length(problems, 1)
  expect_match(problems, "Minto's remifentanil model is not defined")
  # The scenario james-lean-body-mass: at 150 kg the Schnider pump gives far
  # more propofol over the hour than the Eleveld pump, and the Minto pump less
  # remifentanil than the library's (Kim's, at this BMI)
  d <- simulate(dosesTCI, age = 40, weight = 150, height = 160, sex = "female")
  totalOf <- function(n) {
    tci <- d[[n]]$tci
    r <- tci$rates[!tci$rates$Bolus & tci$rates$Time < 60, ]
    sum(tci$boluses$Amount) + sum(r$Rate * diff(c(r$Time, 60))) * 150
  }
  expect_gt(totalOf("propofol (Schnider)"), 1.7 * totalOf("propofol"))
  expect_lt(totalOf("remifentanil (Minto)"), 0.7 * totalOf("remifentanil"))
})

# --- The pipeline -------------------------------------------------------------

test_that("recalculatePK() appends a companion for each dosed drug that has one, after the library drugs", {
  drugs <- recalculatePK(NULL, dd, dosesIV, 40, 70, 170, "male", alternativeModels = TRUE)
  expect_equal(names(drugs), c("propofol", "remifentanil", "propofol (Schnider)", "remifentanil (Minto)"))
  s <- drugs[["propofol (Schnider)"]]
  expect_equal(s$drug, "propofol (Schnider)")
  expect_equal(s$companionOf, "propofol")
  expect_equal(s$modelLabel, "Schnider")
  expect_true(isAlternativeModelEntry(s))
  expect_false(isAlternativeModelEntry(drugs$propofol))
  expect_equal(s$Color, alternativeModelColor(rowOf("propofol")$Color))
  # The library's units, band and threshold, for the same panel
  expect_equal(s$Concentration.Units, drugs$propofol$Concentration.Units)
  expect_equal(s$endCe, drugs$propofol$endCe)
  expect_equal(s$MEAC, drugs$propofol$MEAC)
  expect_equal(drugs[["remifentanil (Minto)"]]$companionOf, "remifentanil")
  # Off by default, and nothing for a drug without an alternative model
  expect_equal(names(recalculatePK(NULL, dd, dosesIV, 40, 70, 170, "male")), c("propofol", "remifentanil"))
  fent <- data.frame(Drug = "fentanyl", Time = 0, Dose = 100, Units = "mcg")
  expect_equal(names(recalculatePK(NULL, dd, fent, 40, 70, 170, "male", alternativeModels = TRUE)), "fentanyl")
})

test_that("a companion is simulated on its parent's doses, differs from it, and carries no time until threshold", {
  drugs <- simulate(dosesIV, recovery = TRUE)
  s <- drugs[["propofol (Schnider)"]]
  p <- drugs$propofol
  expect_identical(s$DT, p$DT)
  expect_equal(range(s$results$Time), c(0, 120))
  # Schnider's V1 of 4.27 L against Eleveld's 6.3 L: a higher plasma peak
  expect_gt(max(seriesAtTime(s, "Plasma", seq(0, 5, by = 0.05))),
            max(seriesAtTime(p, "Plasma", seq(0, 5, by = 0.05))))
  expect_false(isTRUE(all.equal(seriesAtTime(s, "Effect Site", c(5, 30, 60)),
                                seriesAtTime(p, "Effect Site", c(5, 30, 60)))))
  # The library's time until threshold is there; the companion's is not
  expect_true(any(p$results$Site == "Recovery"))
  expect_gt(p$max$Recovery, 0)
  expect_false(any(s$results$Site == "Recovery"))
  expect_equal(s$max$Recovery, 0)
  expect_true(all(is.na(s$equiSpace$Recovery) | s$equiSpace$Recovery == 0))
  m <- drugs[["remifentanil (Minto)"]]
  expect_identical(m$DT, drugs$remifentanil$DT)
  expect_equal(m$max$Recovery, 0)
  # Minto and Eleveld nearly agree for the reference adult
  expect_equal(seriesAtTime(m, "Effect Site", 60), seriesAtTime(drugs$remifentanil, "Effect Site", 60),
               tolerance = 0.05)
})

test_that("the simulation cache reuses a companion and the switch changes the key", {
  first <- simulate(dosesIV)
  again <- simulate(dosesIV, cache = first)
  expect_identical(again[["propofol (Schnider)"]]$results, first[["propofol (Schnider)"]]$results)
  expect_identical(again[["propofol (Schnider)"]]$simKey, first[["propofol (Schnider)"]]$simKey)
  # With the switch off the companions are gone and the parents are untouched
  off <- simulate(dosesIV, alternativeModels = FALSE, cache = first)
  expect_equal(names(off), c("propofol", "remifentanil"))
  expect_identical(off$propofol$results, first$propofol$results)
})

test_that("a companion is not a second opioid: the MEAC sum and the export", {
  with <- simulate(dosesIV, recovery = FALSE)
  without <- simulate(dosesIV, recovery = FALSE, alternativeModels = FALSE)
  expect_equal(totalOpioidMEAC(with), totalOpioidMEAC(without))
  # Both pumps' rows reach the exported dose table, each under its own name
  tci <- simulate(dosesTCI, recovery = FALSE)
  exported <- exportDoseTable(dosesTCI, tci)
  expect_true(all(c("propofol (Schnider)", "remifentanil (Minto)") %in% exported$Drug))
  expect_equal(sum(exported$Drug == "propofol (Schnider)"), nrow(tci[["propofol (Schnider)"]]$tci$rates))
  expect_equal(sum(exported$Drug == "propofol"), 2 + nrow(tci$propofol$tci$rates))
})

# --- The plot -----------------------------------------------------------------

test_that("a companion is drawn in its parent's panel as a distinct, legended line", {
  drugs <- simulate(dosesIV, recovery = TRUE)
  p <- plotOf(drugs, plotRecovery = TRUE)
  # Two panels, not four
  expect_equal(levels(p$plotResults$Wrap), c("propofol\n(mcg/ml)", "remifentanil\n(ng/ml)"))
  # The companion's rows sit on the parent's panel
  rows <- p$plotResults[as.character(p$plotResults$Drug) == "propofol (Schnider)", ]
  expect_gt(nrow(rows), 0)
  expect_true(all(as.character(rows$Wrap) == "propofol\n(mcg/ml)"))
  expect_setequal(unique(as.character(rows$Site)), c("Plasma", "Effect Site"))
  # Listed under its parent, with its own colour, and no recovery of its own
  expect_equal(levels(p$plotResults$Drug),
               c("propofol", "propofol (Schnider)", "remifentanil", "remifentanil (Minto)", "Recovery"))
  expect_false(any(as.character(p$plotResults$Site[as.character(p$plotResults$Drug) == "propofol (Schnider)"]) == "Recovery"))
  expect_s3_class(p$plotObject, "ggplot")
  built <- ggplot2::ggplot_build(p$plotObject)
  expect_no_error(built)
  # The height is that of two panels
  expect_equal(p$plotHeight, plotOf(simulate(dosesIV, alternativeModels = FALSE), plotRecovery = TRUE)$plotHeight)
})

test_that("the plot survives normalization, a log axis and the plasma line blanked", {
  drugs <- simulate(dosesIV, recovery = FALSE)
  for (norm in c("Peak plasma", "Peak effect site")) {
    p <- plotOf(drugs, normalization = norm)
    expect_true("propofol (Schnider)" %in% as.character(p$plotResults$Drug))
    expect_no_error(ggplot2::ggplot_build(p$plotObject))
  }
  p <- plotOf(drugs, logY = TRUE)
  expect_no_error(ggplot2::ggplot_build(p$plotObject))
  p <- plotOf(drugs, plasmaLinetype = "blank")
  comp <- p$plotResults[as.character(p$plotResults$Drug) == "remifentanil (Minto)", ]
  expect_true(all(as.character(comp$Site) == "Effect Site"))
})

test_that("under TCI the companion's pump is a second line on the parent's rate panel", {
  drugs <- simulate(dosesTCI, recovery = TRUE)
  for (d in names(drugs)) expect_false(is.null(drugs[[d]]$tci), info = d)
  # Each pump hits its own target
  for (d in c("propofol", "propofol (Schnider)"))
    expect_equal(seriesAtTime(drugs[[d]], "Effect Site", c(30, 59)), c(3, 3), tolerance = 0.003, info = d)
  for (d in c("remifentanil", "remifentanil (Minto)"))
    expect_equal(seriesAtTime(drugs[[d]], "Effect Site", c(30, 59)), c(4, 4), tolerance = 0.003, info = d)
  # ... with loading doses of its own
  expect_false(isTRUE(all.equal(drugs[["propofol (Schnider)"]]$tci$boluses$Amount, drugs$propofol$tci$boluses$Amount)))

  p <- plotOf(drugs, plotRecovery = TRUE, plotMEAC = TRUE, plotInteraction = TRUE)
  wraps <- levels(p$plotResults$Wrap)
  # Each rate panel directly under its own drug's panel
  expect_equal(wraps, c("propofol\n(mcg/ml)", "propofol TCI\n(mg/kg/min)",
                        "remifentanil\n(ng/ml)", "remifentanil TCI\n(mcg/kg/min)",
                        PLOT_NAME_MEAC, PLOT_NAME_INTERACTION))
  rate <- p$plotResults[as.character(p$plotResults$Site) == "Rate", ]
  expect_setequal(unique(as.character(rate$Drug)),
                  c("propofol TCI", "propofol (Schnider) TCI", "remifentanil TCI", "remifentanil (Minto) TCI"))
  expect_true(all(as.character(rate$Wrap[as.character(rate$Drug) == "propofol (Schnider) TCI"]) == "propofol TCI\n(mg/kg/min)"))
  expect_equal(levels(p$plotResults$Drug)[1:8],
               c("propofol", "propofol (Schnider)", "propofol TCI", "propofol (Schnider) TCI",
                 "remifentanil", "remifentanil (Minto)", "remifentanil TCI", "remifentanil (Minto) TCI"))
  # The loading-dose labels: the library's pump first, the companion's named for its model
  labels <- Filter(function(l) inherits(l$geom, "GeomLabel"), p$plotObject$layers)
  expect_length(labels, 1)
  lab <- labels[[1]]$data
  expect_true(any(grepl("\\(Schnider\\)$", lab$Label)))
  expect_true(any(grepl("\\(Minto\\)$", lab$Label)))
  expect_equal(sum(!grepl("\\(", lab$Label)), 2)
  expect_true(all(lab$vjust[grepl("\\(", lab$Label)] > lab$vjust[!grepl("\\(", lab$Label)][1]))
  # The MEAC panel counts remifentanil once: no companion in the derived panels
  expect_false(any(grepl("Minto", as.character(p$plotResults$Drug[as.character(p$plotResults$Wrap) == PLOT_NAME_MEAC]))))
  expect_no_error(ggplot2::ggplot_build(p$plotObject))
})

test_that("a companion whose parent has no series to draw is dropped, not drawn on its own", {
  drugs <- simulate(dosesIV, recovery = FALSE)
  # Drop the parent as the app would if the drug had just been removed
  drugs$propofol <- NULL
  p <- plotOf(drugs)
  expect_equal(levels(p$plotResults$Wrap), "remifentanil\n(ng/ml)")
  expect_false("propofol (Schnider)" %in% as.character(p$plotResults$Drug))
  expect_no_error(ggplot2::ggplot_build(p$plotObject))
})

# --- Scenarios and help -------------------------------------------------------

test_that("the comparison scenarios are registered, valid and switch the models on", {
  for (id in c("eleveld-vs-minto-schnider", "tci-eleveld-vs-minto-schnider")) {
    s <- helpScenarioById(id)
    expect_false(is.null(s), info = id)
    expect_true(s$options$mintoSchnider, info = id)
    expect_true(all(c("propofol", "remifentanil") %in% s$doses$Drug), info = id)
    expect_length(helpScenarioCheck(s, ddHelp), 0)
    html <- helpScenarioPageHTML(id)
    expect_match(html, "<td>Minto / Schnider</td>", fixed = TRUE)
    expect_match(html, "<td>on</td>", fixed = TRUE)
  }
  expect_true(all(helpScenarioIds() != "" ))
  expect_false(helpScenarioById("propofol-bolus")$options$mintoSchnider)
  expect_match(helpScenarioPageHTML("propofol-bolus"), "<td>Minto / Schnider</td>", fixed = TRUE)
  # The switch needs one of the two drugs, and a patient the models are defined for
  bad <- helpScenario("fent", "F", "Opioids", "x", doses = helpDoses(c("fentanyl", 0, 100, "mcg")),
                      mintoSchnider = TRUE)
  expect_match(helpScenarioCheck(bad, ddHelp), "Minto / Schnider needs", all = FALSE)
  infant <- helpScenario("infant", "I", "Opioids", "x", age = 0.5, weight = 7, height = 65,
                         doses = helpDoses(c("propofol", 0, 2, "mg/kg")), mintoSchnider = TRUE)
  expect_match(helpScenarioCheck(infant, ddHelp), "Schnider's propofol model is not defined", all = FALSE)
})

test_that("loading a scenario sends the Minto / Schnider setting", {
  skip_if_not(exists("MockShinySession", asNamespace("shiny")), "MockShinySession not available")
  session <- shiny::MockShinySession$new()
  sent <- list()
  session$sendInputMessage <- function(inputId, message) sent[[inputId]] <<- message
  doseTable <- shiny::reactiveVal(doseTableInit)
  eventTable <- shiny::reactiveVal(eventTableInit)
  applyHelpScenario(session, helpScenarioById("eleveld-vs-minto-schnider"), doseTable, eventTable)
  expect_true(sent$mintoSchnider$value)
  applyHelpScenario(session, helpScenarioById("propofol-bolus"), doseTable, eventTable)
  expect_false(sent$mintoSchnider$value)
})

test_that("the help page about the models renders its generated parameter table", {
  expect_true(helpPageExists(ALTERNATIVE_MODELS_HELP_PAGE))
  html <- helpPageHTML(ALTERNATIVE_MODELS_HELP_PAGE)
  expect_match(html, "the Schnider model", fixed = TRUE)
  expect_match(html, "the Minto model", fixed = TRUE)
  expect_match(html, "Reference adult", fixed = TRUE)
  # The child's row says Schnider is not defined there, with the reason
  expect_match(html, "not defined: its CL1 is not positive", fixed = TRUE)
  md <- helpAlternativeModelTableMarkdown(ddHelp)
  expect_equal(length(gregexpr("\\| Reference adult", md)[[1]]), 2)
  expect_match(md, "4.27 \\| 24 \\| 238")
})

# --- The server ---------------------------------------------------------------

test_that("in the app the switch adds the companions and the hover reports both models", {
  oldConfig <- .sprglobals$config
  .sprglobals$config <- DEFAULT_CONFIG
  on.exit(.sprglobals$config <- oldConfig)
  shiny::testServer(app_server, {
    notified <- list()
    session$sendNotification <- function(type, message) notified[[length(notified) + 1]] <<- list(type = type, message = message)
    session$setInputs(
      timeUnits = "minutes", timeMode = "clock", referenceTime = "08:00", maximum = "120",
      plotWidth = 800, yaxisHeight = 200, plasmaLinetype = "dashed", effectsiteLinetype = "solid",
      weight = 70, weightUnit = "1", height = 170, heightUnit = "1", age = 40, ageUnit = "1",
      sex = "male", showThreshold = TRUE, normalization = "none", typical = "Range", logY = FALSE,
      mintoSchnider = FALSE
    )
    dt <- data.frame(
      Drug = c("propofol", "propofol", "remifentanil", ""), Time = c("0", "0", "0", ""),
      Dose = c("2", "100", "0.2", ""), Units = c("mg/kg", "mcg/kg/min", "mcg/kg/min", ""),
      stringsAsFactors = FALSE
    )
    doseTable(dt)
    session$flushReact()
    expect_equal(names(drugs()), c("propofol", "remifentanil"))

    session$setInputs(mintoSchnider = TRUE)
    expect_true(mintoSchnider())
    expect_equal(names(drugs()), c("propofol", "remifentanil", "propofol (Schnider)", "remifentanil (Minto)"))
    expect_equal(names(companionsOf("propofol")), "propofol (Schnider)")
    expect_length(companionsOf("fentanyl"), 0)
    # The hover reads the panel through the library's drug and adds the companion
    text <- xy_str(list(x = 30, panelvar1 = "propofol\n(mcg/ml)"))
    expect_match(text, "propofol Ce: ")
    expect_match(text, ", Schnider Ce: ")
    expect_match(text, "Time until threshold")
    # The plot has two panels
    expect_equal(levels(plotResultsReactive()$Wrap), c("propofol\n(mcg/ml)", "remifentanil\n(ng/ml)"))
    # A click on the companion's line is a click on the parent
    expect_equal(imgDrugTime(list(x = 10, panelvar1 = "propofol\n(mcg/ml)"))$drug, "propofol")

    # A patient one model is not defined for: the companion is left out and a notice says so
    session$setInputs(age = 0.5, weight = 7, height = 65)
    expect_equal(names(drugs()), c("propofol", "remifentanil", "remifentanil (Minto)"))
    shown <- Filter(function(n) is.list(n$message) && identical(n$message$id, "alternativeModels"), notified)
    expect_gt(length(shown), 0)
    expect_match(shown[[length(shown)]]$message$html, "Schnider's propofol model is not defined")

    # Off again: the companions go
    session$setInputs(mintoSchnider = FALSE)
    expect_equal(names(drugs()), c("propofol", "remifentanil"))
  })
})

test_that("under TCI in the app the hover on the rate panel reports both pumps", {
  oldConfig <- .sprglobals$config
  .sprglobals$config <- DEFAULT_CONFIG
  on.exit(.sprglobals$config <- oldConfig)
  shiny::testServer(app_server, {
    session$setInputs(
      timeUnits = "minutes", timeMode = "clock", referenceTime = "08:00", maximum = "120",
      plotWidth = 800, yaxisHeight = 200, plasmaLinetype = "dashed", effectsiteLinetype = "solid",
      weight = 70, weightUnit = "1", height = 170, heightUnit = "1", age = 40, ageUnit = "1",
      sex = "male", showThreshold = FALSE, normalization = "none", typical = "Range", logY = FALSE,
      mintoSchnider = TRUE
    )
    dt <- data.frame(
      Drug = c("propofol", "propofol", ""), Time = c("0", "60", ""),
      Dose = c("3", "0", ""), Units = c(TCI_UNIT_EFFECT, TCI_UNIT_EFFECT, ""),
      stringsAsFactors = FALSE
    )
    doseTable(dt)
    session$flushReact()
    expect_equal(names(drugs()), c("propofol", "propofol (Schnider)"))
    expect_equal(levels(plotResultsReactive()$Wrap), c("propofol\n(mcg/ml)", "propofol TCI\n(mg/kg/min)"))
    text <- xy_str(list(x = 30, panelvar1 = "propofol TCI\n(mg/kg/min)"))
    expect_match(text, "propofol TCI rate: ")
    expect_match(text, ", Schnider rate: ")
    text <- xy_str(list(x = 0.05, panelvar1 = "propofol TCI\n(mg/kg/min)"))
    expect_match(text, "propofol TCI loading dose: ")
    expect_match(text, ", Schnider loading dose: ")
    expect_equal(imgDrugTime(list(x = 10, panelvar1 = "propofol TCI\n(mg/kg/min)"))$drug, "propofol")
  })
})
