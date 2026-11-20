# % effect (R/pdEffect.R): a drug's effect as a percentage of its maximum,
# 0 to 100% (Steven L. Shafer, 2026-11-20).  The sigmoid Emax curve and the
# endpoint's own effect site are checked against closed forms and against an
# independent integration that shares no code with pdToleranceCompartment();
# the panel and the Graph Option against the plot and relevantFields().
# (Claude Code, 2026-11-20, at the request of Steven L. Shafer.)

# A table in place of the CSV for the length of one test
withEffectTable <- function(rows, code) {
  old <- .pdEffectCache$table
  on.exit(.pdEffectCache$table <- old, add = TRUE)
  .pdEffectCache$table <- rows
  force(code)
}

effectRow <- function(drug = "fentanyl", endpoint = "test effect", role = "drawn",
                      driver = "Plasma", ke0 = NA, ec50 = 0.002, gamma = 1,
                      verification = "read_full_text", provisional = NA) {
  data.frame(Drug = drug, endpoint = endpoint, role = role, driver = driver,
             ke0_per_min = ke0, ec50_mcg_per_ml = ec50, gamma = gamma,
             emax = 10, emax_units = "mmHg", provisional = provisional,
             population = "test", source = "test", doi = NA_character_,
             pmid = NA_character_, verification = verification, notes = "",
             stringsAsFactors = FALSE)
}

test_that("the table has the columns the code reads, and only known values", {
  table <- pdEffectTable()
  expect_true(all(c("Drug", "endpoint", "role", "driver", "ke0_per_min",
                    "ec50_mcg_per_ml", "gamma", "emax", "emax_units", "provisional",
                    "population", "source", "doi", "pmid", "verification") %in% names(table)))
  expect_true(all(table$role %in% c("drawn", "alternative")))
  expect_true(all(table$driver %in% PD_EFFECT_DRIVERS))
  applied <- table$verification %in% PD_EFFECT_APPLIED_VERIFICATION
  expect_true(all(table$ec50_mcg_per_ml[applied] > 0))
  expect_true(all(table$gamma[applied] > 0))
  ke0 <- table$ke0_per_min[applied]
  expect_true(all(is.na(ke0) | ke0 > 0))
  # an effect-site endpoint names no ke0 of its own: the drug's is used
  expect_true(all(is.na(table$ke0_per_min[table$driver == "Effect Site"])))
  # every drug is a library drug
  expect_true(all(table$Drug %in% getDrugDefaultsGlobal()$Drug))
  # every drawn endpoint has one row, and no drawn endpoint name hides another
  drawn <- table[applied & table$role == "drawn", ]
  expect_false(any(duplicated(paste(drawn$Drug, drawn$endpoint))))
})

test_that("the effect is the sigmoid Emax curve as a share of its maximum", {
  expect_equal(pdEffectPercent(0, 1, 2), 0)
  expect_equal(pdEffectPercent(1, 1, 0.75), 50)
  expect_equal(pdEffectPercent(1, 1, 3), 50)
  expect_equal(pdEffectPercent(0.5, 0.29, 0.75), 100 * 0.5^0.75 / (0.29^0.75 + 0.5^0.75))
  expect_equal(pdEffectPercent(c(-1, NA, Inf), 1, 1), c(0, 0, 0))
  # no Inf / Inf far above the EC50
  expect_equal(pdEffectPercent(1e300, 1e-300, 5), 100)
  x <- 10^seq(-4, 4, length.out = 50)
  e <- pdEffectPercent(x, 1, 1.5)
  expect_true(all(diff(e) > 0) && all(e > 0 & e < 100))
  expect_error(pdEffectPercent(1, 0, 1))
  expect_error(pdEffectPercent(1, 1, 0))
})

test_that("an endpoint's own effect site is exact for plasma linear between points", {
  # A plasma curve, exponential, read as linear between its points; against an
  # independent integration of dCe/dt = ke0 (Cp - Ce) by small Euler steps on
  # the same linear interpolant
  t <- seq(0, 600, by = 10)
  cp <- 2 * exp(-0.005 * t)
  ke0 <- 0.0103333
  results <- data.frame(Drug = "x", Time = t, Site = "Plasma", Y = cp)
  d <- pdEffectDriver(results, "mcg", effectRow(ke0 = ke0))
  h <- 0.001
  fine <- seq(0, 600, by = h)
  cpFine <- stats::approx(t, cp, fine)$y
  ce <- numeric(length(fine))
  for (i in seq_along(fine)[-1]) ce[i] <- ce[i - 1] + h * ke0 * (cpFine[i - 1] - ce[i - 1])
  expect_equal(d$C, ce[match(t, round(fine, 6))], tolerance = 1e-4)
  # the effect site lags plasma: zero at first, rising, crossing plasma at its peak
  expect_equal(d$C[1], 0)
  peak <- which.max(d$C)
  expect_gt(peak, 1)
  expect_equal(d$C[peak], cp[peak], tolerance = 0.02)
  # with no ke0, plasma directly
  direct <- pdEffectDriver(results, "mcg", effectRow(ke0 = NA))
  expect_equal(direct$C, cp)
  # the drug's own effect site where the row says so
  es <- rbind(results, data.frame(Drug = "x", Time = t, Site = "Effect Site", Y = cp / 2))
  own <- pdEffectDriver(es, "mcg", effectRow(driver = "Effect Site"))
  expect_equal(own$C, cp / 2)
  # and none where the drug has no effect site
  expect_null(pdEffectDriver(results, "mcg", effectRow(driver = "Effect Site")))
})

test_that("the panel does not depend on the Drug Library's concentration unit", {
  row <- effectRow(ke0 = 0.01, ec50 = 0.001, gamma = 1.5)
  t <- seq(0, 300, by = 5)
  results <- function(scale)
    data.frame(Drug = "fentanyl", Time = t, Site = "Plasma", Y = 2 * exp(-0.01 * t) * scale)
  ng  <- pdEffectSeries(list(results = results(1),    Concentration.Units = "ng"), row)
  mcg <- pdEffectSeries(list(results = results(1e-3), Concentration.Units = "mcg"), row)
  expect_equal(ng$Y, mcg$Y, tolerance = 1e-12)
  expect_equal(ng$C, mcg$C, tolerance = 1e-12)
})

test_that("only the first applied drawn row of an endpoint is drawn", {
  rows <- rbind(
    effectRow("fentanyl", "A", ec50 = 1),
    effectRow("fentanyl", "A", ec50 = 2),
    effectRow("fentanyl", "A", role = "alternative", ec50 = 3),
    effectRow("fentanyl", "B", ec50 = 4),
    effectRow("morphine", "A", verification = "pending_verification"),
    effectRow("sufentanil", "A", role = "alternative")
  )
  withEffectTable(rows, {
    r <- pdEffectRows("fentanyl")
    expect_equal(r$endpoint, c("A", "B"))
    expect_equal(r$ec50_mcg_per_ml, c(1, 4))
    expect_equal(nrow(pdEffectRows("morphine")), 0)
    expect_equal(pdEffectDrugs(c("propofol", "fentanyl", "morphine", "sufentanil", NA, "")),
                 "fentanyl")
    expect_equal(pdEffectSeriesDrug(c("fentanyl A", "fentanyl B", "morphine A",
                                      "sufentanil A", "fentanyl", "fentanyl tolerance")),
                 c("fentanyl", "fentanyl", NA, NA, NA, NA))
    expect_equal(pdEffectSeriesRow("fentanyl B")$ec50_mcg_per_ml, 4)
    expect_null(pdEffectSeriesRow("propofol"))
    # the panel order: one slot per endpoint, NA for a drug without one
    names <- pdEffectPanelNames(c("propofol", "fentanyl", "morphine"))
    expect_equal(names, list(c(NA, "fentanyl A", NA), c(NA, "fentanyl B", NA)))
    # the help table lists every row, the drawn ones marked
    md <- helpPdEffectTableMarkdown()
    expect_equal(length(strsplit(md, "\n")[[1]]), 2 + nrow(rows))
    expect_match(md, "| drawn |", fixed = TRUE)
    expect_match(md, "| alternative |", fixed = TRUE)
    expect_match(md, "| not yet |", fixed = TRUE)
  })
  withEffectTable(rows[0, ], {
    expect_equal(pdEffectPanelNames(c("propofol", "fentanyl")), list(c(NA_character_, NA_character_)))
    expect_match(helpPdEffectTableMarkdown(), "No drug")
  })
})

test_that("the Graph Option is offered only while a drug with a drawn row is drawn", {
  withEffectTable(effectRow("fentanyl", "A"), {
    expect_true(relevantFields("fentanyl")[["showEffect"]])
    expect_false(relevantFields("propofol")[["showEffect"]])
    expect_false(relevantFields(c("propofol", "fentanyl"), plotted = "propofol")[["showEffect"]])
  })
  expect_true(relevantFields("irbesartan")[["showEffect"]])
})

test_that("the plot draws the panel under the drug, from 0 to 100%", {
  withEffectTable(effectRow("fentanyl", "analgesia", ke0 = 0.05, ec50 = 0.001, gamma = 2), {
    dd <- getDrugDefaultsGlobal()
    ev <- data.frame(Time = numeric(0), Event = character(0))
    DT <- data.frame(Drug = "fentanyl", Time = c(0, 60), Dose = c(2, 0),
                     Units = "mcg/kg/hr")
    sim <- simulateDrugsWithCovariates(DT, ev, 70, 170, 50, "male", maximum = 240,
                                       plotRecovery = FALSE)
    plotted <- function(on, ...) suppressMessages(simulationPlot(
      drugs = sim, events = ev, drugDefaults = dd,
      eventDefaults = getEventDefaults(), xBreaks = seq(0, 240, 60),
      xLabels = seq(0, 240, 60), xMaximum = 240, plotEffect = on, ...))
    off <- plotted(FALSE)
    on <- plotted(TRUE)
    expect_false(any(grepl("analgesia", off$plotResults$Drug)))
    panel <- on$plotResults[as.character(on$plotResults$Drug) == "fentanyl analgesia", ]
    expect_gt(nrow(panel), 10)
    expect_equal(panel$Y[1], 0)
    expect_gt(max(panel$Y), 5)
    expect_true(all(panel$Y >= 0 & panel$Y <= 100))
    # directly under fentanyl's own panel, in % of maximal effect
    wraps <- levels(on$plotResults$Wrap)
    expect_equal(which(grepl("^fentanyl analgesia", wraps)), which(grepl("^fentanyl\n", wraps)) + 1)
    expect_true(any(grepl(PD_EFFECT_UNITS, wraps, fixed = TRUE)))
    # kept out of the legend
    expect_false("fentanyl analgesia" %in% on$plotObject$scales$get_scales("colour")$breaks)
    # and it turns a log axis off
    isLog <- function(p) {
      s <- p$plotObject$scales$get_scales("y")
      tr <- if (is.null(s)) NULL else if (!is.null(s$transformation)) s$transformation else s$trans
      !is.null(tr) && grepl("log", tr$name)
    }
    expect_true(isLog(plotted(FALSE, logY = TRUE)))
    expect_false(isLog(plotted(TRUE, logY = TRUE)))
    # not drawn under normalization
    norm <- plotted(TRUE, normalization = "Peak plasma")
    expect_false(any(grepl("analgesia", norm$plotResults$Drug)))
  })
})

test_that("irbesartan's % effect: Huang 2006, from its own effect site, provisional", {
  rows <- pdEffectRows("irbesartan")
  expect_equal(nrow(rows), 1)
  expect_equal(rows$endpoint, "SBP fall")
  expect_equal(rows$ec50_mcg_per_ml, 0.29)
  expect_equal(rows$gamma, 0.75)
  expect_equal(rows$ke0_per_min * 60, 0.62, tolerance = 1e-4)
  expect_true(isTRUE(rows$provisional))
  expect_match(rows$source, "Huang 2006")
  # Hedaya 2015 kept in view as an alternative
  table <- pdEffectTable()
  expect_true(any(table$Drug == "irbesartan" & grepl("Hedaya", table$source) &
                  table$role == "alternative"))

  # 150 mg once, as the help page states it: the effect-site peak 74% at
  # 3.4 h and 32% at 24 h (Huang); Hedaya 20% at 3.8 h and almost none left
  # at 24 h.  The label puts peak effect at 3 to 6 h.
  ev <- data.frame(Time = numeric(0), Event = character(0))
  DT <- data.frame(Drug = "irbesartan", Time = 0, Dose = 150, Units = "mg PO")
  sim <- simulateDrugsWithCovariates(DT, ev, 70, 170, 40, "male", maximum = 1440,
                                     plotRecovery = FALSE)
  entry <- sim[["irbesartan"]]
  huang <- pdEffectSeries(entry, rows)
  peak <- which.max(huang$Y)
  expect_equal(huang$Y[peak], 74, tolerance = 0.02)
  expect_gt(huang$Time[peak] / 60, 3)
  expect_lt(huang$Time[peak] / 60, 4)
  at24 <- stats::approx(huang$Time, huang$Y, 1440)$y
  expect_gt(at24, 25)
  expect_lt(at24, 35)
  hedaya <- pdEffectSeries(entry, table[grepl("Hedaya", table$source) &
                                        table$endpoint == "SBP fall", ][1, ])
  expect_equal(max(hedaya$Y), 20, tolerance = 0.03)
  expect_lt(stats::approx(hedaya$Time, hedaya$Y, 1440)$y, 1)
})

test_that("the help page shows the generated table", {
  html <- helpPageHTML("models/percent-effect")
  expect_match(html, "Huang 2006", fixed = TRUE)
  expect_match(html, "Hedaya", fixed = TRUE)
})
