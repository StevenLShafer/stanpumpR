# The parameter converter on the help page models/pk-converter: the markup
# (R/help-converter.R) and the arithmetic (inst/www/pk-convert.js).  The
# examples are those of Steven L. Shafer's Convert.xls, and the values pinned
# are the ones it computes, read from its cells.

test_that("the converter page is registered and renders", {
  expect_true(helpPageExists("models/pk-converter"))
  html <- helpPageHTML("models/pk-converter")
  expect_match(html, '<div class="pkconv">', fixed = TRUE)
  for (g in c("vc", "micro", "half", "coef", "exp")) {
    expect_match(html, sprintf('data-pk-group="%s"', g), fixed = TRUE)
  }
  expect_s3_class(helpPageUI("models/pk-converter"), "shiny.tag")
  expect_true(file.exists(system.file("www", "pk-convert.js", package = "stanpumpR")))
})

test_that("the converter is one HTML block that Shiny leaves unbound", {
  block <- helpPKConverterHTML()
  # commonmark ends an HTML block at a blank line
  expect_false(grepl("\n[[:space:]]*\n", block))
  inputs <- regmatches(block, gregexpr("<input[^>]*>", block))[[1]]
  expect_gt(length(inputs), 30)
  # Shiny binds a text input by its id or, failing that, its name
  expect_true(all(grepl("data-shiny-no-bind-input", inputs, fixed = TRUE)))
  expect_false(any(grepl("\\sid=", inputs)))
  text <- inputs[grepl('type="text"', inputs, fixed = TRUE)]
  expect_false(any(grepl("\\sname=", text)))
  # The rendered page keeps every field
  html <- helpPageHTML("models/pk-converter")
  expect_equal(lengths(regmatches(html, gregexpr("<input", html))), length(inputs))
})

# --- The arithmetic, run in Node -------------------------------------------

# Evaluate a JavaScript expression with PKConvert in scope; the result comes
# back through JSON
pkConvertJS <- function(expression) {
  script <- tempfile(fileext = ".js")
  on.exit(unlink(script))
  js <- normalizePath(system.file("www", "pk-convert.js", package = "stanpumpR"), winslash = "/")
  writeLines(c(sprintf("const P = require(%s);", jsonlite::toJSON(js, auto_unbox = TRUE)),
               sprintf("process.stdout.write(JSON.stringify(%s));", expression)), script)
  out <- system2(Sys.which("node"), shQuote(script), stdout = TRUE, stderr = TRUE)
  jsonlite::fromJSON(paste(out, collapse = ""), simplifyVector = TRUE)
}

skipWithoutNode <- function() {
  skip_on_cran()
  skip_if(!nzchar(Sys.which("node")), "node is not installed")
}

micro <- function(m) c(m$k10, m$k12, m$k13, m$k21, m$k31)

test_that("every field a definition takes is on the page", {
  skipWithoutNode()
  required <- pkConvertJS("P.required")
  block <- helpPKConverterHTML()
  for (g in names(required)) {
    section <- regmatches(block, regexpr(sprintf('data-pk-group="%s".*?</section>', g), block, perl = TRUE))
    for (n in 1:3) {
      for (f in required[[g]][[as.character(n)]]) {
        row <- regmatches(section, regexpr(sprintf('<tr data-pk-n="[^"]*"><th[^>]*>.*?data-pk-field="%s"', f),
                                           section, perl = TRUE))
        expect_length(row, 1)
        expect_true(as.character(n) %in% strsplit(sub('<tr data-pk-n="([^"]*)".*', "\\1", row), ",")[[1]],
                    info = paste(g, f, n))
      }
    }
  }
})

test_that("tabs 1 and 2 (volumes and clearances; V1 and the rate constants)", {
  skipWithoutNode()
  m <- pkConvertJS("P.fromVolumesClearances(3, [10, 100, 1000], [3, 2, 1]).model")
  expect_equal(micro(m), c(0.3, 0.2, 0.1, 0.02, 0.001))
  expect_equal(m$vdss, 1110)
  expect_equal(m$lambda, c(0.606979569, 0.013275844, 0.000744586), tolerance = 1e-8)
  expect_equal(m$lambda, cube(0.3, 0.2, 0.1, 0.02, 0.001), tolerance = 1e-10)
  expect_equal(m$A, c(0.098825768, 0.001109493, 6.47383e-05), tolerance = 1e-7)
  expect_equal(m$halfLife, c(1.1419613041687, 52.2111556850848, 930.915879405947), tolerance = 1e-10)
  m <- pkConvertJS("P.fromMicro(2, 5, {k10: 0.7, k12: 0.5, k21: 0.05}).model")
  expect_equal(m$v, c(5, 50))
  expect_equal(m$cl, c(3.5, 2.5))
  expect_equal(m$lambda, c(1.221343022, 0.028656978), tolerance = 1e-8)
  expect_equal(m$A, c(0.196421016, 0.003578984), tolerance = 1e-8)
  expect_equal(100 * m$fraction, c(98.21050794, 1.789492063), tolerance = 1e-8)
  m <- pkConvertJS("P.fromVolumesClearances(1, [8], [4]).model")
  expect_equal(c(m$lambda, m$A, m$halfLife), c(0.5, 0.125, log(2) / 0.5))
})

test_that("tab 3 (V1, Vdss, CL and the half-lives) recovers tab 1's model", {
  skipWithoutNode()
  m <- pkConvertJS(paste0("P.fromHalfLives(3, {v1: 10, vdss: 1110, cl: 3, ",
                          "t: [1.1419613041687, 52.2111556850848, 930.915879405947]}).model"))
  expect_equal(m$v, c(10, 100, 1000), tolerance = 1e-10)
  expect_equal(m$cl, c(3, 2, 1), tolerance = 1e-10)
  m <- pkConvertJS("P.fromHalfLives(2, {v1: 5, cl: 3.5, t: [0.567528669686619, 24.1877277788829]}).model")
  expect_equal(c(m$v, m$cl), c(5, 50, 3.5, 2.5), tolerance = 1e-10)
  m <- pkConvertJS("P.fromHalfLives(1, {cl: 4, t: [1.38629436111989]}).model")
  expect_equal(m$v1, 8, tolerance = 1e-12)
})

test_that("tab 4 (coefficients and exponents) reproduces the spreadsheet", {
  skipWithoutNode()
  m <- pkConvertJS("P.fromCoefficients(3, [12.2, 3.76, 1.44], [0.87, 0.12, 0.013]).model")
  expect_equal(m$v1, 1 / 17.4)
  expect_equal(micro(m), c(0.111448765, 0.371947306, 0.153610825, 0.328975813, 0.03701729),
               tolerance = 1e-8)
  r <- threeCompartmentFromTriexponential(c(12.2, 3.76, 1.44), c(0.87, 0.12, 0.013))
  expect_equal(c(m$v, m$cl), c(r$v1, r$v2, r$v3, r$cl1, r$cl2, r$cl3), tolerance = 1e-10)
  expect_equal(100 * m$fraction, c(70.11494253, 21.6091954, 8.275862069), tolerance = 1e-8)
  # The order the exponents are entered in does not matter
  m2 <- pkConvertJS("P.fromCoefficients(3, [1.44, 12.2, 3.76], [0.013, 0.87, 0.12]).model")
  expect_equal(micro(m2), micro(m))
  m <- pkConvertJS("P.fromCoefficients(2, [0.196421015874573, 0.00357898412542674], [1.22134302209383, 0.0286569779061718]).model")
  expect_equal(c(m$v, m$cl), c(5, 50, 3.5, 2.5), tolerance = 1e-10)
})

test_that("tab 5 (V1, the exponents, k21 and k31) reproduces the spreadsheet", {
  skipWithoutNode()
  m <- pkConvertJS(paste0("P.fromExponentsAndRates(3, 10, [0.606979569298565, 0.0132758444333374, ",
                          "0.000744586268097897], 0.02, 0.001).model"))
  expect_equal(c(m$v, m$cl), c(10, 100, 1000, 3, 2, 1), tolerance = 1e-9)
  m <- pkConvertJS("P.fromExponentsAndRates(2, 5, [1.22134302209383, 0.0286569779061718], 0.05).model")
  expect_equal(c(m$v, m$cl), c(5, 50, 3.5, 2.5), tolerance = 1e-9)
})

test_that("an inadmissible definition is refused, naming what failed", {
  skipWithoutNode()
  e <- pkConvertJS(paste0("P.fromExponentsAndRates(3, 10, [0.606979569298565, 0.0132758444333374, ",
                          "0.000744586268097897], 2, 0.001).error"))
  expect_match(e, "k12 would be -")
  # Vdss below V1 has no peripheral volume to give
  e <- pkConvertJS("P.fromHalfLives(3, {v1: 10, vdss: 5, cl: 3, t: [1, 50, 900]}).error")
  expect_type(e, "character")
  expect_match(pkConvertJS("P.fromCoefficients(2, [1, 1], [0.5, 0.5]).error"), "equal")
  expect_match(pkConvertJS("P.fromVolumesClearances(2, [5, 50], [3.5, -1]).error"), "k12")
})

test_that("the model table has one layout, whichever panel defined the model", {
  skipWithoutNode()
  # The fractional coefficients are outputs only, in the table
  block <- helpPKConverterHTML()
  expect_false(grepl("data-pk-field=\"frac", block, fixed = TRUE))
  expect_match(block, 'class="pkconv-summary-body"', fixed = TRUE)
  layout <- function(rows) lapply(rows$cells, `[[`, "name")
  labels <- c("Volumes", "Clearances", "Exponents", "Half-lives", "Coefficients",
              "Fractional coefficients", "Micro rate constants")
  three <- c("P.fromVolumesClearances(3, [10, 100, 1000], [3, 2, 1]).model",
             "P.fromCoefficients(3, [0.0988257683958246, 0.00110949328401814, 0.0000647383201572566], [0.606979569298565, 0.0132758444333374, 0.000744586268097897]).model",
             "P.fromMicro(3, 10, {k10: 0.3, k12: 0.2, k13: 0.1, k21: 0.02, k31: 0.001}).model")
  rows <- lapply(three, function(m) pkConvertJS(sprintf("P.summaryRows(%s, 60, 'h')", m)))
  expect_equal(rows[[1]]$label, labels)
  expect_equal(layout(rows[[1]]),
               list(c("V1", "V2", "V3"), c("CL1", "CL2", "CL3"),
                    c("λ1", "λ2", "λ3"),
                    c("t½ λ1", "t½ λ2", "t½ λ3"),
                    c("A1", "A2", "A3"), c("A1/ΣA", "A2/ΣA", "A3/ΣA"),
                    c("k10", "k12", "k13", "k21", "k31")))
  for (r in rows[-1]) {
    expect_equal(layout(r), layout(rows[[1]]))
    expect_equal(r$cells, rows[[1]]$cells, tolerance = 1e-8)
  }
  # In hours: clearances and rates 60 times larger, half-lives 60 times smaller
  values <- lapply(rows[[1]]$cells, `[[`, "value")
  expect_equal(values[[2]], c(180, 120, 60))
  expect_equal(values[[7]], c(18, 12, 6, 1.2, 0.06))
  expect_equal(values[[4]][3], 930.915879405947 / 60, tolerance = 1e-10)
  expect_equal(values[[6]], c(98.8257684, 1.109493284, 0.06473832), tolerance = 1e-8)
  expect_equal(rows[[1]]$units, c("L", "L/h", "1/h", "h", "1/L", "%", "1/h"))
  # Fewer compartments, the same rows
  two <- pkConvertJS("P.summaryRows(P.fromVolumesClearances(2, [5, 50], [3.5, 2.5]).model)")
  expect_equal(two$label, labels)
  expect_equal(layout(two)[[7]], c("k10", "k12", "k21"))
  one <- pkConvertJS("P.summaryRows(P.fromVolumesClearances(1, [8], [4]).model)")
  expect_equal(lengths(layout(one)), rep(1L, 7))
})

test_that("a field reads a comma only as a thousands separator", {
  skipWithoutNode()
  js <- pkConvertJS(paste0(
    "['1110', '1,110', '1,110.5', ' 0.5 ', '1e-3', '0,5', '0,05', '1,10', '12,3456', 'abc', '']",
    ".map(function(s) { var x = P.parseNumber(s); return x === null ? 'blank' : String(x); })"))
  expect_equal(js, c("1110", "1110", "1110.5", "0.5", "0.001",
                     "NaN", "NaN", "NaN", "NaN", "NaN", "blank"))
})

test_that("the effect site agrees with the simulator's own solvers", {
  skipWithoutNode()
  m <- pkConvertJS("P.fromVolumesClearances(3, [10, 100, 1000], [3, 2, 1]).model")
  js <- pkConvertJS(paste0(
    "(function() { var m = P.fromVolumesClearances(3, [10, 100, 1000], [3, 2, 1]).model;",
    " return {peak: P.peakTime(m, 0.456), ke0: P.ke0FromPeakTime(m, 1.6),",
    " ce: P.effect(m, 0.456, P.peakTime(m, 0.456)), cp: P.plasma(m, P.peakTime(m, 0.456)),",
    " tie: P.effect(m, m.lambda[0], 1), near: P.effect(m, m.lambda[0] * (1 + 1e-7), 1)}; })()"))
  # optimize()'s default tolerance leaves effectSitePeakTime() about 1e-6
  # from the crossing the bisection finds (checked to 1e-12 below)
  expect_equal(js$peak, effectSitePeakTime(m$A, m$lambda, 0.456), tolerance = 1e-5)
  expect_equal(js$ke0, ke0FromTPeakBolus(1.6, m$A, m$lambda), tolerance = 1e-6)
  # At the peak the effect site meets the plasma
  expect_equal(js$ce, js$cp, tolerance = 1e-12)
  # ke0 equal to an exponent is the limit, not a division by zero
  expect_equal(js$tie, js$near, tolerance = 1e-6)
})

# --- Decrement times ---------------------------------------------------------

test_that("the converter's TCI is the simulator's controller, step for step", {
  skipWithoutNode()
  # Propofol for one man, entered in the converter as V1 and its rate constants
  PK <- getDrugPK("propofol", 70, 170, 50, "male", getDrugDefaults("propofol"))
  k <- PK$PK[[1]]
  js <- pkConvertJS(sprintf(paste0(
    "(function() { var m = P.fromMicro(3, %.17g, {k10: %.17g, k12: %.17g, k13: %.17g, k21: %.17g, k31: %.17g}).model;",
    " var out = {}; ['plasma', 'effect'].forEach(function(site) {",
    "   var tr = P.tciTrajectory(m, %.17g, site, 30);",
    "   out[site] = {t: tr.states.map(function(s) { return s.t; }),",
    "                p: tr.states.map(function(s) { return s.p; }),",
    "                e: tr.states.map(function(s) { return s.e; })}; });",
    " return out; })()"),
    k$v1, k$k10, k$k12, k$k13, k$k21, k$k31, k$ke0))
  # R/tci.R's tciRate() and tciKinetics(), driven the same way: a target of 1
  # from zero, the interval lengthening once the plasma is held
  K <- tciKinetics(PK)
  for (site in c("plasma", "effect")) {
    p <- numeric(3); e <- numeric(4); t <- 0; dt <- TCI_INTERVAL
    dtMax <- max(TCI_INTERVAL, min(10, 30 / 2000))
    i <- 0
    while (t < 30 - 1e-9) {
      step <- min(dt, 30 - t)
      x <- tciRate(p, e, 1, site, step, K)
      d3 <- exp(-K$lambda3 * step); d4 <- exp(-K$lambda4 * step)
      p <- p * d3 + K$pinf * x$rate * (1 - d3)
      e <- e * d4 + K$einf * x$rate * (1 - d4)
      t <- t + step; i <- i + 1
      dt <- if (x$rate > 0 && !x$effectMode && t > K$tPeak) min(dt * 1.1, dtMax) else TCI_INTERVAL
    }
    js_site <- js[[site]]
    expect_equal(length(js_site$t), i, info = site)
    expect_equal(js_site$p[i, ], p, tolerance = 1e-8, info = site)
    expect_equal(js_site$e[i, ], e, tolerance = 1e-8, info = site)
  }
  # The effect site is held at its target, and the plasma at its own
  expect_equal(sum(js$effect$e[nrow(js$effect$e), ]), 1, tolerance = 1e-4)
  expect_equal(sum(js$plasma$p[nrow(js$plasma$p), ]), 1, tolerance = 1e-8)
})

test_that("decrement times agree with an independent root search", {
  skipWithoutNode()
  # One compartment: every plasma decrement time is the half-life's multiple
  js <- pkConvertJS("P.decrementCurves(P.fromVolumesClearances(1, [8], [4]).model, 0, 50, 60, 10)")
  expect_null(js$effect)
  expect_equal(js$plasma$time, rep(log(2) / 0.5, length(js$plasma$time)), tolerance = 1e-9)
  # Three compartments: the 20, 50 and 80% times from the converter's own
  # end-of-infusion states, found again by uniroot() in R
  js <- pkConvertJS(paste0(
    "(function() { var m = P.fromVolumesClearances(3, [10, 100, 1000], [3, 2, 1]).model;",
    " var ke0 = P.ke0FromPeakTime(m, 1.6), tr = P.tciTrajectory(m, ke0, 'effect', 120);",
    " var s = tr.states[tr.states.length - 1];",
    " return {l: tr.K.lambda4, e: s.e,",
    "   d: [0.2, 0.5, 0.8].map(function(f) { return P.decrementTime(s.e, tr.K.lambda4, f); })}; })()"))
  ce <- function(t) sum(js$e * exp(-js$l * t))
  ref <- vapply(c(0.2, 0.5, 0.8), function(f)
    uniroot(function(t) ce(t) - (1 - f) * ce(0), c(1e-9, 1e5), tol = 1e-12)$root, numeric(1))
  expect_equal(js$d, ref, tolerance = 1e-8)
  # A larger decrease takes longer, and the plasma's 50% time grows with the
  # length of the infusion (the context-sensitive half-time)
  expect_true(all(diff(js$d) > 0))
  c50 <- pkConvertJS("P.decrementCurves(P.fromVolumesClearances(3, [10, 100, 1000], [3, 2, 1]).model, 0, 50, 600, 50).plasma.time")
  expect_true(all(diff(c50) > 0))
})

test_that("each decrement curve starts once its site has reached the target", {
  skipWithoutNode()
  js <- pkConvertJS(paste0(
    "(function() { var m = P.fromVolumesClearances(3, [10, 100, 1000], [3, 2, 1]).model;",
    " return P.decrementCurves(m, P.ke0FromPeakTime(m, 1.6), 50, 600, 400); })()"))
  # The plasma reaches its target in the first 10-s interval; the effect site
  # within 5% of it shortly before its 1.6-min peak
  expect_equal(js$plasma$duration[1], 10 / 60, tolerance = 1e-9)
  expect_gt(js$effect$duration[1], 1)
  expect_lt(js$effect$duration[1], 1.6)
  expect_true(all(is.finite(c(js$plasma$time, js$effect$time))))
  expect_equal(max(js$plasma$duration), 600)
})

test_that("one- and two-compartment models have an effect-site curve too", {
  skipWithoutNode()
  # One compartment held long at Cp = Ce = 1: after the stop
  # Ce(t) = (ke0 exp(-k t) - k exp(-ke0 t)) / (ke0 - k), here k = 0.5, ke0 = 0.2
  one <- pkConvertJS("P.decrementCurves(P.fromVolumesClearances(1, [8], [4]).model, 0.2, 50, 120, 20).effect")
  ce <- function(t) (0.2 * exp(-0.5 * t) - 0.5 * exp(-0.2 * t)) / (0.2 - 0.5)
  ref <- uniroot(function(t) ce(t) - 0.5, c(0, 100), tol = 1e-12)$root
  expect_equal(tail(one$time, 1), ref, tolerance = 1e-6)
  # Two compartments: the effect site is held at its target and every point is finite
  two <- pkConvertJS(paste0(
    "(function() { var m = P.fromVolumesClearances(2, [5, 50], [3.5, 2.5]).model;",
    " var tr = P.tciTrajectory(m, 0.5, 'effect', 30), s = tr.states[tr.states.length - 1];",
    " return {ce: s.e.reduce(function(a, b) { return a + b; }, 0),",
    "   curve: P.decrementCurves(m, 0.5, 50, 60, 20).effect}; })()"))
  expect_equal(two$ce, 1, tolerance = 1e-4)
  expect_gt(length(two$curve$time), 10)
  expect_true(all(is.finite(two$curve$time)))
})

test_that("a site that never reaches its target has no decrement curve", {
  skipWithoutNode()
  # ke0 0.2 /min: after a one-minute effect-site infusion Ce is still rising
  js <- pkConvertJS("P.decrementCurves(P.fromVolumesClearances(1, [8], [4]).model, 0.2, 50, 1, 20)")
  expect_length(js$effect$time, 0)
  expect_length(js$effect$duration, 0)
  expect_gt(length(js$plasma$time), 0)
})

# --- Bounded work (the UI review of 2026-11-04) ----------------------------
#
# The page computes on the browser's one thread, so an entry it cannot finish
# freezes the tab.  Two did, on the spreadsheet's three-compartment example:
# ke0 = 1e-300 (a peak 916,252 min out, 55 million search points for each
# update of the controller) and an infusion of 1e-323 min (the axis step
# underflowed to 0, and the tick loop never advanced).

test_that("the axis ticks are bounded and finite for any range", {
  skipWithoutNode()
  js <- pkConvertJS(paste0(
    "[1e-323, 5e-324, 1e-300, 1e308, 1.7976931348623157e308, Infinity, NaN, 0, -1, 600]",
    ".map(function(x) { var t = P.axisTicks(x, 6).ticks; ",
    "  return {n: t.length, finite: t.every(isFinite), last: t[t.length - 1]}; })"))
  expect_true(all(js$n >= 1 & js$n <= pkConvertJS("P.limits.ticks") + 1))
  expect_true(all(js$finite))
  # An ordinary range keeps its round ticks
  expect_equal(pkConvertJS("P.axisTicks(600, 6).ticks"), seq(0, 600, by = 100))
  # A range the step underflows gives its two ends
  expect_equal(js$n[1], 2)
})

test_that("a ke0 too small to simulate stops the effect site, not the page", {
  skipWithoutNode()
  t0 <- Sys.time()
  js <- pkConvertJS(paste0(
    "(function() { var m = P.fromVolumesClearances(3, [10, 100, 1000], [3, 2, 1]).model;",
    "  var c = P.decrementCurves(m, 1e-300, 50, 600, 400), d = P.decrementCurves(m, 1e-4, 50, 600, 400);",
    "  return {tiny: c.effect, plasma: c.plasma.time.length, slow: d.effect.aborted}; })()"))
  expect_lt(as.numeric(difftime(Sys.time(), t0, units = "secs")), 15)
  expect_true(js$tiny$aborted)
  expect_length(js$tiny$time, 0)
  expect_true(js$slow)
  # The plasma curve needs no search, and is drawn as before
  expect_gt(js$plasma, 100)
})

test_that("a slow but feasible effect site is still simulated in full", {
  skipWithoutNode()
  # ke0 3e-4 /min, a peak 5 h after a bolus: within the work limit
  js <- pkConvertJS(paste0(
    "(function() { var m = P.fromVolumesClearances(3, [10, 100, 1000], [3, 2, 1]).model;",
    "  return P.decrementCurves(m, 3e-4, 50, 600, 400).effect; })()"))
  expect_null(js$aborted)
  expect_gt(length(js$time), 100)
})

test_that("an infusion of years stops at the update limit", {
  skipWithoutNode()
  js <- pkConvertJS(paste0(
    "(function() { var m = P.fromVolumesClearances(3, [10, 100, 1000], [3, 2, 1]).model;",
    "  var tr = P.tciTrajectory(m, 0.5, 'effect', 1e300);",
    "  return {aborted: tr.aborted, n: tr.states.length, limit: P.limits.updates}; })()"))
  expect_true(js$aborted)
  expect_lte(js$n, js$limit)
})
