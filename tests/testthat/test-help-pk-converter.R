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
