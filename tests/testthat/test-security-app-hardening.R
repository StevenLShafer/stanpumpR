# The app hardening that followed the external security audit of 2026-11-02
# (every claim re-checked; the bookmark findings in a headless browser).

dd <- getDrugDefaultsGlobal()

test_that("a link's dose table is rebuilt from what the grid would accept", {
  link <- list(
    Drug  = c("propofol", "<img src=x onerror=alert(1)>", "fentanyl", "fentanyl", "", "propofol"),
    Time  = c("0", "<b>t</b>", "5", "<img>", "", strrep("9", 5000)),
    Dose  = c("2", "", "50", "1", "", "1"),
    Units = c("mg/kg", "<img>", "furlongs", "mcg", "", "mg/kg"))
  r <- restoredDoseTable(link, dd)
  expect_true(r$changed)
  expect_identical(names(r$table), c("Drug", "Time", "Dose", "Units"))
  expect_true(all(vapply(r$table, is.character, TRUE)))
  # the good row as it was
  expect_identical(unlist(r$table[1, ]), c(Drug = "propofol", Time = "0", Dose = "2", Units = "mg/kg"))
  # an unknown drug empties its row
  expect_identical(unlist(r$table[2, ], use.names = FALSE), rep("", 4))
  # a unit the drug does not offer is blanked, never swapped for its default
  expect_identical(r$table$Units[3], "")
  # an unreadable time is blanked; the rest of the row stays
  expect_identical(unlist(r$table[4, ], use.names = FALSE), c("fentanyl", "", "1", "mcg"))
  expect_identical(r$table$Time[6], "")
  # nothing HTML-shaped survives anywhere
  expect_false(any(grepl("<", unlist(r$table))))
  # and the table the grid draws validates
  expect_true(validateDoseTableInput(r$table, dd))
})

test_that("an ordinary bookmark's dose table is restored unchanged", {
  link <- list(Drug = c("propofol", "fentanyl", "", ""), Time = c("0", "10", "", ""),
               Dose = c("2", "50", "", ""), Units = c("mg/kg", "mcg", "", ""))
  r <- restoredDoseTable(link, dd)
  expect_false(r$changed)
  expect_identical(r$table, as.data.frame(link, stringsAsFactors = FALSE))
  # Shiny parses a numeric dose column as numbers
  r <- restoredDoseTable(list(Drug = "propofol", Time = "0", Dose = 2, Units = "mg/kg"), dd)
  expect_false(r$changed)
  expect_identical(r$table$Dose, "2")
})

test_that("every teaching scenario's dose table survives the restore unchanged", {
  n <- 0
  for (sc in helpScenarios()) {
    dt <- helpScenarioDoseTable(sc)
    if (is.null(dt) || nrow(dt) == 0) next
    # what the bookmark carries: the table's columns, as Shiny parses them
    r <- restoredDoseTable(jsonlite::fromJSON(jsonlite::toJSON(as.list(dt))), dd)
    expect_false(r$changed, info = sc$id)
    n <- n + 1
  }
  expect_gt(n, 50)
})

test_that("a link whose tables are not tables neither crashes nor restores", {
  # DT = "abc" ended the session at startup
  expect_false(isBookmarkRestore(list(DT = "abc")))
  expect_true(opensOnStartupMenu(list(DT = "abc")))
  expect_true(isBookmarkRestore(list(DT = list(Drug = "propofol"))))
  for (x in list("abc", 1, list(Foo = 1), list(Drug = list(list(1, 2)), Time = "0", Dose = "1", Units = "mg"))) {
    r <- restoredDoseTable(x, dd)
    expect_true(r$changed)
    expect_true(validateDoseTableInput(r$table, dd))
  }
  # too many rows: capped at what the grid holds
  n <- MAX_DOSE_ROWS + 50
  r <- restoredDoseTable(list(Drug = rep("propofol", n), Time = rep("0", n),
                              Dose = rep("1", n), Units = rep("mg", n)), dd)
  expect_true(r$changed)
  expect_equal(nrow(r$table), MAX_DOSE_ROWS - 1L)
})

test_that("a link's event table keeps only known events at readable times", {
  ev <- getEventDefaults()
  known <- ev$Event[1]
  r <- restoredEventTable(list(Time = c(5, 10, -1, NA), Event = c(known, "<img>", known, known)),
                          ev, eventTableInit)
  expect_true(r$changed)
  expect_identical(r$table, data.frame(Time = 5, Event = known, stringsAsFactors = FALSE))
  expect_true(validateEventTableInput(r$table, ev))
  # no Event column stopped the restore part way
  expect_identical(restoredEventTable(list(Foo = 1), ev, eventTableInit)$table, eventTableInit)
  # an empty one is no change
  r <- restoredEventTable(list(Time = list(), Event = list()), ev, eventTableInit)
  expect_false(r$changed)
  expect_identical(r$table, eventTableInit)
})

test_that("a table goes to the server's log as its size only", {
  out <- capture.output(withr::with_options(list(ECHO_OUTPUT_COMMENTS = TRUE),
    outputComments(data.frame(Drug = "propofol", Dose = 2))))
  expect_match(paste(out, collapse = "\n"), "<table: 1 rows x 2 columns>")
  expect_false(any(grepl("propofol", out)))
  # nor does the email recipient
  src <- deparse(sendSlide)
  expect_false(any(grepl("outputComments\\([^)]*recipient", src)))
})

test_that("Email Slide is bounded across sessions, per worker and per recipient", {
  old <- .emailSends$log
  on.exit(.emailSends$log <- old)
  .emailSends$log <- NULL
  t0 <- as.POSIXct("2026-11-02 12:00:00", tz = "UTC")
  for (i in seq_len(EMAIL_RECIPIENT_LIMIT_PER_HOUR)) {
    expect_null(emailRateProblem("A@example.org", t0))
    recordEmailSend("a@example.org ", t0)
  }
  # the same address, however written
  expect_match(emailRateProblem("a@EXAMPLE.org", t0), "That address")
  expect_null(emailRateProblem("b@example.org", t0))
  # an hour later it is forgotten
  expect_null(emailRateProblem("a@example.org", t0 + EMAIL_LIMIT_WINDOW_SECONDS + 1))
  .emailSends$log <- NULL
  for (i in seq_len(EMAIL_WORKER_LIMIT_PER_HOUR)) recordEmailSend(paste0(i, "@example.org"), t0)
  expect_match(emailRateProblem("new@example.org", t0), "this hour")
})

test_that("the page's own policy: CSP directives, referrer, and an inline script no value can close", {
  # app_ui() uses numericInput(updateOn = ), which needs shiny 1.10
  skip_if_not_installed("shiny", "1.10.0")
  oldConfig <- .sprglobals$config
  .sprglobals$config <- DEFAULT_CONFIG
  on.exit(.sprglobals$config <- oldConfig)
  tags <- htmltools::renderTags(app_ui()(list()))
  html <- paste(as.character(tags$head), as.character(tags$html))
  # the attribute is HTML-escaped: ' is &#39;
  html <- gsub("&#39;", "'", html, fixed = TRUE)
  expect_match(html, "form-action 'self'", fixed = TRUE)
  expect_match(html, "object-src 'none'", fixed = TRUE)
  expect_match(html, "base-uri 'self'", fixed = TRUE)
  expect_match(html, '<meta name="referrer" content="same-origin"', fixed = TRUE)
  script <- regmatches(html, regexpr("var drug_defaults=.*?</script>", html, perl = TRUE))
  expect_length(script, 1)
  expect_false(grepl("<", sub("</script>$", "", script), fixed = TRUE))
})

# A tripwire, not a defence: the help pages and the API guide are repository
# Markdown rendered with its raw HTML allowed (shiny::markdown()), so a
# merged change could carry script.  What they render to may hold none.
test_that("the rendered help and API guide carry no script, handler or foreign embed", {
  root <- system.file(package = "stanpumpR")
  files <- c(list.files(file.path(root, "help"), "\\.md$", recursive = TRUE, full.names = TRUE),
             list.files(file.path(root, "apidocs"), "\\.md$", recursive = TRUE, full.names = TRUE))
  expect_gt(length(files), 100)
  bad <- "<script|<iframe|<object|<embed|<form|<base[ >]|<meta[ >]|<link[ >]|javascript:|\\son[a-z]+\\s*=|src=\"(https?:)?//"
  for (f in files) {
    html <- as.character(shiny::markdown(readLines(f, warn = FALSE, encoding = "UTF-8")))
    hit <- regmatches(html, regexpr(bad, html, ignore.case = TRUE, perl = TRUE))
    expect_length(hit, 0)
    if (length(hit)) message(basename(f), ": ", hit)
  }
})

test_that("a drug table value of </script> cannot close the inline script", {
  skip_if_not_installed("shiny", "1.10.0")
  oldConfig <- .sprglobals$config
  .sprglobals$config <- DEFAULT_CONFIG
  on.exit(.sprglobals$config <- oldConfig)
  hostile <- dd
  hostile$Category[1] <- "</script><b id=x>"
  local_mocked_bindings(getDrugDefaultsGlobal = function(...) hostile)
  tags <- htmltools::renderTags(app_ui()(list()))
  html <- paste(as.character(tags$head), as.character(tags$html))
  script <- regmatches(html, regexpr("var drug_defaults=.*?</script>", html, perl = TRUE))
  # jsonlite already writes "/" as "\\/"; "<" is now "\\u003c" as well
  expect_match(script, "\\u003c\\/script>\\u003cb id=x>", fixed = TRUE)
  expect_false(grepl("<b id=x>", html, fixed = TRUE))
})
