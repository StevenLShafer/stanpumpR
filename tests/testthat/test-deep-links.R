# Links that open the app on a scenario or a help page (R/deep-links.R).
# Drafted by Claude Code, 2026-10-14, at the request of Steven L. Shafer.

local_mocked_bindings(outputComments = function(...) {})

dd <- getDrugDefaultsGlobal()
registry <- helpPageRegistry(dd)

test_that("?scenario= names a scenario and ?help= a page", {
  link <- deepLinkFromQuery("?scenario=propofol-bolus", dd, registry)
  expect_identical(link$scenario$id, "propofol-bolus")
  expect_null(link$help)
  expect_length(link$problems, 0)
  expect_true(deepLinkOpens(link))

  link <- deepLinkFromQuery("?help=scenarios%2Fpropofol-bolus", dd, registry)
  expect_null(link$scenario)
  expect_identical(link$help, "scenarios/propofol-bolus")
  expect_true(deepLinkOpens(link))

  link <- deepLinkFromQuery("?debug=1&scenario=tci-propofol&help=tci", dd, registry)
  expect_identical(link$scenario$id, "tci-propofol")
  expect_identical(link$help, "tci")
})

test_that("a URL without a link opens as usual", {
  for (q in list(NULL, "", "?debug=1", "?_inputs_&age=40", NA_character_, "?scenario=&help=")) {
    link <- deepLinkFromQuery(q, dd, registry)
    expect_null(link$scenario)
    expect_null(link$help)
    expect_length(link$problems, 0)
    expect_false(deepLinkOpens(link))
  }
})

test_that("an unknown scenario is a warning, an unknown page the not-found page", {
  link <- deepLinkFromQuery("?scenario=no-such-scenario", dd, registry)
  expect_null(link$scenario)
  expect_match(link$problems, "no teaching scenario called \"no-such-scenario\"")
  expect_false(deepLinkOpens(link))

  link <- deepLinkFromQuery("?help=no/such/page", dd, registry)
  expect_identical(link$help, "not-found")
  expect_true(deepLinkOpens(link))

  # Too long to be an id: ignored, not an error
  long <- strrep("a", DEEP_LINK_MAX_LENGTH + 1)
  link <- deepLinkFromQuery(paste0("?scenario=", long, "&help=", long), dd, registry)
  expect_false(deepLinkOpens(link))
  expect_length(link$problems, 0)
})

test_that("a scenario the current drug library cannot run is refused with its reason", {
  broken <- dd[dd$Drug != "propofol", ]
  link <- deepLinkFromQuery("?scenario=propofol-bolus", broken, helpPageRegistry(broken))
  expect_null(link$scenario)
  expect_match(link$problems, "cannot be loaded with this drug library")
})

test_that("every scenario and every help page can be linked to", {
  for (s in helpScenarios()) {
    expect_identical(deepLinkFromQuery(deepLinkToScenario(s$id), dd, registry)$scenario$id, s$id)
  }
  for (id in registry$id) {
    expect_identical(deepLinkFromQuery(paste0("?help=", utils::URLencode(id, reserved = TRUE)),
                                       dd, registry)$help, id)
  }
})

test_that("a scenario's page carries its own link", {
  html <- helpScenarioPageHTML("propofol-bolus")
  expect_match(html, 'href="?scenario=propofol-bolus" target="_blank"', fixed = TRUE)
  expect_match(html, "<code>?scenario=propofol-bolus</code>", fixed = TRUE)
})

# --- In the running app ------------------------------------------------------

oldConfig <- .sprglobals$config
.sprglobals$config <- DEFAULT_CONFIG
withr::defer(.sprglobals$config <- oldConfig)

# The browser's URL cannot be set on a mock session, so the parsed link is
# mocked instead, and the server's response to it tested.
test_that("a ?scenario= link loads the scenario instead of opening the drug menu", {
  s <- helpScenarioById("tiva-remifentanil-propofol")
  local_mocked_bindings(deepLinkFromQuery = function(...) list(scenario = s, help = NULL, problems = character(0)))
  shiny::testServer(app_server, {
    expect_false(startupMenuOpen)
    expect_identical(doseTable(), helpScenarioDoseTable(s))
    expect_identical(eventTable(), helpScenarioEventTable(s))
    expect_equal(doseTableFormat(), timeFormat(s$options$timeUnits, "relative"))
    expect_identical(doseTableDraft(), doseTable())
    expect_false(doseTableHistory()$can_undo)
    expect_identical(helpPage(), "home")
  })
})

test_that("a ?help= link opens the help on the page, with the default drugs behind it", {
  local_mocked_bindings(deepLinkFromQuery = function(...) list(scenario = NULL, help = "tci", problems = character(0)))
  shiny::testServer(app_server, {
    expect_false(startupMenuOpen)
    expect_identical(helpPage(), "tci")
    expect_identical(doseTable(), doseTableInit)
  })
})

test_that("both together load the scenario and open on the page", {
  s <- helpScenarioById("propofol-bolus")
  local_mocked_bindings(deepLinkFromQuery = function(...) list(scenario = s, help = "scenarios/propofol-bolus", problems = character(0)))
  shiny::testServer(app_server, {
    expect_false(startupMenuOpen)
    expect_identical(doseTable(), helpScenarioDoseTable(s))
    expect_identical(helpPage(), "scenarios/propofol-bolus")
  })
})

test_that("an unknown scenario opens the drug menu as usual, with a warning", {
  local_mocked_bindings(deepLinkFromQuery = function(...) {
    list(scenario = NULL, help = NULL, problems = "There is no teaching scenario called \"x\".")
  })
  shiny::testServer(app_server, {
    expect_true(startupMenuOpen)
    expect_identical(doseTable(), doseTableBlank)
  })
})
