# The Patient Profile and Graph Options show only the controls that apply to
# the drugs in the dose table (relevantFields() in R/patient-fields.R), and
# the Opioid tolerance field raises every opioid's MEAC (recalculatePK()).

test_that("with no drug, no optional control is shown", {
  shown <- relevantFields(character(0))
  expect_named(shown, RELEVANT_FIELDS)
  expect_false(any(shown))
  expect_false(any(relevantFields(c("", NA))))
})

test_that("each control follows the drugs it applies to", {
  shown <- relevantFields(c("fentanyl", "propofol"))
  expect_true(all(shown[c("opioidTolerance", "showMEAC", "plotMEAC", "plotInteraction",
                          "mintoSchnider")]))
  expect_false(any(shown[c("osmolality", "opioidMacInteraction", "hamberg", "cyp4f2",
                           ASM_INTERACTIONS_INPUT)]))

  # An opioid without a MEAC still counts (codeine is in the Opioids
  # category), and the field its model reads appears
  shown <- relevantFields("codeine")
  expect_true(shown[["opioidTolerance"]])
  expect_true(shown[["cyp2d6"]])
  expect_false(shown[["plotInteraction"]])

  expect_true(relevantFields("mannitol")[["osmolality"]])
  expect_true(relevantFields(c("sevoflurane", "fentanyl"))[["opioidMacInteraction"]])
  expect_false(relevantFields("sevoflurane")[["opioidMacInteraction"]])
  expect_true(all(relevantFields("warfarin")[c("vkorc1", "cyp4f2", "hamberg")]))
  expect_true(relevantFields("carbamazepine")[[ASM_INTERACTIONS_INPUT]])
  expect_true(relevantFields("methadone")[["cyp2b6"]])
})

test_that("a covariate field is shown exactly for the drugs whose model reads it", {
  dd <- getDrugDefaultsGlobal()
  for (field in names(PATIENT_FIELD_LABELS)) {
    readers <- patientFieldReaders(field, dd)
    expect_true(length(readers) > 0, info = field)
    expect_true(relevantFields(readers[1], dd)[[field]], info = field)
    others <- setdiff(dd$Drug, readers)[1:5]
    expect_false(relevantFields(others, dd)[[field]], info = field)
  }
})

test_that("every hideable control is wrapped where the server can find it", {
  # app_ui() uses numericInput(updateOn = ), which needs shiny 1.10
  skip_if_not_installed("shiny", "1.10.0")
  oldConfig <- .sprglobals$config
  .sprglobals$config <- DEFAULT_CONFIG
  on.exit(.sprglobals$config <- oldConfig)
  html <- as.character(htmltools::renderTags(app_ui()(list()))$html)
  for (field in setdiff(RELEVANT_FIELDS, c("plotMEAC", "plotInteraction")))
    expect_match(html, sprintf('id="field_%s"', field), fixed = TRUE, info = field)
  expect_match(html, 'id="opioidTolerance"', fixed = TRUE)
  # The Additional plots are in Graph Options, no longer a panel of their own
  expect_match(html, "Additional plots", fixed = TRUE)
  expect_no_match(html, "Additional Plots", fixed = TRUE)
})

test_that("the tooltips show on hover only, so a click does not pin them", {
  tip <- hoverTooltip(shiny::span("x"), "tip")
  expect_match(as.character(tip), "&quot;trigger&quot;:&quot;hover&quot;", fixed = TRUE)
})

test_that("opioid tolerance multiplies every opioid's MEAC by its factor", {
  dd <- getDrugDefaultsGlobal()
  DT <- data.frame(Drug = c("fentanyl", "propofol"), Time = 0, Dose = 1,
                   Units = c("mcg", "mg"))
  meac <- function(tolerance) {
    drugs <- recalculatePK(NULL, dd, DT, 40, 70, 170, "male",
                           opioidTolerance = tolerance)
    c(fentanyl = drugs$fentanyl$MEAC, propofol = drugs$propofol$MEAC)
  }
  base <- dd$MEAC[dd$Drug == "fentanyl"]
  expect_equal(meac("none"), c(fentanyl = base, propofol = 0))
  expect_equal(meac("mild"), c(fentanyl = 1.5 * base, propofol = 0))
  expect_equal(meac("somewhat"), c(fentanyl = 2 * base, propofol = 0))
  expect_equal(meac("high"), c(fentanyl = 3 * base, propofol = 0))
  # the default is no tolerance
  expect_equal(recalculatePK(NULL, dd, DT, 40, 70, 170, "male")$fentanyl$MEAC, base)
  expect_error(meac("extreme"))
})
