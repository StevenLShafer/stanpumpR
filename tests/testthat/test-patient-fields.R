# The Patient Profile's hover tips name the drugs each pharmacogenetic field
# and the serum creatinine reach (R/patient-fields.R).  getDrugPK() passes a
# covariate only to the models that name it in their signature, so the
# signatures are what the tips must agree with.

patientFieldArgs <- c("cyp2d6", "cyp2c19", "cyp2c9", "cyp2b6", "creatinine")

test_that("the readers of each field are exactly the models that name it", {
  dd <- getDrugDefaultsGlobal()
  for (arg in patientFieldArgs) {
    readers <- patientFieldReaders(arg, dd)
    expect_gt(length(readers), 0, label = arg)
    expect_identical(readers, sort(readers))
    for (drug in dd$Drug[!isGasDrug(dd$Drug)]) {
      declares <- arg %in% names(formals(get(drug, mode = "function")))
      expect_identical(drug %in% readers, declares, info = paste(drug, arg))
    }
  }
})

test_that("the known readers are in their lists", {
  expect_true(all(c("codeine", "tramadol", "oxycodone", "hydrocodone",
                    "aripiprazole", "haloperidol", "risperidone") %in%
                    patientFieldReaders("cyp2d6")))
  expect_setequal(patientFieldReaders("cyp2c19"), c("citalopram", "escitalopram"))
  expect_setequal(patientFieldReaders("cyp2c9"), c("meloxicam", "phenytoin", "warfarin"))
  expect_identical(patientFieldReaders("cyp2b6"), "methadone")
  expect_true(all(c("cefazolin", "gentamicin", "mannitol", "vancomycin",
                    "sugammadex", "gabapentin", "pregabalin", "oxycodone",
                    "meloxicam", "lacosamide", "levetiracetam",
                    "apixaban", "rivaroxaban") %in%
                    patientFieldReaders("creatinine")))
})

test_that("each tooltip names every drug that reads its field, and the rest ignore it", {
  for (arg in patientFieldArgs) {
    tip <- patientFieldTooltip(arg)
    expect_type(tip, "character")
    expect_length(tip, 1)
    for (drug in patientFieldReaders(arg)) {
      expect_true(grepl(drug, tip, fixed = TRUE),
                  info = paste(drug, "reads", arg, "but its tooltip does not say so"))
    }
    expect_match(tip, "no other drug is affected", fixed = TRUE)
  }
})

test_that("the readers sentence is an English list, with a drug's note in parentheses", {
  expect_identical(patientFieldList("a"), "a")
  expect_identical(patientFieldList(c("a", "b")), "a and b")
  expect_identical(patientFieldList(c("a", "b", "c")), "a, b and c")
  expect_identical(patientFieldList(character(0)), "")

  # Meloxicam's renal term is on its intravenous system only
  expect_match(patientFieldReadersSentence("creatinine"), "meloxicam (intravenous only)", fixed = TRUE)
  expect_false(grepl("(", patientFieldReadersSentence("cyp2b6"), fixed = TRUE))
  expect_identical(patientFieldReadersSentence("cyp2b6"),
                   "Read by methadone; no other drug is affected.")

  # A field no drug reads says so rather than printing an empty list
  none <- data.frame(Drug = "propofol", stringsAsFactors = FALSE)
  expect_identical(patientFieldReadersSentence("cyp2b6", none),
                   "No drug in the library reads it.")
})

test_that("the creatinine tooltip quotes the assumed adult values from R/renalFunction.R", {
  tip <- patientFieldTooltip("creatinine")
  expect_match(tip, sprintf("%s mg/dL for a man, %s for a woman", SCR_ASSUMED_MALE, SCR_ASSUMED_FEMALE),
               fixed = TRUE)
  expect_match(tip, "0.9 mg/dL for a man, 0.8 for a woman", fixed = TRUE)
  expect_match(tip, "Leave blank", fixed = TRUE)
  expect_match(tip, "Renal impairment is then not represented", fixed = TRUE)
  # The pharmacogenetic tips say nothing about a blank field
  for (arg in setdiff(patientFieldArgs, "creatinine"))
    expect_false(grepl("Leave blank", patientFieldTooltip(arg), fixed = TRUE))
})

test_that("an unknown field is refused", {
  expect_error(patientFieldTooltip("weight"))
})

test_that("the Patient Profile carries the five hover tips", {
  skip_if_not_installed("bslib")
  # app_ui() uses numericInput(updateOn = ), which needs shiny 1.10
  skip_if_not_installed("shiny", "1.10.0")
  oldConfig <- .sprglobals$config
  .sprglobals$config <- DEFAULT_CONFIG
  on.exit(.sprglobals$config <- oldConfig)
  html <- as.character(htmltools::renderTags(app_ui()(list()))$html)
  for (arg in patientFieldArgs) {
    tip <- patientFieldTooltip(arg)
    expect_true(grepl(htmltools::htmlEscape(tip), html, fixed = TRUE),
                info = paste("the", arg, "tooltip is not in the UI"))
  }
})
