# Tests for routing the inhaled gases away from the intravenous pipeline.
#
# The gases have no drugs_*.R covariate function and no mass dose, so they must
# never reach recalculatePK() or simCpCe().  These tests pin the partition and
# check that a dose table containing gases still simulates the IV drugs.

test_that("the gas entries are registered in the drug defaults table", {
  gases <- gasDrugNames()
  expect_setequal(
    gases,
    c("air", "oxygen", "nitrousOxide", "sevoflurane", "isoflurane", "ventilation")
  )

  d <- getDrugDefaultsGlobal()
  expect_true("Class" %in% names(d))
  # Every pre-existing drug must still be classified as intravenous
  expect_true(all(d$Class[d$Drug == "propofol"] == "IV"))
  expect_equal(sum(d$Class == "gas"), 6)

  # Units offered in the dose table
  expect_equal(unlist(d$Units[d$Drug == "oxygen"]), "L/min")
  expect_equal(unlist(d$Units[d$Drug == "ventilation"]), "L/min")
  expect_equal(unlist(d$Units[d$Drug == "sevoflurane"]), "%")
})


test_that("isGasDrug partitions drug names", {
  expect_true(all(isGasDrug(c("air", "oxygen", "sevoflurane", "ventilation"))))
  expect_false(any(isGasDrug(c("propofol", "remifentanil", "fentanyl"))))
  expect_equal(isGasDrug(c("propofol", "oxygen")), c(FALSE, TRUE))
  expect_length(isGasDrug(character(0)), 0)
})


test_that("simulateGases returns NULL when there are no gases", {
  expect_null(simulateGases(NULL))
  expect_null(simulateGases(data.frame(Drug = character(0), Time = numeric(0),
                                       Dose = numeric(0))))
  ivOnly <- data.frame(Drug = c("propofol", "remifentanil"), Time = c(0, 0),
                       Dose = c(200, 100), stringsAsFactors = FALSE)
  expect_null(simulateGases(ivOnly))
})


test_that("simulateGases picks the gas rows out of a mixed dose table", {
  DT <- data.frame(
    Drug  = c("propofol", "oxygen", "sevoflurane", "ventilation", "remifentanil"),
    Time  = c(0, 0, 0, 0, 0),
    Dose  = c(200, 2, 2, 4, 100),
    Units = c("mg", "L/min", "%", "L/min", "mcg"),
    stringsAsFactors = FALSE
  )
  sim <- simulateGases(DT, weight = 70, age = 40, maximum = 30)

  expect_false(is.null(sim))
  # The intravenous drugs must not appear in the gas output
  expect_false(any(c("propofol", "remifentanil") %in% sim$results$Drug))
  expect_true(all(c("sevoflurane", "oxygen", "nitrousOxide", "MAC") %in%
                    sim$results$Drug))

  brain <- sim$results[sim$results$Drug == "sevoflurane" &
                         sim$results$Site == "Brain", "Y"]
  expect_gt(max(brain), 0.3)
  expect_lt(max(brain), 2)
})


test_that("getDrugPK cannot handle a gas, which is why the partition exists", {
  # "oxygen" is in the drug defaults table now, so it passes getDrugPK()'s name
  # check and then fails at eval(call("oxygen", ...)) -- the gases have no
  # drugs_*.R covariate function.  This is exactly the failure that routing the
  # gas rows away from the intravenous path prevents.
  expect_error(
    getDrugPK("oxygen", weight = 70, height = 170, age = 50, sex = "male",
              drugDefaults = getDrugDefaults("oxygen")),
    "could not find function"
  )
})


test_that("a dose table containing gases still simulates the IV drugs", {
  DT <- data.frame(
    Drug  = c("propofol", "oxygen", "sevoflurane", "remifentanil", "ventilation"),
    Time  = c(0, 0, 0, 0, 0),
    Dose  = c(200, 2, 2, 60, 4),
    Units = c("mg", "L/min", "%", "mcg", "L/min"),
    stringsAsFactors = FALSE
  )
  ET <- data.frame(Time = double(), Event = character(), Fill = character())

  # This is the partition performed by doseTableIV() in app_server.R
  ivDT <- DT[!isGasDrug(DT$Drug), , drop = FALSE]
  expect_setequal(unique(ivDT$Drug), c("propofol", "remifentanil"))

  out <- simulateDrugsWithCovariates(ivDT, ET, weight = 70, height = 170,
                                     age = 50, sex = "male", maximum = 60,
                                     plotRecovery = FALSE)
  expect_setequal(names(out), c("propofol", "remifentanil"))
  expect_gt(max(out$propofol$equiSpace$Ce), 0)
  expect_gt(max(out$remifentanil$equiSpace$Ce), 0)
})


test_that("simulateDrugsWithCovariates skips gas rows rather than failing", {
  # The exported, Shiny-free entry point must survive a mixed table too, since
  # vignettes and scripts call it directly.
  DT <- data.frame(
    Drug  = c("propofol", "oxygen", "sevoflurane"),
    Time  = c(0, 0, 0),
    Dose  = c(200, 2, 2),
    Units = c("mg", "L/min", "%"),
    stringsAsFactors = FALSE
  )
  ET <- data.frame(Time = double(), Event = character(), Fill = character())

  out <- simulateDrugsWithCovariates(DT, ET, weight = 70, height = 170,
                                     age = 50, sex = "male", maximum = 60,
                                     plotRecovery = FALSE)
  expect_named(out, "propofol")
})


test_that("processdoseTable tolerates an empty drug list", {
  # A dose table containing only gases leaves the IV drug list empty.  The old
  # 1:length(drugList) form produced c(1, 0) and indexed a NULL drug.
  expect_no_error(processdoseTable(NULL, NULL, list(), 60, FALSE))
  expect_equal(processdoseTable(NULL, NULL, list(), 60, FALSE), list())
})
