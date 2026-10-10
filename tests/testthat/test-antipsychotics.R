# The antipsychotic registry (R/antipsychotics.R): route bases agree with the
# units the library offers, research-gated profiles stay out of the library,
# and occupancy is computed only from a paired, concentration-driven fit.

dd <- getDrugDefaultsGlobal()
profiles <- antipsychoticProfiles()

test_that("every Antipsychotics drug has a profile, and every live profile a drug", {
  inCategory <- dd$Drug[!is.na(dd$Category) & dd$Category == "Antipsychotics"]
  drugs <- stats::na.omit(vapply(profiles, `[[`, "", "drug"))
  expect_setequal(drugs, inCategory)
})

test_that("blocked profiles have no parameters in the library", {
  blocked <- names(Filter(function(p) p$modelStatus == "blocked", profiles))
  expect_setequal(blocked, c("quetiapineXR", "chlorpromazine"))
  for (b in blocked) {
    expect_true(is.na(profiles[[b]]$drug))
    expect_false(b %in% dd$Drug)
  }
})

test_that("each profile's parameter basis matches the routes it offers", {
  basisRoute <- c(apparent_oral = ROUTE_PO, apparent_im = ROUTE_IM, absolute_iv = ROUTE_IV)
  for (p in profiles) {
    if (is.na(p$drug)) next
    units <- dd$Units[dd$Drug == p$drug][[1]]
    expect_true(all(doseRoute(units) == basisRoute[[p$parameterBasis]]), info = p$drug)
    expect_identical(p$route, basisRoute[[p$parameterBasis]], info = p$drug)
    # No TCI: there is no validated target for any of these
    expect_false(any(units %in% c("Plasma target", "Effect site target")), info = p$drug)
    row <- dd[dd$Drug == p$drug, ]
    expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC, row$endCe), rep(0, 5),
                 info = p$drug)
  }
})

test_that("occupancy is the paired hyperbola, with its limits", {
  q <- antipsychoticProfile("quetiapine")$pd[[1]]
  expect_equal(q$EC50, 1369 * 383.5 / 1000)          # 525.0 ng/mL
  expect_equal(antipsychoticOccupancy("quetiapine", q$EC50), 50)
  expect_equal(antipsychoticOccupancy("quetiapine", 0), 0)
  expect_equal(antipsychoticOccupancy("quetiapine", 1e9), 100, tolerance = 1e-6)

  # Risperidone's two fits stay paired: 88 with 4.9, 100 with 8.2
  expect_equal(antipsychoticOccupancy("risperidone", 4.9, fit = 1), 44)
  expect_equal(antipsychoticOccupancy("risperidone", 8.2, fit = "Uchida 2011, Emax fixed"), 50)
  expect_error(antipsychoticOccupancy("risperidone", 1, fit = "Uchida 88/8.2"), "Unknown fit")
})

test_that("non-occupancy and missing endpoints are refused", {
  expect_error(antipsychoticOccupancy("haloperidol", 3), "PANSS")
  for (p in c("haloperidolIV", "droperidol", "droperidolIM", "chlorpromazine"))
    expect_error(antipsychoticOccupancy(p, 1), "No pharmacodynamic endpoint")
  expect_error(antipsychoticOccupancy("quetiapine", -1), "nonnegative")
  expect_error(antipsychoticProfile("clozapine"), "Unknown antipsychotic profile")
})

test_that("only aripiprazole has an effect site", {
  for (p in profiles) {
    if (is.na(p$drug)) next
    pk <- getDrugPK(p$drug, 70, 170, 35, "male", getDrugDefaults(p$drug))
    hasCe <- pk$PK$default$ke0 > 0
    expect_identical(hasCe, p$drug == "aripiprazole", info = p$drug)
  }
})
