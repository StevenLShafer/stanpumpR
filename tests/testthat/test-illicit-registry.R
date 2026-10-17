# The illicit-drug route and evidence registry,
# inst/extdata/illicitRouteRegistry.csv.  One row per drug and route, saying
# what the human evidence for that route is and whether the library offers it.
#
# The registry exists because the same drug's routes rest on very different
# evidence: a matched population fit for one, a bioavailability with no
# absorption model for another, a case report for a third.  The rule it
# enforces is that stanpumpR offers dose units ONLY for a route whose input
# model is identified, and records, next to the route it refuses, why.
#
# Drafted by Claude Code, 2026-10-17, at the request of Steven L. Shafer.

registry <- utils::read.csv(
  system.file("extdata", "illicitRouteRegistry.csv", package = "stanpumpR"),
  stringsAsFactors = FALSE)
drugDefaults <- getDrugDefaultsGlobal(FALSE)

ROUTE_STATUS <- c("calibrated", "anchored", "observed", "unsupported")
PARAMETER_STATUS <- c("published_fit", "measured_summary",
                      "derived_approximation", "not_identified")
# A route may be offered only on one of these: a route whose input model is
# merely observed or unsupported has nothing to simulate with.
OFFERABLE_ROUTE_STATUS <- c("calibrated", "anchored")
OFFERABLE_PARAMETER_STATUS <- c("published_fit", "measured_summary",
                                "derived_approximation")

test_that("every row is fully classified, with a source", {
  for (col in names(registry))
  {
    # nzchar(NA) is TRUE, so a missing field would pass the test below:
    # read.csv turns an empty field into NA, and that is exactly the
    # incompleteness this test exists to catch.
    expect_false(anyNA(registry[[col]]), info = col)
    expect_true(all(nzchar(registry[[col]])), info = col)
  }
  expect_true(all(registry$route_status %in% ROUTE_STATUS))
  expect_true(all(registry$parameter_status %in% PARAMETER_STATUS))
  expect_true(all(registry$offered %in% c("yes", "no")))
  expect_true(all(grepl("^https://", registry$source_url)))
  # One row per drug, ANALYTE, route and formulation: a single dose can have
  # more than one analyte worth recording (MDMA records both the parent and
  # the MDA metabolite it was fitted with on the same oral dose).
  expect_false(any(duplicated(
    registry[, c("drug", "analyte", "route", "formulation")])))
})

test_that("the registry covers exactly the library's illicit drugs", {
  expect_setequal(unique(registry$drug), illicitDrugNames(drugDefaults))
})

test_that("every offered route has units in the library, and every unit a row", {
  for (drug in unique(registry$drug)) {
    rows <- registry[registry$drug == drug, ]
    units <- strsplit(drugDefaults$Units[drugDefaults$Drug == drug], ",")[[1]]
    offered <- sort(unique(rows$route[rows$offered == "yes"]))
    expect_setequal(offered, sort(unique(doseRoute(units))))
    # An offered route must be a route the engine knows
    expect_true(all(offered %in% DOSE_ROUTES), info = drug)
  }
})

test_that("a route is offered only where its input model is identified", {
  offered <- registry[registry$offered == "yes", ]
  expect_true(all(offered$route_status %in% OFFERABLE_ROUTE_STATUS))
  expect_true(all(offered$parameter_status %in% OFFERABLE_PARAMETER_STATUS))
  # ... and every offered row names the dose basis its parameters were fitted
  # on, which is what makes a salt-versus-base error visible
  expect_false(any(offered$parameter_dose_basis == "not applicable"))
  expect_false(any(offered$input_dose_basis == "not applicable"))
})

test_that("no route the engine cannot represent is offered", {
  # Pulmonary (smoked, vaporised) delivery has no route token in stanpumpR.
  # Aliasing it to the intranasal route would silently substitute one
  # absorption model, bioavailability and dose denominator for another.
  smoked <- registry[registry$route == "smoked", ]
  expect_gt(nrow(smoked), 0)
  expect_true(all(smoked$offered == "no"))
  expect_false("smoked" %in% DOSE_ROUTES)
})

test_that("every drug the registry covers offers at least one route", {
  for (drug in unique(registry$drug))
    expect_true(any(registry$offered[registry$drug == drug] == "yes"), info = drug)
})

test_that("the apparent-parameter drugs refuse the intravenous route", {
  # LSD and psilocybin are fitted as clearance and volume over an unmeasured
  # bioavailability, which predicts intravenous concentrations wrong by 1/F.
  for (drug in c("LSD", "psilocybin")) {
    iv <- registry[registry$drug == drug & registry$route == "IV", ]
    expect_equal(nrow(iv), 1, info = drug)
    expect_equal(iv$offered, "no", info = drug)
    pk <- getDrugPK(drug, 70, 171, 50, "male")$PK$default
    expect_equal(pk$bioavailability_PO, 1, info = drug)
    units <- strsplit(drugDefaults$Units[drugDefaults$Drug == drug], ",")[[1]]
    expect_false(any(doseRoute(units) == ROUTE_IV), info = drug)
  }
})

test_that("every illicit drug is plasma only, with no effect site and no band", {
  for (drug in illicitDrugNames(drugDefaults)) {
    row <- drugDefaults[drugDefaults$Drug == drug, ]
    expect_equal(getDrugPK(drug, 70, 171, 50, "male")$PK$default$ke0, 0, info = drug)
    expect_equal(row$MEAC, 0, info = drug)
    expect_equal(row$endCe, 0, info = drug)
    expect_equal(c(row$Lower, row$Upper, row$Typical), c(0, 0, 0), info = drug)
  }
})
