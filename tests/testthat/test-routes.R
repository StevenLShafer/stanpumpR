test_that("doseRoute() reads the route from the units suffix", {
  expect_equal(doseRoute(c("mg", "mg/kg", "mcg/kg/min", "mg/hr")), rep("IV", 4))
  expect_equal(doseRoute(c("mg PO", "mg/kg PO", "mg IM", "mg/kg IM", "mg IN")),
               c("PO", "PO", "IM", "IM", "IN"))
  expect_equal(doseRoute(c(tciUnits, gasUnits)), rep("IV", 4))
  expect_equal(doseRoute(character(0)), character(0))
  expect_equal(doseRoute(factor("mg PO")), "PO")
})

test_that("doseRoute() agrees with every unit the app offers", {
  expect_true(all(doseRoute(c(bolusUnits, infusionUnits)) == ROUTE_IV))
  expect_true(all(doseRoute(poUnits) == ROUTE_PO))
  expect_true(all(doseRoute(imUnits) == ROUTE_IM))
  expect_true(all(doseRoute(inUnits) == ROUTE_IN))

  dd <- getDrugDefaultsGlobal()
  units <- unique(unlist(dd$Units))
  # The substring tests simCpCe() used before doseRoute()
  old <- ifelse(grepl("PO", units), "PO",
                ifelse(grepl("IM", units), "IM",
                       ifelse(grepl("IN", units), "IN", "IV")))
  expect_equal(doseRoute(units), old)
})

test_that("groupUnitsByRoute() orders IV, PO, IM, IN and keeps order within a route", {
  units <- c("mg", "mg PO", "mg IN", "mg/kg", "mg IM", "Plasma target", "mg/kg PO")
  expect_equal(groupUnitsByRoute(units),
               c("mg", "mg/kg", "Plasma target", "mg PO", "mg/kg PO", "mg IM", "mg IN"))
})

test_that("the drug defaults list each drug's units grouped by route, with none lost", {
  raw <- getDrugDefaultsGlobal(expand = FALSE)
  dd <- getDrugDefaultsGlobal()
  for (i in seq_len(nrow(dd))) {
    units <- dd$Units[[i]]
    expect_setequal(units, strsplit(raw$Units[i], ",")[[1]])
    expect_false(is.unsorted(match(doseRoute(units), DOSE_ROUTES)), label = dd$Drug[i])
  }
  hydromorphone <- dd$Units[[which(dd$Drug == "hydromorphone")]]
  expect_equal(hydromorphone[doseRoute(hydromorphone) != ROUTE_IV], c("mg PO", "mg IM", "mg IN"))
  expect_true(all(c("Plasma target", "Effect site target") %in%
                    hydromorphone[doseRoute(hydromorphone) == ROUTE_IV]))
})
