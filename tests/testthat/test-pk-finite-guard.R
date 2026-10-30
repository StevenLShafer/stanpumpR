# getDrugPK() refuses a disposition the engines cannot use: a volume that is
# not finite and positive, or a clearance that is not finite and non-negative
# (checkDispositionFinite(), R/getDrugPK.R).  stanpumpR cannot anticipate every
# patient it is given, so no covariate may reach the rate constants as zero,
# infinity or NaN.

good <- list(v1 = 10, v2 = 20, v3 = 1, cl1 = 1, cl2 = 2, cl3 = 0)

test_that("a usable disposition passes, missing compartments included", {
  expect_null(checkDispositionFinite("x", PK_EVENT_DEFAULT, good))
})

test_that("zero, negative, infinite and NaN volumes stop", {
  for (bad in list(0, -1, Inf, NaN, NA_real_))
    expect_error(checkDispositionFinite("x", PK_EVENT_DEFAULT, modifyList(good, list(v2 = bad))),
                 "v2 = .*volume must be finite and positive")
})

test_that("negative, infinite and NaN clearances stop; zero does not", {
  expect_null(checkDispositionFinite("x", PK_EVENT_DEFAULT, modifyList(good, list(cl2 = 0))))
  for (bad in list(-0.1, Inf, NaN, NA_real_))
    expect_error(checkDispositionFinite("x", PK_EVENT_DEFAULT, modifyList(good, list(cl1 = bad))),
                 "cl1 = .*clearance must be finite and not negative")
})

test_that("the error names the drug, and the event when it is not the default", {
  expect_error(checkDispositionFinite("mannitol", "CPB", modifyList(good, list(v1 = 0))),
               "for mannitol (CPB): v1 = 0", fixed = TRUE)
})

test_that("getDrugPK() itself applies the check to a model's output", {
  # Through the caller, so that removing the call from getDrugPK() fails here
  model <- fentanyl(70, 170, 40, "male")
  expect_no_error(getDrugPK("fentanyl", 70, 170, 40, "male", model = model))
  broken <- model
  broken$PK$default$v2 <- 0
  expect_error(getDrugPK("fentanyl", 70, 170, 40, "male", model = broken),
               "for fentanyl: v2 = 0 (a volume must be finite and positive)", fixed = TRUE)
  broken <- model
  broken$PK$default$cl1 <- NaN
  expect_error(getDrugPK("fentanyl", 70, 170, 40, "male", model = broken),
               "for fentanyl: cl1 = NaN", fixed = TRUE)
})

test_that("every drug gives a usable disposition at the extremes of a legal patient", {
  # Newborns, a 0.5 kg infant, a 90-year-old, 250 kg, a light old woman; both
  # sexes; both switch positions.  Vancomycin's Thomson clearance is
  # explicitly undefined at a creatinine clearance near zero and says so.
  dd <- getDrugDefaultsGlobal(FALSE)
  drugs <- dd$Drug[vapply(dd$Drug, function(d) exists(d, mode = "function"), logical(1))]
  patients <- list(c(3.5, 50, 0), c(0.5, 30, 0), c(70, 170, 90), c(250, 190, 40), c(40, 140, 90))
  for (d in drugs) for (p in patients) for (s in c("male", "female")) for (ffm in c(TRUE, FALSE)) {
    r <- tryCatch({ getDrugPK(d, p[1], p[2], p[3], s, getDrugDefaults(d), adjustToFFM = ffm); "ok" },
                  error = function(e) conditionMessage(e))
    if (d == "vancomycin" && grepl("not defined at a creatinine clearance", r)) next
    expect_identical(r, "ok", label = sprintf("%s at %g kg, %g cm, %g y, %s, ffm %s",
                                              d, p[1], p[2], p[3], s, ffm))
  }
})
