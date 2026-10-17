# The antihypertensive model registry and its parameter audit.  See
# data-raw/antihypertensiveRegistry.R (which builds the two CSVs),
# R/antihypertensives.R and docs/antihypertensives.md.  Drafted by Claude
# Code, 2026-10-17.
#
# These tests are the specification's acceptance criteria, written down: that
# every drug, formulation and route has its own keyed record with a citation;
# that no catalog row can produce a blood-pressure number; that a descriptive
# label statistic is never recorded as a fitted coefficient; and that nothing
# this package executes rests on an unverified transcription.

registry <- antihypertensiveRegistry()
parameters <- antihypertensiveParameters()

EVIDENCE <- c("primary_fitted_full", "primary_fitted_partial",
              "primary_structure_only", "official_label_descriptive",
              "proposed_unfitted")
PD_EVIDENCE <- c(EVIDENCE, "secondary_transcription_unverified")
STATUS <- c("ready_for_source_reproduction", "requires_source_extraction",
            "requires_parameter_fit", "descriptive_only")
VALIDATION <- c("none", "in_population", "external_population")
ESTIMATE <- c("fitted", "fixed", "derived", "label_descriptive", "proposed")
VERIFICATION <- c("primary_full_text", "primary_abstract", "derived_here",
                  "handoff_unverified")

test_that("every record is uniquely keyed and fully classified", {
  expect_gt(nrow(registry), 100)
  expect_false(any(duplicated(registry$model_id)))
  for (col in c("model_id", "drug", "active_moiety", "route", "formulation",
                "endpoint", "pk_evidence", "pd_evidence", "execution_status",
                "external_validation_status", "source_type", "source_url",
                "limitations"))
    expect_true(all(!is.na(registry[[col]]) & nzchar(registry[[col]])),
                info = col)
  expect_true(all(registry$pk_evidence %in% EVIDENCE))
  expect_true(all(registry$pd_evidence %in% PD_EVIDENCE))
  expect_true(all(registry$execution_status %in% STATUS))
  expect_true(all(registry$external_validation_status %in% VALIDATION))
  expect_true(all(registry$route %in% c("oral", "iv", "transdermal")))
  # A clickable citation on every row
  expect_true(all(grepl("^https?://", registry$source_url)))
})

test_that("no blood pressure is simulated anywhere, and nothing is externally validated", {
  # The central gate: the package plots concentrations.  A record may only
  # turn this on when the whole dose-to-pressure chain, its baseline and its
  # measurement context are populated AND source reproduction passes, and
  # nothing qualifies.
  expect_false(any(registry$bp_simulation_enabled))
  expect_true(all(registry$external_validation_status == "none"))
})

test_that("formulation and route are separate records where the specification demands it", {
  pairs <- list(
    # propranolol immediate-release against long-acting
    c("propranolol__ir_tablet__oral__catalog__none__label",
      "propranolol__la_capsule__oral__catalog__none__label"),
    # each loop diuretic oral against intravenous
    c("furosemide__tablet__oral__catalog__none__label",
      "furosemide__injection__iv__catalog__none__label"),
    c("torsemide__tablet__oral__catalog__none__label",
      "torsemide__injection__iv__catalog__none__label"),
    c("bumetanide__tablet__oral__catalog__none__label",
      "bumetanide__injection__iv__catalog__none__label"),
    # the spironolactone tablet against the study suspension
    c("spironolactone__tablet__oral__catalog__none__label",
      "spironolactone__oral_suspension__oral__south_asian_men__concentration__tatipalli2021"),
    # metoprolol immediate-release, extended-release and intravenous
    c("metoprolol__tartrate_ir_tablet__oral__catalog__none__label",
      "metoprolol__succinate_er_tablet__oral__catalog__none__label"),
    c("metoprolol__tartrate_ir_tablet__oral__catalog__none__label",
      "metoprolol__tartrate_injection__iv__catalog__none__label"),
    # clonidine oral against the transdermal system
    c("clonidine__ir_tablet__oral__catalog__none__label",
      "clonidine__transdermal_patch__transdermal__catalog__none__label"),
    # racemic extended-release verapamil against the controlled-onset
    # product, whose record is the S-enantiomer
    c("verapamil__er_tablet__oral__catalog__none__label",
      "verapamil__coer_tablet__oral__catalog__none__label")
  )
  for (p in pairs) {
    expect_true(all(p %in% registry$model_id), info = paste(p, collapse = " / "))
  }
  # Standalone canrenone is its own record, not the metabolite formed from
  # spironolactone
  canrenone <- registry[registry$drug == "canrenone", ]
  expect_equal(nrow(canrenone), 1)
  expect_match(canrenone$limitations, "different record")
})

test_that("a catalog-only record carries the default policy and no fitted vector", {
  catalog <- registry[grepl("__catalog__none__", registry$model_id), ]
  expect_gt(nrow(catalog), 50)
  expect_true(all(catalog$execution_status == "descriptive_only"))
  expect_false(any(catalog$bp_simulation_enabled))
  expect_true(all(is.na(catalog$stanpumpr_drug)))
  expect_true(all(catalog$endpoint == "none"))
  # A label descriptor must never be recorded as a fitted compartment or
  # blood-pressure coefficient: no catalog record has any parameter at all
  expect_equal(sum(parameters$model_id %in% catalog$model_id), 0)
})

test_that("only concentration records are executable, and they are the library's drugs", {
  ready <- registry[registry$execution_status == "ready_for_source_reproduction", ]
  # Every executable record is either a concentration curve implemented as a
  # drug, or an endpoint relation evaluable against a supplied concentration
  implemented <- ready[ready$endpoint == "concentration", ]
  expect_setequal(implemented$stanpumpr_drug,
                  c("clevidipine", "fenoldopamPediatric", "carvedilol",
                    "azilsartan", "chlorthalidone"))
  dd <- getDrugDefaultsGlobal()
  for (d in implemented$stanpumpr_drug) {
    expect_true(d %in% dd$Drug, info = d)
    expect_true(exists(d, mode = "function"), info = d)
    # Implemented as an antihypertensive, with no MEAC invented for it
    expect_identical(dd$Category[dd$Drug == d], "Antihypertensives")
    expect_equal(dd$MEAC[dd$Drug == d], 0)
  }
  # Every drug in the Antihypertensives category is an implemented record:
  # the catalog never leaks a selectable drug
  expect_setequal(dd$Drug[dd$Category %in% "Antihypertensives"],
                  implemented$stanpumpr_drug)
})

test_that("a record that cannot be ported says so, and names the obstacle", {
  gated <- registry[registry$execution_status != "ready_for_source_reproduction" &
                      !grepl("__catalog__none__", registry$model_id), ]
  expect_gt(nrow(gated), 20)
  expect_true(all(is.na(gated$stanpumpr_drug) |
                    gated$stanpumpr_drug %in%
                      c("clevidipine", "fenoldopamPediatric", "carvedilol",
                    "azilsartan", "chlorthalidone")))
  # The four the handoff asked to be ported first.  Azilsartan was on that
  # list and has since been ported, from the full text supplied on
  # 2026-10-17; so has chlorthalidone, from the same paper.  What is gated
  # now is the INACTIVE metabolite record, and for a reason that is no
  # longer about access: every number in it is verified, and an inactive
  # metabolite is simply not worth a curve.
  azl <- registry[registry$model_id ==
    "azilsartan__medoxomil_tablet__oral__stage2_hypertension__concentration__tsai2016", ]
  expect_identical(azl$execution_status, "ready_for_source_reproduction")
  expect_identical(azl$stanpumpr_drug, "azilsartan")
  m2 <- registry[registry$model_id ==
    "azilsartan__m2_metabolite__oral__stage2_hypertension__concentration__tsai2016", ]
  expect_identical(m2$execution_status, "requires_source_extraction")
  expect_match(m2$limitations, "DELIBERATELY")
  # Hydrochlorothiazide is the one whose obstacle MOVED: its numbers are all
  # verified now, and what blocks it is an input shape the engine lacks.
  hctz <- registry[registry$model_id ==
    "hydrochlorothiazide__tablet__oral__pooled_adults__concentration__vanwart2013", ]
  expect_identical(hctz$execution_status, "requires_source_extraction")
  expect_match(hctz$limitations, "[Ww]eibull")
  # Carvedilol was on that list and has since been ported: the zero-order
  # oral release mode it needed is R/oral-zero-order.R, and its full text was
  # reached through PubMed Central.  Its PK record is no longer gated; only
  # its isoproterenol-challenge endpoint is, and for a different reason.
  carv <- registry[registry$model_id ==
    "carvedilol__ir_tablet__oral__healthy_korean_men__concentration__hwang2023", ]
  expect_identical(carv$execution_status, "ready_for_source_reproduction")
  carvHR <- registry[registry$model_id ==
    "carvedilol__ir_tablet__oral__healthy_korean_men__iso_hr__hwang2023", ]
  expect_match(carvHR$limitations, "isoproterenol")
  nic <- registry[registry$model_id ==
    "nicardipine__injection__iv__cardiac_surgery_adults__concentration__page2007", ]
  expect_match(nic$limitations, "gated")
  snp <- registry[registry$model_id ==
    "nitroprusside__injection__iv__anaesthetised_children__map__barrett2015", ]
  expect_match(snp$limitations, "NO measured drug")
  expect_identical(snp$matrix, "nominal (kinetic-dynamic)")
})

test_that("every parameter belongs to a record and carries its provenance", {
  expect_true(all(parameters$model_id %in% registry$model_id))
  for (col in c("model_id", "parameter", "unit", "estimate_status",
                "reported_or_derived", "uncertainty_type", "source_url",
                "verification"))
    expect_true(all(!is.na(parameters[[col]]) & nzchar(parameters[[col]])),
                info = col)
  expect_true(all(parameters$estimate_status %in% ESTIMATE))
  expect_true(all(parameters$reported_or_derived %in% c("reported", "derived")))
  expect_true(all(parameters$verification %in% VERIFICATION))
  expect_true(all(grepl("^https?://", parameters$source_url)))
  # A derived number is labelled derived on both axes
  derived <- parameters[parameters$reported_or_derived == "derived", ]
  expect_true(all(derived$estimate_status == "derived"))
  expect_true(all(derived$verification == "derived_here"))
  # Every runnable record's parameters carry matrix, population and endpoint
  runnable <- registry$model_id[registry$execution_status ==
                                  "ready_for_source_reproduction"]
  run <- parameters[parameters$model_id %in% runnable, ]
  expect_gt(nrow(run), 10)
  for (col in c("matrix", "population", "endpoint"))
    expect_true(all(!is.na(run[[col]])), info = col)
})

test_that("nothing this package executes rests on an unverified transcription", {
  # This container cannot reach the publishers, so most of the handoff's
  # numbers could not be checked.  They are catalog data; they must never be
  # an input to anything the package computes.
  runnable <- registry$model_id[registry$execution_status ==
                                  "ready_for_source_reproduction"]
  unverified <- parameters[parameters$verification == "handoff_unverified", ]
  expect_gt(nrow(unverified), 20)
  expect_false(any(unverified$model_id %in% runnable))
  # And an unfilled parameter stays empty rather than being guessed, with a
  # note saying what is missing and why it matters
  missing <- parameters[is.na(parameters$value), ]
  expect_gt(nrow(missing), 3)
  expect_true(all(!is.na(missing$notes) & nzchar(missing$notes)))
  expect_true(all(missing$estimate_status %in% ESTIMATE))
})

test_that("a number printed in its own abstract is verified against it", {
  # Three of the handoff's numbers turned out to be in their abstracts, which
  # PubMed serves even where the publisher's host is blocked, and they matched
  # exactly.  They are evidence now, not pending transcription, and a rebuild
  # must not quietly send them back to handoff_unverified.
  abstractVerified <- list(
    list(id = "hydrochlorothiazide__tablet__oral__pooled_adults__concentration__vanwart2013",
         parameter = "non-renal CL", value = 2.44),
    list(id = "hydrochlorothiazide__tablet__oral__pooled_adults__concentration__vanwart2013",
         # Table 2's own name: it is the coefficient of a power function of
         # creatinine clearance, 18.3 x (CL_CR/120)^1.38, not a clearance.
         parameter = "renal CL coefficient", value = 18.3),
    list(id = "furosemide__tablet_and_injection__oral__pooled_adults__concentration_and_urine__popPK",
         parameter = "F", value = 0.591))
  for (x in abstractVerified) {
    row <- parameters[parameters$model_id == x$id &
                        parameters$parameter == x$parameter, ]
    expect_equal(nrow(row), 1, info = x$parameter)
    expect_equal(row$value, x$value, info = x$parameter)
    # These began as abstract verifications.  Van Wart's full text arrived on
    # 2026-10-17 and upgraded them, which is a strengthening: the guard here
    # is against a rebuild sending them BACK to handoff_unverified, so any
    # primary reading passes and an unverified one does not.
    expect_true(row$verification %in% c("primary_abstract", "primary_full_text"),
                info = x$parameter)
  }
  # Hydrochlorothiazide's two half-lives came from the abstract as well, and
  # they are the reason section 4 can say that record is short by exactly one
  # volume: with the clearances they give both eigenvalues.
  hctz <- parameters[grepl("vanwart2013", parameters$model_id) &
                       grepl("^half-life", parameters$parameter), ]
  expect_equal(nrow(hctz), 2)
  expect_equal(sort(hctz$value), c(1.6, 14.8))
  expect_true(all(hctz$verification %in%
                    c("primary_abstract", "primary_full_text")))
  # And the volume they left the record short of is now in hand, so the
  # two-compartment system closes: section 4's row says determined.
  v1 <- parameters[grepl("vanwart2013", parameters$model_id) &
                     parameters$parameter == "V1", ]
  expect_equal(nrow(v1), 1)
  expect_equal(v1$value, 97.3)
})

test_that("an uncertainty keeps the type its source names, or says it was not reported", {
  # Hammer's are approximate standard errors, by the paper's own words
  hammer <- parameters[grepl("hammer2008", parameters$model_id) &
                         !is.na(parameters$uncertainty), ]
  expect_gt(nrow(hammer), 3)
  expect_true(all(hammer$uncertainty_type == "approximate standard error"))
  # Campanero's and Donnelly's are printed without a type, and must not be
  # upgraded to SD or SE by assumption
  untyped <- parameters[grepl("campanero2008|donnelly1993", parameters$model_id) &
                          !is.na(parameters$uncertainty), ]
  expect_gt(nrow(untyped), 4)
  expect_true(all(untyped$uncertainty_type == "not_reported"))
  # Where a type IS named it is spelled out, never abbreviated into ambiguity
  named <- setdiff(unique(parameters$uncertainty_type), "not_reported")
  expect_true(all(nchar(named) > 10))
})

test_that("the endpoint relations are complete, verified, and signed as falls", {
  available <- antihypertensiveEndpointsAvailable()
  expect_equal(nrow(available), 4)
  expect_true(all(available$model_id %in% registry$model_id))
  expect_false(any(available$bp_simulation_enabled))
  for (id in available$model_id) {
    expect_identical(
      registry$execution_status[registry$model_id == id],
      "ready_for_source_reproduction", info = id)
    # Every coefficient of an evaluable relation was verified primarily
    p <- parameters[parameters$model_id == id & !is.na(parameters$value), ]
    expect_true(nrow(p) > 0, info = id)
    expect_true(all(p$verification %in%
                      c("primary_full_text", "primary_abstract")), info = id)
    # A positive concentration always predicts a fall, never a rise: the
    # canonical sign convention for a negative published Emax
    expect_lt(antihypertensiveEndpoint(id, 1)$predicted_change, 0)
  }
})

test_that("an Emax relation halves at its EC50 and saturates at its maximum", {
  emax <- Filter(function(e) e$structure == "emax",
                 stanpumpR:::ANTIHYPERTENSIVE_ENDPOINTS)
  expect_equal(length(emax), 3)
  for (e in emax) {
    at50 <- antihypertensiveEndpoint(e$model_id, e$ec50)$predicted_change
    expect_equal_rounded(-e$max_drop / 2, at50)
    big <- antihypertensiveEndpoint(e$model_id, e$ec50 * 1e6)$predicted_change
    expect_gt(big, -e$max_drop)
    expect_lt(big, -e$max_drop * 0.999)
  }
})

test_that("a linear relation warns that it has no ceiling", {
  amlo <- antihypertensiveEndpoint(
    "amlodipine__tablet__oral__hypertensive_adults__erect_sbp__donnelly1993",
    c(1, 10))
  expect_equal_rounded(c(-3.1, -31), amlo$predicted_change)
  expect_true(all(grepl("linear and has no maximum", amlo$warnings)))
  expect_true(all(grepl("outside a verified range", amlo$warnings)))
  expect_identical(unique(amlo$endpoint), "erect_sbp")
})

test_that("a record with no evaluable relation is refused with its status", {
  expect_error(
    antihypertensiveEndpoint("captopril__ir_tablet__oral__catalog__none__label", 10),
    "descriptive_only")
  expect_error(antihypertensiveEndpoint("not_a_model", 10), "Unknown model_id")
  expect_error(
    antihypertensiveEndpoint(
      "terazosin__ir_tablet__oral__healthy_volunteers__sbp__campanero2008", -1),
    "non-negative")
})

test_that("an endpoint that is not a blood pressure is never described as one", {
  notBP <- registry[registry$endpoint %in%
    c("exercise_sbp", "exercise_hr", "isoproterenol_induced_hr",
      "svr_proxy_map_over_hr", "urinary_sodium",
      "exercise_hr_and_sbp", "map_threshold_probability",
      "pulse_pressure_and_resistance_turnover", "dose_rate_sbp"), ]
  expect_gt(nrow(notBP), 8)
  # None of them is evaluable, so none can be mistaken for a pressure
  expect_false(any(notBP$model_id %in% antihypertensiveEndpointsAvailable()$model_id))
  expect_false(any(notBP$bp_simulation_enabled))
})
