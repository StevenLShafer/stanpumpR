# The Parkinson's disease product summary, model registry and parameter
# audit.  See data-raw/parkinsonsRegistry.R (which builds the three CSVs) and
# R/parkinsons.R.  Drafted by Claude Code, 2026-10-19.

products   <- parkinsonsProducts()
registry   <- parkinsonsRegistry()
parameters <- parkinsonsParameters()

READINESS <- c("implemented", "complete_not_implemented", "needs_full_text",
               "needs_engine", "no_suitable_model")
EVIDENCE  <- c("full_text", "abstract", "label_only", "none")

# The drugs implemented for this group, in its own category
IMPLEMENTED <- c("levodopa", "levodopaIntestinalGel", "pramipexole", "pramipexoleER",
                 "safinamide", "opicapone", "istradefylline", "amantadineER",
                 "pimavanserin")

# Every drug the registry was asked to cover: the levodopa products, the
# dopamine agonists, the MAO-B and COMT inhibitors, amantadine, istradefylline,
# the anticholinergics, and the drugs for psychosis, orthostatic hypotension
# and dementia in Parkinson's disease
REQUESTED <- c("levodopa", "carbidopa", "pramipexole", "ropinirole", "rotigotine",
               "apomorphine", "bromocriptine", "cabergoline", "piribedil", "tavapadon",
               "selegiline", "rasagiline", "safinamide", "entacapone", "tolcapone",
               "opicapone", "amantadine", "istradefylline", "trihexyphenidyl",
               "benztropine", "biperiden", "pimavanserin", "droxidopa", "rivastigmine",
               "zonisamide")

test_that("every product row is filled in, with a US label wherever one exists", {
  for (col in c("product_id", "drug", "drug_class", "product", "route", "us_status",
                "tmax", "half_life", "peak_effect", "therapeutic_range", "covariates",
                "genotype"))
    expect_true(all(nzchar(products[[col]]) & !is.na(products[[col]])), info = col)
  expect_false(any(duplicated(products$product_id)))
  urls <- stats::na.omit(c(products$label_url, products$ema_url))
  expect_true(all(grepl("^https://", urls)))
  labelled <- !is.na(products$label_url)
  expect_true(all(grepl("^https://www.accessdata.fda.gov/drugsatfda_docs/label/",
                        products$label_url[labelled])))
  # Only products not approved in the US, unread, or never given an NDA label
  # here go without one
  expect_setequal(products$product_id[!labelled],
                  c("levodopa_benserazide", "tavapadon", "trihexyphenidyl",
                    "benztropine", "biperiden", "zonisamide_pd"))
  expect_true(all(grepl("^https://www.ema.europa.eu/en/medicines/human/EPAR/",
                        stats::na.omit(products$ema_url))))
})

test_that("every registry row is fully classified with a clickable source", {
  for (col in c("model_id", "drug", "population", "analyte", "readiness",
                "evidence_read", "blocking_issue", "source", "source_url"))
    expect_true(all(nzchar(registry[[col]]) & !is.na(registry[[col]])), info = col)
  expect_false(any(duplicated(registry$model_id)))
  expect_true(all(registry$readiness %in% READINESS))
  expect_true(all(registry$evidence_read %in% EVIDENCE))
  expect_true(all(grepl("^https?://", registry$source_url)))
  expect_true(all(stats::na.omit(registry$product_id) %in% products$product_id))
})

test_that("every requested drug is catalogued, in the registry and the products", {
  expect_setequal(unique(registry$drug), REQUESTED)
  expect_true(all(setdiff(REQUESTED, c("carbidopa", "piribedil")) %in% products$drug))
})

test_that("an implemented row maps to a library drug, and no other row does", {
  dd <- getDrugDefaultsGlobal()
  done <- registry[registry$readiness == "implemented", ]
  expect_true(all(done$stanpumpr_drug %in% dd$Drug))
  for (d in done$stanpumpr_drug) expect_true(exists(d, mode = "function"), info = d)
  expect_true(all(is.na(registry$stanpumpr_drug[registry$readiness != "implemented"])))
  # and each product that names a library drug names one the registry implements
  named <- stats::na.omit(products$stanpumpr_drug)
  expect_true(all(named %in% done$stanpumpr_drug))
})

test_that("the category holds exactly the implemented drugs; levodopa alone has an effect site", {
  dd <- getDrugDefaultsGlobal()
  expect_setequal(dd$Drug[dd$Category %in% PARKINSONS_DRUG_CATEGORY], IMPLEMENTED)
  expect_true(all(IMPLEMENTED %in% registry$stanpumpr_drug))
  for (d in IMPLEMENTED) {
    row <- dd[dd$Drug == d, ]
    expect_equal(row$MEAC, 0, info = d)
    PK <- getDrugPK(d, 70, 170, 60, "male")
    if (d %in% c("levodopa", "levodopaIntestinalGel")) {
      expect_equal(PK$PK[[1]]$ke0 * 60, 1.80, tolerance = 1e-6, info = d)
      expect_equal(row$endCe, 0.81, info = d)
    } else {
      expect_equal(PK$tPeak, 0, info = d)
      expect_equal(row$endCe, 0, info = d)
    }
    # A band only where a range is published (levodopa and amantadine, AGNP)
    if (!d %in% c("levodopa", "levodopaIntestinalGel", "amantadineER")) {
      expect_equal(row$Lower, 0, info = d)
      expect_equal(row$Upper, 0, info = d)
    }
  }
  # Zonisamide was already in the library and stays an antiseizure drug
  expect_identical(dd$Category[dd$Drug == "zonisamide"], "Antiseizure")
})

test_that("no implemented model reads a pharmacogenetic field", {
  # Genotype has no quantified effect on any of these drugs' kinetics (the
  # help page says why); a model that gains one must say so here.
  for (d in IMPLEMENTED)
    for (arg in c("cyp2d6", "cyp2c19", "cyp2c9", "cyp2b6", "vkorc1"))
      expect_false(helpDrugDeclares(d, arg), info = paste(d, arg))
  # The renal models do read the creatinine
  readers <- IMPLEMENTED[vapply(IMPLEMENTED, helpDrugDeclares, logical(1), "creatinine")]
  expect_setequal(readers, c("pramipexole", "pramipexoleER", "amantadineER"))
})

test_that("rows waiting on a paper name it; rows with no model say why", {
  waiting <- registry[registry$readiness == "needs_full_text", ]
  expect_true(all(!is.na(waiting$to_obtain) & nzchar(waiting$to_obtain)))
  expect_true(all(grepl("PMID|doi|PMC|NDA", waiting$to_obtain)))
  fromAbstract <- registry[registry$readiness == "implemented" & registry$evidence_read != "full_text", ]
  expect_setequal(fromAbstract$stanpumpr_drug, "zonisamide")
  none <- registry[registry$readiness %in% c("no_suitable_model", "needs_engine",
                                             "complete_not_implemented"), ]
  expect_true(all(nchar(none$blocking_issue) > 60))
  alt <- registry[registry$readiness == "complete_not_implemented", ]
  expect_true(all(alt$evidence_read == "full_text"))
})

test_that("the parameter audit covers every implemented drug, with the models' numbers", {
  expect_setequal(unique(parameters$drug), IMPLEMENTED)
  for (col in c("drug", "parameter", "value", "units", "provenance", "evidence", "source_url"))
    expect_true(all(nzchar(parameters[[col]]) & !is.na(parameters[[col]])), info = col)
  expect_true(all(parameters$provenance %in% c("estimated", "fixed", "derived")))
  value <- function(d, p) as.numeric(parameters$value[parameters$drug == d & parameters$parameter == p])
  expect_equal(value("levodopa", "CL/F"), LEVODOPA_CL)
  expect_equal(value("levodopa", "Vc/F"), LEVODOPA_VC)
  expect_equal(value("levodopa", "Q/F"), LEVODOPA_Q)
  expect_equal(value("levodopa", "Vp/F"), LEVODOPA_VP)
  expect_equal(value("levodopa", "KTR tablets"), LEVODOPA_KTR_ORAL)
  expect_equal(value("levodopa", "ke0 UPDRS III"), LEVODOPA_KE0)
  expect_equal(value("levodopa", "EC50 UPDRS III") / 1000, LEVODOPA_UPDRS_EC50)
  expect_equal(value("levodopaIntestinalGel", "KTR gel"), LEVODOPA_KTR_GEL)
  expect_equal(value("levodopaIntestinalGel", "Frel gel"), LEVODOPA_FREL_GEL)
  expect_equal(value("pramipexole", "CL/F"), PRAMIPEXOLE_CL)
  expect_equal(value("pramipexole", "V3/F"), PRAMIPEXOLE_V3)
  expect_equal(value("pramipexole", "salt per base"), PRAMIPEXOLE_SALT_PER_BASE)
  expect_equal(value("pramipexoleER", "ka ER"), PRAMIPEXOLE_KA_ER)
  expect_equal(value("pramipexoleER", "D1 ER"), PRAMIPEXOLE_D1_ER)
  expect_equal(value("safinamide", "CL/F"), SAFINAMIDE_CL)
  expect_equal(value("safinamide", "V/F"), SAFINAMIDE_V)
  expect_equal(value("safinamide", "CL age exponent"), SAFINAMIDE_CL_AGE)
  expect_equal(value("opicapone", "CL/F"), OPICAPONE_CL)
  expect_equal(value("opicapone", "dose gamma on F1"), OPICAPONE_F_GAMMA)
  expect_equal(value("istradefylline", "CL/F"), ISTRADEFYLLINE_CL)
  expect_equal(value("istradefylline", "V3 weight exponent"), ISTRADEFYLLINE_V3_WT)
  expect_equal(value("istradefylline", "smoker on CL/F"), ISTRADEFYLLINE_SMOKER)
  expect_equal(value("amantadineER", "Vc/F"), AMANTADINE_ER_VC)
  expect_equal(value("amantadineER", "k"), AMANTADINE_ER_K)
  expect_equal(value("amantadineER", "F1"), AMANTADINE_ER_F1)
  expect_equal(value("pimavanserin", "CL/F"), PIMAVANSERIN_CL)
  expect_equal(value("pimavanserin", "V/F"), PIMAVANSERIN_V)
})

test_that("the help page's generated blocks render from the registry", {
  md <- helpParkinsonsProductsMarkdown()
  expect_true(grepl("help:drugs/levodopa)", md, fixed = TRUE))
  expect_equal(length(gregexpr("^#### ", md, perl = TRUE)[[1]]) +
                 lengths(regmatches(md, gregexpr("\n#### ", md))), length(unique(products$drug_class)))
  reg <- helpParkinsonsRegistryMarkdown()
  expect_equal(length(strsplit(reg, "\n")[[1]]), nrow(registry) + 2)
  expect_true(grepl("Needs an engine feature", reg, fixed = TRUE))
  expect_true(nzchar(helpParkinsonsToObtainMarkdown()))
})

test_that("the help page renders its generated tables (as the app calls them)", {
  # helpExpandGenerated() hands every builder the drug library first; a
  # builder that took its registry first rendered nothing.
  md <- helpExpandGenerated(helpReadMarkdown("models/parkinsons"))
  expect_false(grepl("<!-- generated:", md, fixed = TRUE))
  expect_true(grepl("Needs an engine feature", md, fixed = TRUE))
  expect_true(grepl("pramipexole dihydrochloride tablets (Mirapex)", md, fixed = TRUE))
  expect_true(grepl("Heikkinen", md, fixed = TRUE))
  # and the vasoactive page, whose builders had the same fault
  va <- helpExpandGenerated(helpReadMarkdown("models/vasoactive"))
  expect_true(grepl("| Drug | Status |", va, fixed = TRUE))
  expect_gt(lengths(regmatches(va, gregexpr("\n\\| \\[?[a-z]", va))), nrow(vasoactiveRegistry()) - 1)
})

