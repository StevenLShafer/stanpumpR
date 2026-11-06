# The chemotherapy source bibliography, model registry and source relations.
# See data-raw/chemotherapyRegistry.R (which builds the three CSVs),
# R/chemotherapy.R and docs/chemotherapy.md.  Drafted by Claude Code,
# 2026-11-05.

sources   <- chemotherapySources()
registry  <- chemotherapyRegistry()
relations <- chemotherapyRelations()

READINESS <- c("implemented", "complete_not_implemented", "needs_full_text",
               "needs_engine", "no_source_identified")
REVIEW    <- c("record_checked", "abstract_screened", "full_text_partial",
               "full_text_read", "parameters_verified")
ENGINE    <- c("linear", "linear_metabolite", "michaelis_menten", "new_engine",
               "unknown")
ROLES     <- c("structural_pk", "covariate", "external_validation",
               "interaction_context", "clinical_outcome_trial",
               "binding_exposure_toxicity", "dosing_formula", "dosing_guideline",
               "target_guideline", "management_guideline",
               "pharmacogenetic_guideline", "product_label", "regulatory_method",
               "regulatory_safety", "mechanistic_context")

# The ten drugs the handoff was asked for, busulfan, and its expansion list
REQUESTED <- c("carboplatin", "cisplatin", "cyclophosphamide", "fluorouracil",
               "methotrexate", "doxorubicin", "paclitaxel", "vincristine",
               "irinotecan", "etoposide", "busulfan")
EXPANSION <- c("docetaxel", "oxaliplatin", "capecitabine", "gemcitabine",
               "cytarabine", "pemetrexed", "ifosfamide", "melphalan",
               "fludarabine", "thiotepa", "topotecan", "temozolomide",
               "mercaptopurine", "thioguanine", "asparaginase", "daunorubicin",
               "epirubicin", "idarubicin", "bleomycin", "dacarbazine",
               "bendamustine", "vinblastine", "vinorelbine", "cladribine")

test_that("every source is one document with a unique id and a clickable address", {
  expect_false(any(duplicated(sources$source_id)))
  expect_true(all(sources$kind %in% c("primary_study", "guideline", "regulatory", "label")))
  expect_true(all(sources$review_depth %in% REVIEW))
  expect_true(all(grepl("^https://", sources$url)))
  for (col in c("source_id", "kind", "citation", "url", "review_depth"))
    expect_true(all(nzchar(sources[[col]]) & !is.na(sources[[col]])), info = col)
  # one record per publication
  pmid <- stats::na.omit(sources$pmid)
  expect_false(any(duplicated(pmid)))
  expect_false(any(duplicated(stats::na.omit(sources$doi))))
  expect_true(all(grepl("^[0-9]+$", pmid)))
  expect_true(all(grepl("^PMC[0-9]+$", stats::na.omit(sources$pmcid))))
  expect_true(all(grepl("^10\\.", stats::na.omit(sources$doi))))
  # a PubMed record's address is its PMID
  hasPmid <- !is.na(sources$pmid)
  expect_identical(sources$url[hasPmid],
                   paste0("https://pubmed.ncbi.nlm.nih.gov/", sources$pmid[hasPmid], "/"))
})

test_that("the handoff's corrections were applied, and the wrong ones were not", {
  author <- function(id) sources$first_author[sources$source_id == id]
  # its own corrections, confirmed against PubMed
  expect_identical(author("C07"), "de Jonge")
  expect_identical(author("C26"), "Hsu")
  expect_identical(author("P15"), "Kontny")
  expect_identical(author("P20"), "Oyaga-Iriarte")
  expect_identical(sources$year[sources$source_id == "P19"], "2002")
  # four first authors the handoff itself had wrong (docs/chemotherapy.md)
  expect_identical(author("C04"), "Duong")         # not Nath
  expect_identical(author("C22"), "Krogh-Madsen")  # not Bogason
  expect_identical(author("C30"), "Urien")         # not You
  expect_identical(author("C42"), "Hall")          # not Crooke
  # the bundled pairs are split
  expect_true(all(c("C37a", "C37b", "C38a", "C38b") %in% sources$source_id))
  expect_false(any(c("C37", "C38") %in% sources$source_id))
})

test_that("every registry row is classified, with what blocks it", {
  for (col in c("model_id", "drug", "product", "route", "population", "analyte",
                "structure", "wave", "readiness", "engine", "evidence_read",
                "blocking_issue"))
    expect_true(all(nzchar(registry[[col]]) & !is.na(registry[[col]])), info = col)
  expect_false(any(duplicated(registry$model_id)))
  expect_true(all(registry$readiness %in% READINESS))
  expect_true(all(registry$engine %in% ENGINE))
  expect_true(all(registry$wave %in% c("first", "second", "expansion")))
  expect_true(all(registry$evidence_read %in% c(REVIEW, "none")))
  # a candidate names its source and says what to obtain; a gap names neither
  candidate <- registry$readiness != "no_source_identified"
  expect_true(all(registry$source_id[candidate] %in% sources$source_id))
  expect_true(all(nzchar(registry$to_obtain[registry$readiness == "needs_full_text"])))
  expect_true(all(is.na(registry$source_id[!candidate])))
  # a row's evidence is its source's review depth
  m <- match(registry$source_id[candidate], sources$source_id)
  expect_identical(registry$evidence_read[candidate], sources$review_depth[m])
})

test_that("every drug asked for is catalogued, in its wave", {
  expect_setequal(unique(registry$drug), c(REQUESTED, EXPANSION))
  first <- unique(registry$drug[registry$wave == "first"])
  expect_setequal(first, c("carboplatin", "methotrexate", "fluorouracil", "busulfan"))
  # the formulation products are separate rows from the conventional drug
  products <- registry$product
  expect_true(any(grepl("nab-paclitaxel", products)))
  expect_true(any(grepl("pegylated liposomal doxorubicin", products)))
  expect_true(any(grepl("liposomal irinotecan", products)))
  for (p in c("pegaspargase", "calaspargase pegol", "native E. coli asparaginase"))
    expect_true(p %in% products, info = p)
})

test_that("relations are well formed, and a P prefix is not a structural model", {
  expect_true(all(relations$role %in% ROLES))
  expect_true(all(relations$source_id %in% sources$source_id))
  expect_true(all(relations$drug %in% c(registry$drug, "global")))
  expect_true(all(stats::na.omit(relations$model_id) %in% registry$model_id))
  structural <- relations$source_id[relations$role == "structural_pk"]
  # the Calvert formula, the 5-FU outcome trial and the etoposide binding study
  expect_false(any(c("P01", "P11", "P22") %in% structural))
  role <- function(id) relations$role[relations$source_id == id]
  expect_identical(role("P01"), "dosing_formula")
  expect_identical(role("P11"), "clinical_outcome_trial")
  expect_identical(role("P22"), "binding_exposure_toxicity")
  # thiotepa: de Jonge 2004 is its direct model; P06 is context; P07 is nothing
  thiotepa <- relations[relations$drug == "thiotepa", ]
  expect_true("C07" %in% thiotepa$source_id[thiotepa$role == "structural_pk"])
  expect_identical(thiotepa$role[thiotepa$source_id == "P06"], "interaction_context")
  expect_false("P07" %in% thiotepa$source_id)
  # G14 belongs to carboplatin; the cross-cutting guidance is global
  expect_true("carboplatin" %in% relations$drug[relations$source_id == "G14"])
  for (id in c("G07", "R01", "R02", "R03"))
    expect_identical(relations$drug[relations$source_id == id], "global", info = id)
  # every source is used somewhere
  expect_setequal(unique(relations$source_id), sources$source_id)
})

test_that("an implemented row names a library drug, and no other row does", {
  dd <- getDrugDefaultsGlobal()
  done <- registry[registry$readiness == "implemented", ]
  expect_true(all(done$stanpumpr_drug %in% dd$Drug))
  expect_true(all(is.na(registry$stanpumpr_drug[registry$readiness != "implemented"])))
  # an implemented model was read in full
  expect_true(all(done$evidence_read %in% c("full_text_read", "parameters_verified")))
})

test_that("the help page's generated blocks render from the registry", {
  md <- helpChemotherapyRegistryMarkdown()
  expect_match(md, "| Drug | Product |", fixed = TRUE)
  expect_equal(length(strsplit(md, "\n")[[1]]), nrow(registry) + 2)
  want <- helpChemotherapyToObtainMarkdown()
  expect_match(want, "McCune", fixed = TRUE)
  html <- helpPageHTML("models/chemotherapy")
  expect_match(html, "MTXPK.org", fixed = TRUE)  # a paper still to obtain
  expect_false(grepl("<!-- generated", html, fixed = TRUE))
})
