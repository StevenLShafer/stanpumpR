# Parents and their metabolites (AGENTS.md): a parent's formation of an active
# metabolite must agree with the library's own model of that metabolite,
# because the metabolite curve is drawn with that model and only the ratio of
# formation to metabolite clearance is identifiable.  METABOLITE_FORMATION_BASIS
# (R/metaboliteCoefficients.R) records, for every parent, how that agreement is
# kept; these tests hold the models to it.

declaredMetabolite <- function(drug) {
  x <- get(drug, mode = "function")(weight = 70, height = 170, age = 35,
                                     sex = "male")
  m <- x$metabolite$name
  if (is.null(m)) m <- x$transitChain$metabolite$name
  m
}

parentsInLibrary <- function() {
  drugs <- getDrugDefaultsGlobal(FALSE)$Drug
  drugs <- drugs[vapply(drugs, exists, logical(1), mode = "function")]
  met <- lapply(drugs, function(d) tryCatch(declaredMetabolite(d),
                                            error = function(e) NULL))
  names(met) <- drugs
  unlist(met[!vapply(met, is.null, logical(1))])
}

test_that("every parent the models declare has one row, naming its metabolite", {
  declared <- parentsInLibrary()
  basis <- METABOLITE_FORMATION_BASIS
  expect_setequal(names(declared), basis$parent)
  expect_equal(anyDuplicated(basis$parent), 0L)
  expect_equal(unname(declared[basis$parent]), basis$metabolite)
  expect_true(all(basis$basis %in% c("reads", "shared", "joint")))
})

test_that("every metabolite is a model in the library", {
  for (m in METABOLITE_FORMATION_BASIS$metabolite)
    expect_true(exists(m, mode = "function"), info = m)
})

test_that("a 'reads' or 'shared' parent really calls what it names", {
  basis <- METABOLITE_FORMATION_BASIS[METABOLITE_FORMATION_BASIS$basis != "joint", ]
  for (i in seq_len(nrow(basis))) {
    body <- paste(deparse(get(basis$parent[i], mode = "function")), collapse = "\n")
    expect_true(grepl(paste0(basis$uses[i], "("), body, fixed = TRUE),
                info = paste(basis$parent[i], "does not call", basis$uses[i]))
  }
})

test_that("libraryReferenceClearance() is the model's own CL1 at the reference man", {
  expect_equal(libraryReferenceClearance("morphine"),
               morphine(70, 170, 35, "male")$PK$default$cl1 * 60)
  expect_equal(libraryReferenceClearance("morphine"), 74.0)
  expect_error(libraryReferenceClearance("notADrug"), "No drug model")
})

test_that("a revision of the metabolite's clearance carries into a 'reads' parent", {
  # Doubling morphine's clearance must double codeine's formation, and the
  # same for hydromorphone into hydrocodone, and oxymorphone into oxycodone's
  # first pass: the parent holds no copy of the number.
  double_cl1 <- function(model) {
    force(model)   # before the mock replaces the binding it names
    function(...) {
      x <- model(...)
      x$PK$default$cl1 <- 2 * x$PK$default$cl1
      x
    }
  }
  base <- codeine(70, 170, 35, "male")$metabolite$kFormation
  local_mocked_bindings(morphine = double_cl1(morphine))
  expect_equal(codeine(70, 170, 35, "male")$metabolite$kFormation, 2 * base,
               tolerance = 1e-9)

  base <- hydrocodone(70, 170, 35, "male")$metabolite$kFormation
  local_mocked_bindings(hydromorphone = double_cl1(hydromorphone))
  expect_equal(hydrocodone(70, 170, 35, "male")$metabolite$kFormation,
               2 * base, tolerance = 1e-9)

  base <- oxycodone(70, 170, 35, "male")$metabolite$firstPassFraction
  local_mocked_bindings(oxymorphone = double_cl1(oxymorphone))
  expect_equal(oxycodone(70, 170, 35, "male")$metabolite$firstPassFraction,
               2 * base, tolerance = 1e-9)
})
