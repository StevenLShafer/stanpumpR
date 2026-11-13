# The index of FDA prescribing information, inst/extdata/fdaLabels.csv
# (R/fda-labels.R, papers/labels/README.md): every drug a user can dose has a
# row saying where its label is or why there is none, the rows are well
# formed, each drug's help page links to them, and, in a checkout that has
# papers/labels/ (it is not in the built package), the files are there.

labels <- fdaLabels()
dd <- getDrugDefaultsGlobal()
doseable <- dd$Drug[!is.na(dd$Category) & nzchar(dd$Category)]

test_that("every drug a user can dose has a row in the label index", {
  expect_setequal(unique(labels$drug), doseable)
  expect_true(all(labels$status %in% FDA_LABEL_STATUSES))
})

test_that("each row is complete for its status", {
  has <- labels$status == "label"
  expect_true(all(nzchar(labels$file[has])))
  expect_true(all(grepl("\\.pdf$", labels$file[has])))
  expect_true(all(nzchar(labels$title[has])))
  expect_true(all(labels$source[has] %in% c("archive", "dailymed")))
  expect_true(all(grepl("^[0-9a-f]{32}$", labels$md5[has])))
  dm <- has & labels$source == "dailymed"
  expect_true(all(grepl("^[0-9a-f-]{36}$", labels$setid[dm])))
  expect_true(all(grepl("^\\d{4}-\\d{2}-\\d{2}$", labels$published[dm])))
  ar <- has & labels$source == "archive"
  expect_true(all(nzchar(labels$card[ar])))
  expect_true(all(grepl("^$|^\\d{4}-\\d{2}(-\\d{2})?$", labels$published[has])))
  expect_true(all(grepl("^$|^(NDA|ANDA|BLA) \\d{6}$", labels$application)))
  # a drug without a label says why, and a drug with one has nothing to explain
  expect_true(all(nzchar(labels$note[!has])))
  expect_true(all(!nzchar(labels$file[!has])))
  expect_false(any(labels$drug[!has] %in% labels$drug[has]))
  expect_false(any(duplicated(labels[, c("drug", "file")])))
  # one file, one checksum
  byFile <- unique(labels[has, c("file", "md5")])
  expect_false(any(duplicated(byFile$file)))
})

test_that("the files and cards are in the repository", {
  root <- testthat::test_path("..", "..")
  dir <- file.path(root, "papers", "labels")
  skip_if_not(dir.exists(dir), "papers/labels/ is not in the built package")
  rows <- unique(labels[labels$status == "label", c("file", "md5", "card")])
  path <- file.path(dir, rows$file)
  expect_true(all(file.exists(path)), info = paste(rows$file[!file.exists(path)], collapse = ", "))
  expect_equal(unname(tools::md5sum(path)), rows$md5)
  expect_setequal(list.files(dir, pattern = "\\.pdf$"), rows$file)
  cards <- rows$card[nzchar(rows$card)]
  expect_true(all(file.exists(file.path(root, "papers", "digests", paste0(cards, ".md")))))
})

test_that("each drug's help page lists its labels or says why it has none", {
  html <- helpDrugFdaLabelHTML("propofol")
  expect_match(html, "FDA prescribing information", fixed = TRUE)
  file <- labels$file[labels$drug == "propofol" & labels$status == "label"][1]
  expect_match(html, paste0(FDA_LABELS_URL, file), fixed = TRUE)
  # a label read by a digest card links to the card
  arch <- labels[labels$drug == "ropivacaine" & labels$source == "archive", ][1, ]
  expect_match(helpDrugFdaLabelHTML("ropivacaine"), paste0(DIGESTS_URL, arch$card, ".md"), fixed = TRUE)
  # no US label: the note, and no link to a file
  none <- helpDrugFdaLabelHTML("fluindione")
  expect_match(none, "not approved in the United States", fixed = TRUE)
  expect_no_match(none, FDA_LABELS_URL, fixed = TRUE)
  expect_identical(helpDrugFdaLabelHTML("notADrug"), "")
  # the generated page carries the section, for a drug and for a gas
  expect_match(helpDrugPageHTML("propofol"), "FDA prescribing information", fixed = TRUE)
  expect_match(helpDrugPageHTML("sevoflurane"), "FDA prescribing information", fixed = TRUE)
})
