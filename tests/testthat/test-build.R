# The build the app names in its debug log and the Source link's tooltip
# (stanpumpRBuild(), R/app_globals.R).  (Claude Code, 2026-10-14.)

test_that("the commit comes from the deploy's config first, then the DESCRIPTION, else is not recorded", {
  desc <- list(Version = "1.2.3", RemoteSha = "0c2bf5439c1b46a49e877e268a0f5fab1db6a75d")
  b <- stanpumpRBuild(desc, config = list(commit = "4da8bf7abcdef0123456789"))
  expect_equal(b$version, "1.2.3")
  expect_equal(b$commit, "4da8bf7abcdef0123456789")
  expect_equal(b$label, "stanpumpR 1.2.3, commit 4da8bf7")

  b <- stanpumpRBuild(desc, config = list(debug = 0))
  expect_equal(b$commit, "0c2bf5439c1b46a49e877e268a0f5fab1db6a75d")
  expect_equal(b$label, "stanpumpR 1.2.3, commit 0c2bf54")

  b <- stanpumpRBuild(list(Version = "1.2.3"), config = NULL)
  expect_true(is.na(b$commit))
  expect_equal(b$label, "stanpumpR 1.2.3, commit not recorded")
  # An empty commit setting is no commit
  expect_true(is.na(stanpumpRBuild(list(Version = "1.2.3"), config = list(commit = ""))$commit))
})

test_that("the installed package has a version and the label is one line", {
  b <- stanpumpRBuild()
  expect_false(is.na(b$version))
  expect_match(b$label, "^stanpumpR \\S+, commit ")
})
