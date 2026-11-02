# The deployment workflows' security properties (2026-11-02; AGENTS.md,
# Deploy, "Preview security").  .github is not in the built package, so these
# run only from a source checkout (devtools::test()), never under R CMD check.

workflowDir <- function() {
  dir <- testthat::test_path("..", "..", ".github", "workflows")
  if (!dir.exists(dir)) testthat::skip("no .github/workflows (not a source checkout)")
  dir
}

readWorkflow <- function(name) readLines(file.path(workflowDir(), name), warn = FALSE)

test_that("every third-party Action is pinned to a full commit SHA", {
  for (f in list.files(workflowDir(), pattern = "\\.ya?ml$")) {
    uses <- grep("^\\s*-?\\s*uses:", readWorkflow(f), value = TRUE)
    refs <- sub(".*uses:\\s*([^ #]+).*", "\\1", uses)
    expect_true(all(grepl("@[0-9a-f]{40}$", refs)),
                info = paste(f, paste(refs[!grepl("@[0-9a-f]{40}$", refs)], collapse = ", ")))
  }
})

test_that("the PR preview holds nothing a PR should not have", {
  deploy <- readWorkflow("shiny-pr-2-deploy.yaml")
  code <- deploy[!grepl("^\\s*#", deploy)]
  # no mail credential reaches the job that runs the PR's code
  expect_false(any(grepl("EMAIL", code)))
  # never a fork
  expect_true(any(grepl("github.event.pull_request.head.repo.full_name == github.repository",
                        code, fixed = TRUE)))
  # the preview's own shinyapps.io token
  expect_true(any(grepl("name: preview", code, fixed = TRUE)))
  # no manual run (which has no PR to deploy and no fork check)
  expect_false(any(grepl("workflow_dispatch", code, fixed = TRUE)))
  # the job that runs the PR's code cannot write to the repository or the PR
  jobStart <- grep("^  deployment-pr:", code)
  commentStart <- grep("^  comment-pr:", code)
  expect_length(jobStart, 1)
  expect_length(commentStart, 1)
  deployJob <- code[jobStart:(commentStart - 1)]
  expect_false(any(grepl("^\\s+[a-z-]+:\\s*write\\s*$", deployJob)))
  # the address is passed to the script as data, never interpolated into it
  commentJob <- code[commentStart:length(code)]
  script <- commentJob[seq(grep("script: \\|", commentJob), length(commentJob))]
  expect_false(any(grepl("${{", script, fixed = TRUE)))
})

test_that("the preview cleanup uses the preview's token", {
  cleanup <- readWorkflow("shiny-pr-3-cleanup.yaml")
  expect_true(any(grepl("name: preview", cleanup, fixed = TRUE)))
  expect_false(any(grepl("EMAIL", cleanup)))
})

test_that("local environment files are never committed or built", {
  root <- testthat::test_path("..", "..")
  if (!file.exists(file.path(root, ".gitignore"))) testthat::skip("not a source checkout")
  expect_true(".Renviron" %in% trimws(readLines(file.path(root, ".gitignore"), warn = FALSE)))
  expect_true("^\\.Renviron$" %in% readLines(file.path(root, ".Rbuildignore"), warn = FALSE))
})
