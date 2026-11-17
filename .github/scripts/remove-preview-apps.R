# Remove PR preview apps from shinyapps.io: terminate each one still running or
# sleeping, then purge it, so that it no longer appears in the account's list.
#
# Called by shiny-pr-3-cleanup.yaml (one PR, when it closes) and by
# shiny-pr-4-sweep.yaml (every preview whose PR is no longer open).  Both run
# this file from the default branch, never from a PR's checkout.
#
# Environment:
#   SHINY_ACCOUNT, SHINY_TOKEN, SHINY_SECRET  the preview account's credentials
#   SHINY_APP_NAME   the app-name prefix; previews are <prefix>_PR_<number>
#   PR_NUMBER        remove this PR's preview (the cleanup)
#   OPEN_PRS         or, when PR_NUMBER is empty, remove every preview whose
#                    number is not in this comma-separated list (the sweep)
#
# An app that is not there is not an error: a PR that changed only papers/ is
# never deployed, and before 2026-11-17 its cleanup failed on "No application
# named ... is currently deployed".  Only names of the form <prefix>_PR_<digits>
# are ever touched, so the production app (<prefix> alone) cannot be.

env <- function(name) Sys.getenv(name, unset = "")
account <- env("SHINY_ACCOUNT")
prefix  <- env("SHINY_APP_NAME")
if (!nzchar(account) || !nzchar(prefix)) {
  stop("SHINY_ACCOUNT and SHINY_APP_NAME must be set", call. = FALSE)
}

rsconnect::setAccountInfo(account, env("SHINY_TOKEN"), env("SHINY_SECRET"))

apps <- rsconnect::applications(account = account)
if (is.null(apps) || nrow(apps) == 0) {
  apps <- data.frame(name = character(), status = character())
}
pattern <- paste0("^", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", prefix),
                  "_PR_([0-9]+)$")
previews <- apps[grepl(pattern, apps$name), c("name", "status"), drop = FALSE]
previews$number <- as.integer(sub(pattern, "\\1", previews$name))

prNumber <- env("PR_NUMBER")
if (nzchar(prNumber)) {
  if (!grepl("^[0-9]+$", prNumber)) stop("PR_NUMBER is not a number", call. = FALSE)
  target <- previews[previews$number == as.integer(prNumber), , drop = FALSE]
  if (nrow(target) == 0) {
    message("No preview app for PR #", prNumber, " (never deployed, or already removed).")
  }
} else {
  openPRs <- trimws(strsplit(env("OPEN_PRS"), ",", fixed = TRUE)[[1]])
  openPRs <- openPRs[nzchar(openPRs)]
  if (!all(grepl("^[0-9]+$", openPRs))) stop("OPEN_PRS is not a list of numbers", call. = FALSE)
  target <- previews[!previews$number %in% as.integer(openPRs), , drop = FALSE]
  message("Preview apps: ", nrow(previews), "; of PRs no longer open: ", nrow(target), ".")
}

# A purged app may still be listed as "deleted"; there is nothing left to do.
target <- target[!target$status %in% "deleted", , drop = FALSE]
failed <- character()
for (i in seq_len(nrow(target))) {
  name <- target$name[i]
  message("Removing ", name, " (", target$status[i], ")")
  ok <- tryCatch({
    if (!target$status[i] %in% c("terminated", "archived")) {
      rsconnect::terminateApp(name, account = account)
    }
    rsconnect::purgeApp(name, account = account)
    TRUE
  }, error = function(e) {
    message("  failed: ", conditionMessage(e))
    FALSE
  })
  if (!ok) failed <- c(failed, name)
}

if (length(failed) > 0) {
  stop("Could not remove: ", paste(failed, collapse = ", "), call. = FALSE)
}
