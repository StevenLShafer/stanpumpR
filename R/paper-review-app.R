# Single Paper Review: the app.
#
# A tab of stanpumpR between Simulator and Help (paperReviewNavPanel(),
# paperReviewModuleServer()), and also an app of its own,
# run_paper_review_app().  Upload one manuscript (and optionally a
# supplement), have Claude extract its model, read the review report, then
# simulate the paper's drug -- and only that drug -- in memory.
#
# Confidentiality is the point of the design (docs/paper-review-privacy.md):
#   - an upload is read into this session's memory and its temporary file
#     deleted at once (paperReviewReadUpload());
#   - the manuscript's bytes are dropped as soon as the extraction returns,
#     and everything else when the user discards it or the session ends
#     (paperReviewDiscard(), session$onSessionEnded());
#   - no bookmarking, no URL state, no debug log, no stanpumpR console
#     comments (ECHO_OUTPUT_COMMENTS = NA; in stanpumpR, its inputs are
#     excluded from the bookmark and its computations run with comments off,
#     paperReviewSilently()), and errors are shown to the user
#     through validate(), which Shiny does not write to the server log;
#   - nothing is written to disk except the transient files Shiny itself uses
#     for a plot or a download, which are deleted as they are served.
#
# (Claude Code, 2026-10-30, at the request of Steven L. Shafer.)

#' A session's private store
#'
#' An environment, not a reactiveValues, so that discarding it is one
#' rm() and nothing lingers in a reactive graph.
#' @keywords internal
paperReviewStore <- function() new.env(parent = emptyenv())

# The namespace of the Single Paper Review tab in stanpumpR.
PAPER_REVIEW_MODULE_ID <- "paperReview"

# Evaluate with stanpumpR's comments off, so nothing about a paper reaches
# the console or the ?debug= log of the app it is a tab of.
paperReviewSilently <- function(expr)
{
  old <- options(ECHO_OUTPUT_COMMENTS = NA)
  on.exit(options(old), add = TRUE)
  expr
}

#' Discard everything a session holds about its paper
#'
#' R cannot overwrite freed memory; this removes every reference and asks the
#' garbage collector to reclaim it, which is as far as an R process can go.
#'
#' @param store the session's store
#' @keywords internal
paperReviewDiscard <- function(store)
{
  rm(list = ls(store, all.names = TRUE), envir = store)
  invisible(gc(verbose = FALSE))
}

#' Read an upload into memory and delete its temporary file
#'
#' Shiny writes each upload to a temporary directory of its own; the file is
#' read and then deleted, with its directory when that is left empty.
#'
#' @param datapath the upload's path, from \code{input$<id>$datapath}
#' @returns the file's bytes
#' @keywords internal
paperReviewReadUpload <- function(datapath)
{
  on.exit({
    unlink(datapath, force = TRUE)
    dir <- dirname(datapath)
    if (dir.exists(dir) &&
        normalizePath(dir, mustWork = FALSE) != normalizePath(tempdir(), mustWork = FALSE) &&
        length(list.files(dir, all.files = TRUE, no.. = TRUE)) == 0)
      unlink(dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)
  size <- file.info(datapath)$size
  if (is.na(size)) return(raw(0))
  readBin(datapath, "raw", size)
}

# A file's size as people read it: "30 kB", not "0.0 MB" (Devin's stress test).
paperReviewSize <- function(bytes)
  if (bytes < 1024^2) paste(max(1, round(bytes / 1024)), "kB") else
    paste(format(round(bytes / 1024^2, 1), nsmall = 1), "MB")

# The tokens a review used, which set its cost: " (12,345 tokens read, 6,789
# written)", or "" when the reply did not say.
paperReviewUsageText <- function(usage)
{
  n <- function(x) suppressWarnings(as.numeric(x %or% NA))
  read <- sum(n(usage$input_tokens), n(usage$cache_read_input_tokens), n(usage$cache_creation_input_tokens),
              na.rm = TRUE)
  written <- n(usage$output_tokens)
  if (!is.finite(written) || read == 0) return("")
  paste0(" (", format(read, big.mark = ","), " tokens read, ", format(written, big.mark = ","), " written)")
}

#' Is this a PDF?
#' @keywords internal
paperReviewIsPdf <- function(bytes)
  length(bytes) > 4 && identical(rawToChar(bytes[1:4]), "%PDF")

#' The example extraction, a fictional paper shipped with the package
#' @keywords internal
paperReviewExample <- function()
  jsonlite::fromJSON(system.file("paper-review", "example-extraction.json", package = "stanpumpR"),
                     simplifyVector = FALSE)

paperReviewHelpHtml <- function()
{
  path <- system.file("paper-review", "help.md", package = "stanpumpR")
  if (!nzchar(path)) return(htmltools::p("Help not found."))
  htmltools::HTML(shiny::markdown(paste(readLines(path, warn = FALSE, encoding = "UTF-8"),
                                        collapse = "\n")))
}

# What both forms of the app put above the tabs: the style that greys a
# disabled upload field, and the banner shown while Claude reads the paper.
paperReviewHeader <- function(ns = shiny::NS(NULL))
{
  htmltools::tagList(
    # A disabled upload field looks disabled (shinyjs disables the input,
    # which leaves its Browse button looking live).
    htmltools::tags$style(htmltools::HTML(
      ".form-group:has(input[type=file]:disabled) { opacity: 0.45; pointer-events: none; }")),
    # Shown while Claude reads the paper.  A plain banner, shown and hidden
    # at once: Shiny's progress notification fades out with a jQuery
    # animation, which stalled in the desktop app's browser pane and left
    # the box flashing over the report after the review had finished.
    shinyjs::hidden(htmltools::div(
      id = ns("reviewBusy"), class = "alert alert-info", role = "status",
      style = "position: fixed; top: 64px; left: 50%; transform: translateX(-50%); z-index: 2000; box-shadow: 0 2px 8px rgba(0,0,0,0.2);",
      htmltools::strong(id = ns("reviewBusyTitle"), "Claude is reading the paper."),
      " This usually takes 2 to 7 minutes, depending on the paper, and stops at 20. Keep this page open:",
      " nothing is kept on the server, so closing it loses the review. The rest of stanpumpR can be used meanwhile.",
      # After PAPER_REVIEW_LATE_SEC, the choice of a quick review instead.
      shinyjs::hidden(htmltools::div(id = ns("reviewLate"), style = "margin-top: 8px;",
        htmltools::strong("This is taking longer than usual."),
        " You can keep waiting, or stop and run a quick review: the same paper read again at a lower",
        " effort, usually in 1 to 4 minutes, which may leave out secondary models and some per-patient detail.",
        htmltools::div(style = "margin-top: 6px;",
          shiny::actionButton(ns("keepWaiting"), "Keep waiting", class = "btn-sm btn-secondary"),
          shiny::actionButton(ns("quickReview"), "Stop and run a quick review",
                              class = "btn-sm btn-primary", style = "margin-left: 6px;")))))))
}

# The review's three tabs: Review, Simulator, Privacy and help.
paperReviewPanels <- function(ns = shiny::NS(NULL))
{
  list(
    bslib::nav_panel("Review", value = "review",
      bslib::layout_sidebar(fillable = FALSE,
        sidebar = bslib::sidebar(width = 320,
          # The confirmation comes first; until it is ticked the upload and
          # Review controls are greyed out (Shafer: unticked, the old order
          # let a paper be chosen and then refused, which confused).
          # Said before anything is uploaded when the server has no key
          # (Devin's stress test, 2026-11-08: the reason came only after a
          # paper had been chosen and the box ticked).
          shiny::uiOutput(ns("keyNotice")),
          shiny::checkboxInput(ns("consent"), paste(
            "I may share this paper with Anthropic, as the Privacy tab describes,",
            "to have it reviewed."), FALSE),
          shiny::fileInput(ns("manuscript"), "Manuscript (PDF)", accept = ".pdf"),
          shiny::fileInput(ns("supplement"), "Supplement (PDF, optional)", accept = ".pdf"),
          shiny::actionButton(ns("review"), "Review the paper", class = "btn-primary"),
          shiny::hr(),
          shiny::actionButton(ns("example"), "Try the example (nothing is sent)"),
          shiny::hr(),
          shiny::actionButton(ns("discard"), "Discard the paper now", class = "btn-danger"),
          shiny::uiOutput(ns("downloadUI")),
          shiny::div(style = "margin-top: 12px; font-size: 0.85em;", shiny::textOutput(ns("status")))
        ),
        shiny::uiOutput(ns("report"))
      )),
    bslib::nav_panel("Simulator", value = "simulator",
      bslib::layout_sidebar(fillable = FALSE,
        sidebar = bslib::sidebar(width = 340,
          shinyjs::hidden(htmltools::div(id = ns("modelBox"),
            shiny::selectInput(ns("model"), "Model", character(0)))),
          shiny::numericInput(ns("weight"), "Weight (kg)", 70, min = MIN_WEIGHT, max = MAX_WEIGHT),
          shiny::numericInput(ns("height"), "Height (cm)", 170, min = MIN_HEIGHT, max = MAX_HEIGHT),
          shiny::numericInput(ns("age"), "Age (years)", 40, min = MIN_AGE, max = MAX_AGE),
          shiny::selectInput(ns("sex"), "Sex", SEX_VALUES),
          shiny::selectInput(ns("timeUnit"), "Time units", PAPER_REVIEW_SIM_TIME_UNITS, "min"),
          shiny::numericInput(ns("maximum"), "Max time", PAPER_REVIEW_MAX_TIME_DEFAULT, min = 0),
          shiny::selectInput(ns("regimen"), "Paper's regimen", character(0)),
          shiny::actionButton(ns("loadRegimen"), "Load regimen into the dose table"),
          shiny::selectInput(ns("overlay"), "Overlay a figure's points", character(0)),
          shiny::checkboxInput(ns("logY"), "Log concentration axis", FALSE)
        ),
        shiny::uiOutput(ns("simulatorNote")),
        shiny::plotOutput(ns("simPlot"), height = "460px"),
        shiny::h5("Dose table"),
        shiny::uiOutput(ns("dosesUI")),
        shiny::tableOutput(ns("simParameters"))
      )),
    bslib::nav_panel("Privacy and help", value = "help", paperReviewHelpHtml()))
}

# The app on its own (run_paper_review_app()).
paperReviewUI <- function()
{
  do.call(bslib::page_navbar, c(list(
    title = "stanpumpR: Single Paper Review",
    id = "nav",
    # Not fillable: a filled layout shrinks the plot and the dose table.
    fillable = FALSE,
    theme = bslib::bs_theme(version = 5),
    header = htmltools::tagList(shinyjs::useShinyjs(), paperReviewHeader())),
    paperReviewPanels()))
}

#' Single Paper Review as a tab of stanpumpR
#'
#' The same review, its own three tabs under one entry of stanpumpR's menu,
#' its inputs in the namespace \code{id} (paperReviewModuleServer()).  Its
#' inputs are kept out of stanpumpR's bookmarks (app_server()'s onBookmark).
#' @param id the module's namespace
#' @keywords internal
paperReviewNavPanel <- function(id = PAPER_REVIEW_MODULE_ID)
{
  ns <- shiny::NS(id)
  bslib::nav_panel("Single Paper Review", value = "Single Paper Review",
                   icon = shiny::icon("file-lines"),
    paperReviewHeader(ns),
    do.call(bslib::navset_underline, c(list(id = ns("nav")), paperReviewPanels(ns))))
}

#' The server of the Single Paper Review tab
#' @param id the module's namespace
#' @keywords internal
# The time, through one function the tests can replace.
paperReviewNow <- function() Sys.time()

paperReviewModuleServer <- function(id = PAPER_REVIEW_MODULE_ID)
  shiny::moduleServer(id, paperReviewServer)

paperReviewServer <- function(input, output, session)
{
  store <- paperReviewStore()
  version <- shiny::reactiveVal(0)
  status <- shiny::reactiveVal("Tick the box to upload a paper, or try the example.")
  doseTable <- shiny::reactiveVal(data.frame(Time = 0, Dose = 0, Units = "mg", stringsAsFactors = FALSE))
  # The time unit the dose table, Max time and the plot show.  The dose table
  # is held in minutes; only what is shown is converted.  Held apart from the
  # selector so that a change of unit converts the table rather than rereading
  # its old numbers in the new unit.
  tableUnit <- shiny::reactiveVal("min")
  unitMinutes <- function(u = tableUnit()) paperReviewMinutes(u)
  unitName <- function(u = tableUnit()) names(PAPER_REVIEW_SIM_TIME_UNITS)[PAPER_REVIEW_SIM_TIME_UNITS == u]
  # Show the table, Max time and the plot in unit u, Max time being
  # `maximumMinutes`.
  setUnit <- function(u, maximumMinutes) {
    tableUnit(u)
    shiny::updateSelectInput(session, "timeUnit", selected = u)
    shiny::updateNumericInput(session, "maximum", label = paste0("Max time (", unitName(u), ")"),
                              value = signif(maximumMinutes / paperReviewMinutes(u), 4))
  }
  bump <- function() version(version() + 1)
  # Is a manuscript in memory?  Drives the Review button, which stays
  # disabled until there is one and the box is ticked: the first real test
  # pressed it unticked and got only a line of status text.
  hasPdf <- shiny::reactiveVal(FALSE)
  session$onSessionEnded(function() paperReviewDiscard(store))
  # Can this server review a paper at all?  With the operator's key
  # (Shafer's, the default for now: R/paper-review-usage.R counts its uses)
  # or a key the user enters for this session; without either only the
  # example runs, so nothing may be uploaded for nothing.
  serverKeyed <- !is.null(paperReviewApiKey(NULL))
  # The user's own key, held only in this session's input: never stored,
  # logged or bookmarked (app_server() keeps every paperReview- input out of
  # the URL; the review app has bookmarking off).
  ownKey <- shiny::reactive(paperReviewOwnKey(input$ownKey))
  keyed <- shiny::reactive(serverKeyed || !is.null(ownKey()))
  output$keyNotice <- shiny::renderUI(htmltools::tagList(
    if (!serverKeyed) htmltools::div(
      class = "alert alert-warning", role = "status", style = "font-size: 0.9em; padding: 8px 10px;",
      htmltools::strong("Reviews on this server need your own Anthropic API key. "),
      "Enter it below to turn uploading on; the example still works without one, and sends nothing."),
    htmltools::tags$details(
      open = if (!serverKeyed) NA, style = "font-size: 0.9em; margin-bottom: 10px;",
      htmltools::tags$summary(if (serverKeyed) "Use your own Anthropic API key (optional)"
                              else "Your Anthropic API key"),
      shiny::passwordInput(session$ns("ownKey"), NULL, placeholder = "sk-ant-..."),
      htmltools::p(class = "text-muted", style = "margin-bottom: 0;",
        if (serverKeyed) "Reviews use stanpumpR's own key unless you enter yours. ",
        "Your key is held only in this page's session, sent only to Anthropic with your review ",
        "(which your Anthropic account then pays for), and never saved or logged."),
      shiny::uiOutput(session$ns("ownKeyNote")))))
  output$ownKeyNote <- shiny::renderUI({
    typed <- input$ownKey
    if (!is.character(typed) || !nzchar(trimws(typed))) return(NULL)
    if (is.null(ownKey()))
      htmltools::p(class = "text-danger", style = "margin: 4px 0 0;",
                   "That is not an Anthropic API key (they begin sk-ant-), so it is not used.")
    else htmltools::p(class = "text-success", style = "margin: 4px 0 0;", "Your key will be used.")
  })
  # The review runs in a background process (paperReviewExtractAsync()), so
  # this session and every other on the worker stay live while it runs.
  # A review with the operator's key is first admitted against its limit
  # (paperReviewSharedKeyAdmit(), R/paper-review-usage.R), unless its place
  # was reserved already (a quick review's).  The function that stops its
  # background process is kept, for a quick review.
  running <- new.env(parent = emptyenv())
  reviewTask <- shiny::ExtendedTask$new(function(pdf, supplement, key, inferenceGeo, shared, effort, admitted) {
    running$stop <- NULL
    send <- function(...) paperReviewTrackUse(
      paperReviewExtractAsync(pdf, supplement, key = key, inferenceGeo = inferenceGeo, effort = effort,
                              onJob = function(stop) running$stop <- stop),
      shared = shared)
    if (!isTRUE(shared) || isTRUE(admitted)) return(send())
    promises::then(paperReviewSharedKeyAdmit(), send)
  })
  reviewing <- shiny::reactive(identical(reviewTask$status(), "running"))
  # The review now running: when it started, whether it is the quick one,
  # whether the user has been offered the quick one, and whether they took it.
  reviewStarted <- shiny::reactiveVal(NULL)
  quick <- shiny::reactiveVal(FALSE)
  lateOffered <- shiny::reactiveVal(FALSE)
  stopForQuick <- shiny::reactiveVal(FALSE)
  # A quick review's place within the shared key's limit, reserved before
  # the full review is stopped.
  preAdmitted <- shiny::reactiveVal(FALSE)
  # A quick review's place being asked for (a second click waits for it).
  admitting <- shiny::reactiveVal(FALSE)
  releaseReserved <- function() if (isTRUE(preAdmitted())) {
    preAdmitted(FALSE)
    paperReviewSharedKeyRelease()
  }
  # The review's key and settings, kept with the manuscript (store$inFlight)
  # so that the quick review can be sent without asking again.
  sendReview <- function(effort, admitted = FALSE) {
    f <- store$inFlight
    reviewStarted(paperReviewNow())
    lateOffered(FALSE)
    quick(!identical(effort, PAPER_REVIEW_EFFORT))
    reviewTask$invoke(f$pdf, f$supplement, paperReviewApiKey(f$own), paperReviewConfig("paper_review_inference_geo"),
                      is.null(f$own), effort, admitted)
  }
  # Is the running review still wanted?  Discarding while Claude reads the
  # paper means its answer is dropped when it comes.
  awaited <- shiny::reactiveVal(FALSE)
  shiny::observe({
    consent <- isTRUE(input$consent) && keyed()
    shinyjs::toggleState("consent", condition = keyed())
    shinyjs::toggleState("manuscript", condition = consent)
    shinyjs::toggleState("supplement", condition = consent)
    shinyjs::toggleState("review", condition = hasPdf() && consent && !reviewing())
    # Discard is live only when there is something to discard.
    shinyjs::toggleState("discard", condition = hasPdf() || !is.null(review()) || reviewing())
  })

  receive <- function(file, slot, label) {
    if (is.null(file)) return()
    bytes <- paperReviewReadUpload(file$datapath)
    if (!paperReviewIsPdf(bytes)) {
      status(paste(label, "is not a PDF and was discarded."))
      return()
    }
    assign(slot, bytes, envir = store)
    if (slot == "pdf") hasPdf(TRUE)
    status(paste0(label, " received (", paperReviewSize(length(bytes)),
                  "), held in this session's memory only."))
  }
  shiny::observeEvent(input$manuscript, receive(input$manuscript, "pdf", "The manuscript"))
  shiny::observeEvent(input$supplement, receive(input$supplement, "supplement", "The supplement"))

  runReview <- function(extraction, example = FALSE, quick = FALSE) {
    review <- tryCatch(paperReviewQuietly(function() paperReviewRun(extraction)),
                       error = function(e) NULL)
    if (!is.null(review)) review$quick <- quick
    if (is.null(review)) {
      status("The extraction came back but the review could not be computed from it.")
      assign("extraction", extraction, envir = store)
      bump()
      return()
    }
    assign("extraction", extraction, envir = store)
    assign("review", review, envir = store)
    assign("example", example, envir = store)
    bump()
  }

  shiny::observeEvent(input$review, {
    if (reviewing()) return(status("A review is already running."))
    if (!exists("pdf", envir = store)) return(status("Upload a manuscript first."))
    if (!isTRUE(input$consent)) return(status("Tick the box to confirm the manuscript may be sent."))
    own <- ownKey()
    key <- paperReviewApiKey(own)
    if (is.null(key)) return(status("No Anthropic API key is configured on this server; enter your own."))
    shinyjs::show("reviewBusy")
    shinyjs::hide("reviewLate")
    shinyjs::html("reviewBusyTitle", "Claude is reading the paper.")
    shinyjs::disable("review")
    status("Claude is reading the paper.")
    awaited(TRUE)
    rm(key)
    # The manuscript stays in the session's memory, out of the upload slot,
    # only until Claude's answer returns, so that a quick review can be sent
    # if this one is slow; then it is dropped, whatever the outcome (and at
    # Discard or the session's end before that).  The key is the user's own
    # or the operator's, resolved again when sent.
    store$inFlight <- list(pdf = store$pdf,
                           supplement = if (exists("supplement", envir = store)) store$supplement,
                           own = own)
    suppressWarnings(rm(list = intersect(c("pdf", "supplement"), ls(store)), envir = store))
    hasPdf(FALSE)
    sendReview(PAPER_REVIEW_EFFORT)
  })

  # A full review still running after PAPER_REVIEW_LATE_SEC is offered a
  # quick one, once.
  shiny::observe({
    if (!reviewing() || quick() || lateOffered() || is.null(reviewStarted())) return()
    left <- PAPER_REVIEW_LATE_SEC - as.numeric(difftime(paperReviewNow(), reviewStarted(), units = "secs"))
    if (left > 0) return(shiny::invalidateLater(ceiling(left * 1000)))
    lateOffered(TRUE)
    shinyjs::show("reviewLate")
  })
  shiny::observeEvent(input$keepWaiting, shinyjs::hide("reviewLate"))
  shiny::observeEvent(input$quickReview, {
    shinyjs::hide("reviewLate")
    if (!reviewing() || quick() || stopForQuick() || admitting() || !exists("inFlight", envir = store)) return()
    # The stopped review is billed for what it had read, and counted.
    stopFull <- function() {
      stopForQuick(TRUE)
      status("Stopping the full review and starting a quick one.")
      if (is.function(running$stop)) running$stop()
    }
    if (!is.null(store$inFlight$own)) return(stopFull())
    # With stanpumpR's own key, the quick review's place within the limit is
    # reserved first; with none left, the full review goes on (CodeRabbit,
    # PR #720).
    status("Checking that a quick review can be sent.")
    admitting(TRUE)
    promises::then(paperReviewSharedKeyAdmit(),
      onFulfilled = function(...) {
        admitting(FALSE)
        # A place that comes back after the review has moved on is given back.
        if (!shiny::isolate(reviewing()) || shiny::isolate(quick()) || shiny::isolate(stopForQuick()) ||
            !exists("inFlight", envir = store))
          return(paperReviewSharedKeyRelease())
        preAdmitted(TRUE)
        stopFull()
      },
      onRejected = function(e) {
        admitting(FALSE)
        status(paste("A quick review cannot be sent:", conditionMessage(e), "The full review goes on."))
      })
    NULL
  })

  shiny::observeEvent(reviewTask$status(), {
    if (reviewTask$status() %in% c("initial", "running")) return()
    # The full review stopped for a quick one: send the quick one now.
    if (isTRUE(stopForQuick())) {
      stopForQuick(FALSE)
      # (A full review that finished before it could be stopped is kept.)
      if (identical(reviewTask$status(), "error") && awaited() && exists("inFlight", envir = store)) {
        shinyjs::html("reviewBusyTitle", "Claude is reading the paper again, for a quick review.")
        status("Claude is reading the paper again, for a quick review.")
        admitted <- preAdmitted()
        preAdmitted(FALSE)
        sendReview(PAPER_REVIEW_QUICK_EFFORT, admitted = admitted)
        return()
      }
    }
    releaseReserved()
    shinyjs::hide("reviewBusy")
    shinyjs::hide("reviewLate")
    running$stop <- NULL
    suppressWarnings(rm(list = intersect("inFlight", ls(store)), envir = store))
    if (!awaited()) return()
    awaited(FALSE)
    result <- tryCatch(reviewTask$result(),
      paper_review_error = function(e) e,
      error = function(e) paperReviewError("unexpected", "The review failed unexpectedly."))
    if (inherits(result, "paper_review_error")) {
      status(paste(conditionMessage(result), "The manuscript has been discarded; upload it again to retry."))
      return()
    }
    assign("usage", result$usage, envir = store)
    runReview(result$extraction, quick = quick())
    status(paste0(if (quick()) "A quick review, done" else "Reviewed", paperReviewUsageText(result$usage),
                  ". The manuscript itself has been ",
                  "discarded; the extraction is held in this session's memory until you discard it ",
                  "or close the page."))
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$example, {
    runReview(paperReviewExample(), example = TRUE)
    status("Showing the built-in example, a fictional paper. Nothing was sent anywhere.")
  })

  shiny::observeEvent(input$discard, {
    paperReviewDiscard(store)
    awaited(FALSE)
    stopForQuick(FALSE)
    releaseReserved()
    running$stop <- NULL
    shinyjs::hide("reviewBusy")
    shinyjs::hide("reviewLate")
    hasPdf(FALSE)
    bump()
    status("Discarded. Nothing about the paper remains in this session.")
  })

  review <- shiny::reactive({ version(); if (exists("review", envir = store)) store$review else NULL })

  output$status <- shiny::renderText(status())

  output$report <- shiny::renderUI({
    r <- review()
    if (is.null(r))
      return(htmltools::div(htmltools::h3("Single Paper Review"),
        htmltools::p("Upload one pharmacokinetic manuscript. Claude reads it, the model it describes is ",
                     "checked in stanpumpR for identifiability, internal consistency, its covariates and ",
                     "its figures, and the drug can then be simulated in the Simulator tab."),
        htmltools::p("Read the Privacy and help tab before uploading a confidential manuscript.")))
    tags <- tryCatch(paperReviewSilently(paperReviewReportTags(r, plots = TRUE)), error = function(e) NULL)
    shiny::validate(shiny::need(!is.null(tags), "The report could not be rendered."))
    htmltools::tagList(
      if (isTRUE(store$example)) htmltools::div(class = "alert alert-info",
        "This is the built-in example: a fictional paper with a deliberate misprint."),
      tags)
  })

  output$downloadUI <- shiny::renderUI({
    if (is.null(review()) || !isTRUE(paperReviewConfig("paper_review_download", TRUE))) return(NULL)
    shiny::downloadButton(session$ns("download"), "Download the report (HTML)", style = "margin-top: 12px;")
  })
  # Shiny writes the page to a temporary file it owns and deletes once served.
  output$download <- shiny::downloadHandler(
    filename = "single-paper-review.html",
    content = function(file) writeLines(paperReviewReportPage(review()), file, useBytes = TRUE),
    contentType = "text/html")

  # --- Simulator ---------------------------------------------------------------

  # A paper with several models (a drug in two populations, two drugs) is
  # simulated one model at a time, chosen here; one model needs no choice.
  currentReview <- shiny::reactive({
    r <- review()
    if (is.null(r) || is.null(r$models)) return(r)
    m <- input$model
    if (is.character(m) && length(m) == 1 && m %in% names(r$models)) r$models[[m]] else r$models[[1]]
  })
  shiny::observeEvent(version(), {
    r <- review()
    if (!is.null(r) && !is.null(r$models)) {
      shiny::updateSelectInput(session, "model", choices = names(r$models), selected = names(r$models)[1])
      shinyjs::show("modelBox")
    } else {
      shinyjs::hide("modelBox")
    }
  }, ignoreInit = FALSE)

  shiny::observeEvent(list(version(), input$model), {
    r <- currentReview()
    if (is.null(r)) {
      shiny::updateSelectInput(session, "regimen", choices = character(0))
      shiny::updateSelectInput(session, "overlay", choices = character(0))
      doseTable(data.frame(Time = 0, Dose = 0, Units = "mg", stringsAsFactors = FALSE))
      return()
    }
    ref <- r$reference
    shiny::updateNumericInput(session, "weight", value = signif(ref$weight, 3))
    shiny::updateNumericInput(session, "height", value = signif(ref$height, 3))
    shiny::updateNumericInput(session, "age", value = signif(ref$age, 3))
    shiny::updateSelectInput(session, "sex", selected = ref$sex)
    regs <- r$extraction$regimens %or% list()
    ids <- vapply(regs, function(x) x$id %or% "", character(1))
    # paste0() of zero ids still gives one label, so a paper with no regimen
    # (the Duzitol editorial) needs its own case.
    labels <- if (length(ids)) paste0(ids, ": ", vapply(regs, function(x) x$description %or% "", character(1)))
              else character(0)
    shiny::updateSelectInput(session, "regimen", choices = stats::setNames(ids, labels))
    figs <- vapply(Filter(function(f) !identical(f$kind, "decrement"), r$figures),
                   function(f) f$id, character(1))
    shiny::updateSelectInput(session, "overlay", choices = c("None" = "", figs))
    units <- r$drug$units
    doseTable(data.frame(Time = 0, Dose = 0, Units = if (length(units)) units[1] else "mg",
                         stringsAsFactors = FALSE))
    first <- if (length(regs)) regs[[1]] else NULL
    setUnit(paperReviewRegimenTimeUnit(r$extraction, first), PAPER_REVIEW_MAX_TIME_DEFAULT)
  }, ignoreInit = FALSE)

  # The user picked another unit: convert what is shown.
  shiny::observeEvent(input$timeUnit, {
    if (identical(input$timeUnit, tableUnit())) return()
    # A Max time that cannot be used (blank, say) becomes the default rather
    # than following the table into the new unit as NA.
    maximumMinutes <- paperReviewMaxTimeMinutes(input$maximum, unitMinutes())
    if (is.na(maximumMinutes)) maximumMinutes <- PAPER_REVIEW_MAX_TIME_DEFAULT
    # A table cut to fit (more rows than the grid holds, which only a forged
    # value can send) is not stored cut: the unit stays as it was and the
    # table is left to be refused by the simulation (CodeRabbit, PR #691)
    if (!is.null(input$doses) &&
        tableGridTruncated(tableGridRead(input$doses, paperReviewDoseColumns("", character(0))))) {
      shiny::showNotification(sprintf("The dose table holds at most %d rows; the time unit was not changed.",
                                      MAX_DOSE_ROWS - 1L), type = "warning")
      setUnit(tableUnit(), maximumMinutes)
      return()
    }
    current <- dosesNow()
    attr(current, "problem") <- NULL
    doseTable(current)
    setUnit(input$timeUnit, maximumMinutes)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$loadRegimen, {
    r <- currentReview(); if (is.null(r)) return()
    regimen <- paperReviewRegimen(r$extraction, input$regimen)
    if (is.null(regimen)) return()
    d <- paperReviewRegimenDoses(regimen)$doses
    if (nrow(d) == 0) return()
    # The grid holds MAX_DOSE_ROWS - 1 rows; a longer regimen (an hourly
    # bolus for 500 hours) would be cut by the browser and then refused, or
    # cut without a word by a change of time unit, so it is refused here.
    if (nrow(d) > MAX_DOSE_ROWS - 1L) {
      shiny::showNotification(sprintf(
        "This regimen has %d doses; the dose table holds at most %d. Enter a shorter part of it by hand.",
        nrow(d), MAX_DOSE_ROWS - 1L), type = "warning")
      return()
    }
    doseTable(d)
    # In the unit the paper uses for this regimen.
    setUnit(paperReviewRegimenTimeUnit(r$extraction, regimen), max(PAPER_REVIEW_MAX_TIME_DEFAULT, ceiling(max(d$Time) * 2)))
  })

  # The doses as text, read as R reads them (validateDose()): a numeric
  # column parsed "1,000" as 1 and "1,5" as 1.5, and refused nothing where it
  # was typed (docs/grid-replacement.md, step 3 of the migration order)
  output$dosesUI <- shiny::renderUI({
    r <- currentReview()
    units <- if (is.null(r)) "mg" else r$drug$units
    d <- doseTable()
    d$Units <- as.character(d$Units)
    d$Time <- signif(d$Time / unitMinutes(), 6)
    # Up to four blank rows to type into, never past the grid's limit, which
    # would make tableGridRead() report the table as cut.
    blankRows <- max(0L, min(4L, MAX_DOSE_ROWS - 1L - nrow(d)))
    blank <- data.frame(Time = rep(NA_real_, blankRows), Dose = rep(NA_real_, blankRows),
                        Units = rep(NA_character_, blankRows))
    shiny::div(style = "max-width: 520px",
      tableGridInput(session$ns("doses"), paperReviewDoseColumns(unitName(), units),
                     rbind(d, blank), rowMenu = TRUE, height = 230, label = "Doses"))
  })

  dosesNow <- shiny::reactive({
    # Minutes, whatever the table shows.
    shown <- !is.null(input$doses)
    d <- if (shown) tableGridRead(input$doses, paperReviewDoseColumns("", character(0))) else doseTable()
    if (is.null(d)) d <- doseTable()
    names(d)[1:3] <- c("Time", "Dose", "Units")
    # A row the grid would not have sent, from a browser that may send
    # anything: a time or dose validateDose() refuses (past MAX_DOSE_VALUE,
    # say), or a unit the paper's drug does not offer.  Read before, it was
    # simulated without a word, an unoffered "mcg" or "g" as itself and
    # anything else as mg (ChatGPT's fourth review, F01).  A row still
    # being filled in, with a cell blank, is passed over as before.
    problem <- NULL
    # A table cut to fit was never one the grid sent (Gemini's production
    # audit, 2026-11-08, finding 6)
    if (shown && tableGridTruncated(d)) {
      problem <- sprintf("The dose table holds at most %d rows.", MAX_DOSE_ROWS - 1L)
    }
    if (shown) {
      r <- currentReview()
      units <- if (is.null(r)) character(0) else as.character(r$drug$units)
      filled <- nzchar(trimws(d$Time)) & nzchar(trimws(d$Dose)) & !is.na(d$Units) & nzchar(d$Units)
      readable <- function(x) vapply(x, function(v) nzchar(validateDose(v)), logical(1), USE.NAMES = FALSE)
      bad <- filled & !(readable(d$Time) & readable(d$Dose) & d$Units %in% units)
      if (any(bad)) {
        problem <- paste(c(problem, sprintf(paste0("Row %s cannot be simulated: enter a time and a dose from 0 to ",
                                                   "1,000,000,000, and a unit from the list (%s)."),
                                            paste(which(bad), collapse = ", "), paste(units, collapse = ", "))),
                         collapse = " ")
        d <- d[!bad, , drop = FALSE]
      }
      # Read as validateDose() reads them: "1,000" passed the check above and
      # was then dropped by as.numeric() below without a word (the same audit)
      # A blank is left blank: validateDose() reads it as 0, which put a row
      # still being filled in (a dose and unit, no time) at time 0 (CodeRabbit)
      fill <- function(x) vapply(x, function(v) {
        if (isBlankEntry(v)) return(v)
        r <- validateDose(v)
        if (nzchar(r)) r else v
      }, character(1), USE.NAMES = FALSE)
      d$Time <- fill(d$Time)
      d$Dose <- fill(d$Dose)
    }
    d <- d[is.finite(suppressWarnings(as.numeric(d$Time))) &
             is.finite(suppressWarnings(as.numeric(d$Dose))) & !is.na(d$Units) & nzchar(d$Units), ,
           drop = FALSE]
    d$Time <- as.numeric(d$Time) * if (shown) unitMinutes() else 1
    d$Dose <- as.numeric(d$Dose)
    d <- d[order(d$Time), , drop = FALSE]
    attr(d, "problem") <- problem
    d
  })

  patient <- shiny::reactive(list(weight = input$weight, height = input$height,
                                  age = input$age, sex = input$sex))

  simulation <- shiny::reactive({
    r <- currentReview()
    shiny::validate(shiny::need(!is.null(r), "Review a paper, or try the example, first."))
    p <- patient()
    shiny::validate(shiny::need(
      is_valid_number(p$weight, MIN_WEIGHT, MAX_WEIGHT) && is_valid_number(p$height, MIN_HEIGHT, MAX_HEIGHT) &&
        is_valid_number(p$age, MIN_AGE, MAX_AGE), "Enter a weight, height and age in range."))
    d <- dosesNow()
    shiny::validate(shiny::need(is.null(attr(d, "problem")), attr(d, "problem")))
    shiny::validate(shiny::need(nrow(d) > 0 && any(d$Dose > 0), "Enter a dose."))
    drug <- paperReviewSilently(paperReviewDrug(r$extraction, p, r$identifiability$closed,
                                                absorption = simAbsorption(), absorptions = r$absorptions))
    shiny::validate(shiny::need(drug$ok, paste(drug$problems, collapse = " ")))
    maximum <- paperReviewMaxTimeMinutes(input$maximum, unitMinutes())
    shiny::validate(shiny::need(!is.na(maximum), sprintf(
      "Enter a Max time greater than 0 and no longer than %s %s (365 days).",
      format(signif(PAPER_REVIEW_MAX_TIME_LIMIT / unitMinutes(), 6), big.mark = ",", scientific = FALSE),
      unitName())))
    maximum <- max(maximum, max(d$Time) + 1)
    # Run on far enough to cover an overlaid figure's points.
    fig <- Filter(function(f) identical(f$id, input$overlay), r$figures)
    if (length(fig)) maximum <- max(maximum, max(fig[[1]]$points$time, na.rm = TRUE) * 1.05)
    sim <- tryCatch(paperReviewSimulate(drug, p, d, maximum), error = function(e) e)
    shiny::validate(shiny::need(!inherits(sim, "error"),
                                if (inherits(sim, "error")) conditionMessage(sim) else ""))
    list(drug = drug, sim = sim)
  })

  # The absorption extravascular doses take: the selected regimen's when it
  # has one (each formulation its own), else the first regimen that does.
  simAbsorption <- shiny::reactive({
    r <- currentReview()
    if (is.null(r)) return(NULL)
    usable <- Filter(function(a) isTRUE(a$ok), r$absorptions %or% list())
    if (length(usable) == 0) return(NULL)
    own <- usable[[input$regimen %or% ""]]
    if (is.null(own)) usable[[1]] else own
  })

  output$simulatorNote <- shiny::renderUI({
    r <- currentReview()
    if (is.null(r)) return(NULL)
    a <- simAbsorption()
    absorptionText <- if (is.null(a)) NULL else {
      regimen <- paperReviewRegimen(r$extraction, a$regimen)
      htmltools::p(style = "margin: 6px 0 0 0;", paste0(
        "Extravascular doses use the absorption of ",
        if (is.null(regimen)) gsub("_", " ", a$route) else paste0(a$regimen, " (", regimen$description, ")"),
        ": ka ", prFmt(a$ka * 60), " /h from ", a$sources[["ka"]] %or% "printed",
        "; F ", prFmt(a$F), if (!is.null(a$sources[["F"]])) paste0(" from ", a$sources[["F"]]) else "",
        if (a$fraction != 1) paste0("; each dosed mg is ", prFmt(a$fraction), " mg of the analyte") else "",
        ". Choose another regimen above to use its absorption."))
    }
    htmltools::div(class = "alert alert-secondary", style = "font-size: 0.9em;",
      htmltools::strong(r$drug$defaults$Drug), ": the paper's model, built in this session's ",
      "memory for the patient entered. It is not a stanpumpR library drug and was not reviewed ",
      "by anyone. Covariates the simulator does not take are held at their reference values.",
      absorptionText)
  })

  output$simPlot <- shiny::renderPlot({
    s <- simulation()
    curve <- s$sim$curve
    Time <- Concentration <- Site <- time <- observed <- NULL
    long <- rbind(data.frame(Time = curve$Time, Concentration = curve$Cp, Site = "Plasma"),
                  if (any(is.finite(curve$Ce)) && any(curve$Ce > 0))
                    data.frame(Time = curve$Time, Concentration = curve$Ce, Site = "Effect site"))
    if (isTRUE(input$logY)) long <- long[long$Concentration > 0, ]
    axis <- list(minutes = unitMinutes(), label = unitName())
    long$Time <- long$Time / axis$minutes
    # stanpumpR's convention: plasma solid, effect site dashed.
    p <- ggplot2::ggplot(long, ggplot2::aes(Time, Concentration, linetype = Site)) +
      ggplot2::geom_line(colour = "#1f4e9c", linewidth = 1) +
      ggplot2::scale_linetype_manual(values = c(Plasma = "solid", `Effect site` = "dashed")) +
      ggplot2::labs(x = paste0("Time (", axis$label, ")"), linetype = NULL,
                    y = paste0("Concentration (", s$drug$defaults$Concentration.Units, "/mL)"),
                    title = s$drug$defaults$Drug) +
      ggplot2::theme_bw(base_size = 14)
    r <- currentReview()
    if (nzchar(input$overlay %or% "")) {
      fig <- Filter(function(f) identical(f$id, input$overlay), r$figures)
      if (length(fig)) {
        pts <- fig[[1]]$points
        pts$time <- pts$time / axis$minutes
        p <- p + ggplot2::geom_point(data = pts, ggplot2::aes(time, observed),
                                     inherit.aes = FALSE, size = 2.5)
      }
    }
    if (isTRUE(input$logY)) p <- p + ggplot2::scale_y_log10()
    p
  })

  output$simParameters <- shiny::renderTable({
    # Every parameter the model implies for this patient, as Convert.xls
    # lists them: the covariates act on the volumes and clearances, and the
    # rest follows.
    paperReviewPKTable(simulation()$sim$pk$PK$default, tableUnit())
  }, caption = "Every parameter of the model, for this patient")
}

# A setting of the review app's configuration.
paperReviewConfig <- function(name, default = NULL)
{
  config <- .sprglobals$config
  value <- if (is.list(config)) config[[name]] else NULL
  if (is.null(value)) default else value
}

#' Launch Single Paper Review
#'
#' A separate app from \code{run_app()}: upload one pharmacokinetic
#' manuscript, have Claude extract its model through the Anthropic API, read a
#' review of it computed in stanpumpR, and simulate the paper's drug.  The API
#' key is the user's own, entered for the session, or else the operator's,
#' read from the \code{STANPUMPR_ANTHROPIC_API_KEY} or
#' \code{ANTHROPIC_API_KEY} environment variable or \code{anthropic_api_key}
#' in the config file (unless \code{paper_review_shared_key} is
#' \code{false}).  See
#' \code{docs/paper-review.md} and \code{docs/paper-review-privacy.md}.
#'
#' @param config_file Path to a YAML configuration file, read by
#'   [config::get()].
#' @return A Shiny app object.
#' @export
run_paper_review_app <- function(config_file = "config.yml")
{
  config <- tryCatch(config::get(file = config_file), error = function(e) list())
  config <- c(config, DEFAULT_CONFIG[!names(DEFAULT_CONFIG) %in% names(config)])
  .sprglobals$config <- config
  # Nothing from stanpumpR's own comment log reaches the console in this app.
  options(ECHO_OUTPUT_COMMENTS = NA)
  options(shiny.maxRequestSize = PAPER_REVIEW_MAX_UPLOAD_BYTES)
  shiny::shinyApp(paperReviewUI(), function(input, output, session) {
    # A dropped connection (a hidden pane, a sleeping laptop) may rejoin the
    # same session rather than leave an upload with nowhere to go.
    session$allowReconnect(TRUE)
    paperReviewServer(input, output, session)
  }, enableBookmarking = "disable")
}
