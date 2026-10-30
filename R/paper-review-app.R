# Single Paper Review: the app.
#
# Its own entry point, run_paper_review_app(), so that nothing here touches
# app_server() or app_ui().  Upload one manuscript (and optionally a
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
#     comments (ECHO_OUTPUT_COMMENTS = NA), and errors are shown to the user
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

paperReviewUI <- function()
{
  bslib::page_navbar(
    title = "stanpumpR: Single Paper Review",
    id = "nav",
    # Not fillable: a filled layout shrinks the plot and the dose table.
    fillable = FALSE,
    theme = bslib::bs_theme(version = 5),
    bslib::nav_panel("Review", value = "review",
      bslib::layout_sidebar(fillable = FALSE,
        sidebar = bslib::sidebar(width = 320,
          shiny::fileInput("manuscript", "Manuscript (PDF)", accept = ".pdf"),
          shiny::fileInput("supplement", "Supplement (PDF, optional)", accept = ".pdf"),
          shiny::checkboxInput("consent", paste(
            "I may share this manuscript with Anthropic, as the Privacy tab describes,",
            "to have it reviewed."), FALSE),
          shiny::actionButton("review", "Review the paper", class = "btn-primary"),
          shiny::hr(),
          shiny::actionButton("example", "Try the example (nothing is sent)"),
          shiny::hr(),
          shiny::actionButton("discard", "Discard the paper now", class = "btn-danger"),
          shiny::uiOutput("downloadUI"),
          shiny::div(style = "margin-top: 12px; font-size: 0.85em;", shiny::textOutput("status"))
        ),
        shiny::uiOutput("report")
      )),
    bslib::nav_panel("Simulator", value = "simulator",
      bslib::layout_sidebar(fillable = FALSE,
        sidebar = bslib::sidebar(width = 340,
          shiny::numericInput("weight", "Weight (kg)", 70, min = MIN_WEIGHT, max = MAX_WEIGHT),
          shiny::numericInput("height", "Height (cm)", 170, min = MIN_HEIGHT, max = MAX_HEIGHT),
          shiny::numericInput("age", "Age (years)", 40, min = MIN_AGE, max = MAX_AGE),
          shiny::selectInput("sex", "Sex", SEX_VALUES),
          shiny::numericInput("maximum", "Max time (minutes)", 240, min = 10, max = 60 * 24 * 28),
          shiny::selectInput("regimen", "Paper's regimen", character(0)),
          shiny::actionButton("loadRegimen", "Load regimen into the dose table"),
          shiny::selectInput("overlay", "Overlay a figure's points", character(0)),
          shiny::checkboxInput("logY", "Log concentration axis", FALSE)
        ),
        shiny::uiOutput("simulatorNote"),
        shiny::plotOutput("simPlot", height = "460px"),
        shiny::h5("Dose table (time in minutes)"),
        rhandsontable::rHandsontableOutput("doses"),
        shiny::tableOutput("simParameters")
      )),
    bslib::nav_panel("Privacy and help", value = "help", paperReviewHelpHtml())
  )
}

paperReviewServer <- function(input, output, session)
{
  store <- paperReviewStore()
  version <- shiny::reactiveVal(0)
  status <- shiny::reactiveVal("No paper loaded.")
  doseTable <- shiny::reactiveVal(data.frame(Time = 0, Dose = 0, Units = "mg", stringsAsFactors = FALSE))
  bump <- function() version(version() + 1)
  session$onSessionEnded(function() paperReviewDiscard(store))

  receive <- function(file, slot, label) {
    if (is.null(file)) return()
    bytes <- paperReviewReadUpload(file$datapath)
    if (!paperReviewIsPdf(bytes)) {
      status(paste(label, "is not a PDF and was discarded."))
      return()
    }
    assign(slot, bytes, envir = store)
    status(paste0(label, " received (", format(round(length(bytes) / 1024^2, 1), nsmall = 1),
                  " MB), held in this session's memory only."))
  }
  shiny::observeEvent(input$manuscript, receive(input$manuscript, "pdf", "The manuscript"))
  shiny::observeEvent(input$supplement, receive(input$supplement, "supplement", "The supplement"))

  runReview <- function(extraction, example = FALSE) {
    review <- tryCatch(paperReviewQuietly(function() paperReviewRun(extraction)),
                       error = function(e) NULL)
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
    if (!exists("pdf", envir = store)) return(status("Upload a manuscript first."))
    if (!isTRUE(input$consent)) return(status("Tick the box to confirm the manuscript may be sent."))
    key <- paperReviewApiKey(if (exists("sessionKey", envir = store)) store$sessionKey else NULL)
    if (is.null(key)) return(status("No Anthropic API key is configured on this server."))
    status("Reviewing: this takes a few minutes.")
    result <- shiny::withProgress(message = "Claude is reading the paper", value = 0.2, {
      tryCatch(
        paperReviewExtract(store$pdf, if (exists("supplement", envir = store)) store$supplement,
                           key = key,
                           inferenceGeo = paperReviewConfig("paper_review_inference_geo")),
        paper_review_error = function(e) e,
        error = function(e) paperReviewError("unexpected", "The review failed unexpectedly."))
    })
    rm(key)
    # The manuscript is not needed again: drop it whatever the outcome.
    suppressWarnings(rm(list = intersect(c("pdf", "supplement"), ls(store)), envir = store))
    if (inherits(result, "paper_review_error")) {
      status(paste(conditionMessage(result), "The manuscript has been discarded; upload it again to retry."))
      return()
    }
    assign("usage", result$usage, envir = store)
    runReview(result$extraction)
    status(paste0("Reviewed. The manuscript itself has been discarded; the extraction is held in ",
                  "this session's memory until you discard it or close the page."))
  })

  shiny::observeEvent(input$example, {
    runReview(paperReviewExample(), example = TRUE)
    status("Showing the built-in example, a fictional paper. Nothing was sent anywhere.")
  })

  shiny::observeEvent(input$discard, {
    paperReviewDiscard(store)
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
    tags <- tryCatch(paperReviewReportTags(r, plots = TRUE), error = function(e) NULL)
    shiny::validate(shiny::need(!is.null(tags), "The report could not be rendered."))
    htmltools::tagList(
      if (isTRUE(store$example)) htmltools::div(class = "alert alert-info",
        "This is the built-in example: a fictional paper with a deliberate misprint."),
      tags)
  })

  output$downloadUI <- shiny::renderUI({
    if (is.null(review()) || !isTRUE(paperReviewConfig("paper_review_download", TRUE))) return(NULL)
    shiny::downloadButton("download", "Download the report (HTML)", style = "margin-top: 12px;")
  })
  # Shiny writes the page to a temporary file it owns and deletes once served.
  output$download <- shiny::downloadHandler(
    filename = "single-paper-review.html",
    content = function(file) writeLines(paperReviewReportPage(review()), file, useBytes = TRUE),
    contentType = "text/html")

  # --- Simulator ---------------------------------------------------------------

  shiny::observeEvent(version(), {
    r <- review()
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
    labels <- paste0(ids, ": ", vapply(regs, function(x) x$description %or% "", character(1)))
    shiny::updateSelectInput(session, "regimen", choices = stats::setNames(ids, labels))
    figs <- vapply(r$figures, function(f) f$id, character(1))
    shiny::updateSelectInput(session, "overlay", choices = c("None" = "", figs))
    units <- r$drug$units
    doseTable(data.frame(Time = 0, Dose = 0, Units = if (length(units)) units[1] else "mg",
                         stringsAsFactors = FALSE))
  }, ignoreInit = FALSE)

  shiny::observeEvent(input$loadRegimen, {
    r <- review(); if (is.null(r)) return()
    regimen <- paperReviewRegimen(r$extraction, input$regimen)
    if (is.null(regimen)) return()
    d <- paperReviewRegimenDoses(regimen)$doses
    if (nrow(d) == 0) return()
    doseTable(d)
    shiny::updateNumericInput(session, "maximum", value = max(240, ceiling(max(d$Time) * 2)))
  })

  output$doses <- rhandsontable::renderRHandsontable({
    r <- review()
    units <- if (is.null(r)) "mg" else r$drug$units
    d <- doseTable()
    d$Units <- as.character(d$Units)
    blank <- data.frame(Time = rep(NA_real_, 4), Dose = rep(NA_real_, 4), Units = rep(NA_character_, 4))
    rhandsontable::hot_col(
      rhandsontable::rhandsontable(rbind(d, blank), rowHeaders = NULL, stretchH = "all",
                                   width = 520, height = 230),
      "Units", type = "dropdown", source = units)
  })

  dosesNow <- shiny::reactive({
    d <- if (!is.null(input$doses)) rhandsontable::hot_to_r(input$doses) else doseTable()
    d <- d[is.finite(suppressWarnings(as.numeric(d$Time))) &
             is.finite(suppressWarnings(as.numeric(d$Dose))) & !is.na(d$Units) & nzchar(d$Units), ,
           drop = FALSE]
    d$Time <- as.numeric(d$Time); d$Dose <- as.numeric(d$Dose)
    d[order(d$Time), , drop = FALSE]
  })

  patient <- shiny::reactive(list(weight = input$weight, height = input$height,
                                  age = input$age, sex = input$sex))

  simulation <- shiny::reactive({
    r <- review()
    shiny::validate(shiny::need(!is.null(r), "Review a paper, or try the example, first."))
    p <- patient()
    shiny::validate(shiny::need(
      is_valid_number(p$weight, MIN_WEIGHT, MAX_WEIGHT) && is_valid_number(p$height, MIN_HEIGHT, MAX_HEIGHT) &&
        is_valid_number(p$age, MIN_AGE, MAX_AGE), "Enter a weight, height and age in range."))
    d <- dosesNow()
    shiny::validate(shiny::need(nrow(d) > 0 && any(d$Dose > 0), "Enter a dose."))
    drug <- paperReviewDrug(r$extraction, p, r$identifiability$closed)
    shiny::validate(shiny::need(drug$ok, paste(drug$problems, collapse = " ")))
    maximum <- max(input$maximum %or% 240, max(d$Time) + 1)
    # Run on far enough to cover an overlaid figure's points.
    fig <- Filter(function(f) identical(f$id, input$overlay), r$figures)
    if (length(fig)) maximum <- max(maximum, max(fig[[1]]$points$time, na.rm = TRUE) * 1.05)
    sim <- tryCatch(paperReviewSimulate(drug, p, d, maximum), error = function(e) e)
    shiny::validate(shiny::need(!inherits(sim, "error"),
                                if (inherits(sim, "error")) conditionMessage(sim) else ""))
    list(drug = drug, sim = sim)
  })

  output$simulatorNote <- shiny::renderUI({
    r <- review()
    if (is.null(r)) return(NULL)
    htmltools::div(class = "alert alert-secondary", style = "font-size: 0.9em;",
      htmltools::strong(r$drug$defaults$Drug), ": the paper's model, built in this session's ",
      "memory for the patient entered. It is not a stanpumpR library drug and was not reviewed ",
      "by anyone. Covariates the simulator does not take are held at their reference values.")
  })

  output$simPlot <- shiny::renderPlot({
    s <- simulation()
    curve <- s$sim$curve
    Time <- Concentration <- Site <- time <- observed <- NULL
    long <- rbind(data.frame(Time = curve$Time, Concentration = curve$Cp, Site = "Plasma"),
                  if (any(is.finite(curve$Ce)) && any(curve$Ce > 0))
                    data.frame(Time = curve$Time, Concentration = curve$Ce, Site = "Effect site"))
    if (isTRUE(input$logY)) long <- long[long$Concentration > 0, ]
    # stanpumpR's convention: plasma solid, effect site dashed.
    p <- ggplot2::ggplot(long, ggplot2::aes(Time, Concentration, linetype = Site)) +
      ggplot2::geom_line(colour = "#1f4e9c", linewidth = 1) +
      ggplot2::scale_linetype_manual(values = c(Plasma = "solid", `Effect site` = "dashed")) +
      ggplot2::labs(x = "Time (minutes)", linetype = NULL,
                    y = paste0("Concentration (", s$drug$defaults$Concentration.Units, "/mL)"),
                    title = s$drug$defaults$Drug) +
      ggplot2::theme_bw(base_size = 14)
    r <- review()
    if (nzchar(input$overlay %or% "")) {
      fig <- Filter(function(f) identical(f$id, input$overlay), r$figures)
      if (length(fig)) p <- p + ggplot2::geom_point(data = fig[[1]]$points,
        ggplot2::aes(time, observed), inherit.aes = FALSE, size = 2.5)
    }
    if (isTRUE(input$logY)) p <- p + ggplot2::scale_y_log10()
    p
  })

  output$simParameters <- shiny::renderTable({
    s <- simulation()
    pk <- s$sim$pk$PK$default
    lambda <- c(pk$lambda_1, pk$lambda_2, pk$lambda_3)
    lambda <- lambda[lambda > 0]
    # Only the compartments the model has: an absent one carries a
    # placeholder volume with no clearance into it.
    has <- c(TRUE, isTRUE(pk$cl2 > 0), isTRUE(pk$cl3 > 0))
    t <- data.frame(
      Quantity = c(c("V1 (L)", "V2 (L)", "V3 (L)")[has], "CL (L/min)",
                   c("Q2 (L/min)", "Q3 (L/min)")[has[2:3]],
                   paste0("Half-life ", seq_along(lambda), " (min)"), "ke0 (1/min)"),
      Value = signif(c(c(pk$v1, pk$v2, pk$v3)[has], pk$cl1, c(pk$cl2, pk$cl3)[has[2:3]],
                       log(2) / lambda, if (isTRUE(pk$ke0 > 0)) pk$ke0 else NA), 4))
    t[!is.na(t$Value), ]
  }, caption = "The model for this patient", digits = 4)
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
#' key is read from the \code{ANTHROPIC_API_KEY} environment variable or
#' \code{anthropic_api_key} in the config file.  See
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
  shiny::shinyApp(paperReviewUI(), paperReviewServer, enableBookmarking = "disable")
}
