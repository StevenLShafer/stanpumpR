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
    header = htmltools::tagList(
      shinyjs::useShinyjs(),
      # A disabled upload field looks disabled (shinyjs disables the input,
      # which leaves its Browse button looking live).
      htmltools::tags$style(htmltools::HTML(
        ".form-group:has(input[type=file]:disabled) { opacity: 0.45; pointer-events: none; }")),
      # Shown while Claude reads the paper.  A plain banner, shown and hidden
      # at once: Shiny's progress notification fades out with a jQuery
      # animation, which stalled in the desktop app's browser pane and left
      # the box flashing over the report after the review had finished.
      shinyjs::hidden(htmltools::div(
        id = "reviewBusy", class = "alert alert-info", role = "status",
        style = "position: fixed; top: 64px; left: 50%; transform: translateX(-50%); z-index: 2000; box-shadow: 0 2px 8px rgba(0,0,0,0.2);",
        htmltools::strong("Claude is reading the paper."),
        " This takes a few minutes, and the app does not respond until it finishes."))),
    bslib::nav_panel("Review", value = "review",
      bslib::layout_sidebar(fillable = FALSE,
        sidebar = bslib::sidebar(width = 320,
          # The confirmation comes first; until it is ticked the upload and
          # Review controls are greyed out (Shafer: unticked, the old order
          # let a paper be chosen and then refused, which confused).
          shiny::checkboxInput("consent", paste(
            "I may share this paper with Anthropic, as the Privacy tab describes,",
            "to have it reviewed."), FALSE),
          shiny::fileInput("manuscript", "Manuscript (PDF)", accept = ".pdf"),
          shiny::fileInput("supplement", "Supplement (PDF, optional)", accept = ".pdf"),
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
          shinyjs::hidden(htmltools::div(id = "modelBox",
            shiny::selectInput("model", "Model", character(0)))),
          shiny::numericInput("weight", "Weight (kg)", 70, min = MIN_WEIGHT, max = MAX_WEIGHT),
          shiny::numericInput("height", "Height (cm)", 170, min = MIN_HEIGHT, max = MAX_HEIGHT),
          shiny::numericInput("age", "Age (years)", 40, min = MIN_AGE, max = MAX_AGE),
          shiny::selectInput("sex", "Sex", SEX_VALUES),
          shiny::selectInput("timeUnit", "Time units", PAPER_REVIEW_SIM_TIME_UNITS, "min"),
          shiny::numericInput("maximum", "Max time", 240, min = 0),
          shiny::selectInput("regimen", "Paper's regimen", character(0)),
          shiny::actionButton("loadRegimen", "Load regimen into the dose table"),
          shiny::selectInput("overlay", "Overlay a figure's points", character(0)),
          shiny::checkboxInput("logY", "Log concentration axis", FALSE)
        ),
        shiny::uiOutput("simulatorNote"),
        shiny::plotOutput("simPlot", height = "460px"),
        shiny::h5("Dose table"),
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
  # A dropped connection (a hidden pane, a sleeping laptop) may rejoin the
  # same session rather than leave an upload with nowhere to go.
  session$allowReconnect(TRUE)
  shiny::observe({
    consent <- isTRUE(input$consent)
    shinyjs::toggleState("manuscript", condition = consent)
    shinyjs::toggleState("supplement", condition = consent)
    shinyjs::toggleState("review", condition = hasPdf() && consent)
    # Discard is live only when there is something to discard.
    shinyjs::toggleState("discard", condition = hasPdf() || !is.null(review()))
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
    # Custom messages go to the browser at once, before the call blocks.
    shinyjs::show("reviewBusy")
    shinyjs::disable("review")
    on.exit(shinyjs::hide("reviewBusy"), add = TRUE)
    result <- tryCatch(
      paperReviewExtract(store$pdf, if (exists("supplement", envir = store)) store$supplement,
                         key = key,
                         inferenceGeo = paperReviewConfig("paper_review_inference_geo")),
      paper_review_error = function(e) e,
      error = function(e) paperReviewError("unexpected", "The review failed unexpectedly."))
    rm(key)
    # The manuscript is not needed again: drop it whatever the outcome.
    suppressWarnings(rm(list = intersect(c("pdf", "supplement"), ls(store)), envir = store))
    hasPdf(FALSE)
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
    setUnit(paperReviewRegimenTimeUnit(r$extraction, first), 240)
  }, ignoreInit = FALSE)

  # The user picked another unit: convert what is shown.
  shiny::observeEvent(input$timeUnit, {
    if (identical(input$timeUnit, tableUnit())) return()
    current <- dosesNow()
    maximumMinutes <- (input$maximum %or% 240) * unitMinutes()
    doseTable(current)
    setUnit(input$timeUnit, maximumMinutes)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$loadRegimen, {
    r <- currentReview(); if (is.null(r)) return()
    regimen <- paperReviewRegimen(r$extraction, input$regimen)
    if (is.null(regimen)) return()
    d <- paperReviewRegimenDoses(regimen)$doses
    if (nrow(d) == 0) return()
    doseTable(d)
    # In the unit the paper uses for this regimen.
    setUnit(paperReviewRegimenTimeUnit(r$extraction, regimen), max(240, ceiling(max(d$Time) * 2)))
  })

  output$doses <- rhandsontable::renderRHandsontable({
    r <- currentReview()
    units <- if (is.null(r)) "mg" else r$drug$units
    d <- doseTable()
    d$Units <- as.character(d$Units)
    d$Time <- signif(d$Time / unitMinutes(), 6)
    blank <- data.frame(Time = rep(NA_real_, 4), Dose = rep(NA_real_, 4), Units = rep(NA_character_, 4))
    rhandsontable::hot_col(
      rhandsontable::rhandsontable(rbind(d, blank), rowHeaders = NULL, stretchH = "all",
                                   width = 520, height = 230,
                                   colHeaders = c(paste0("Time (", unitName(), ")"), "Dose", "Units")),
      "Units", type = "dropdown", source = units)
  })

  dosesNow <- shiny::reactive({
    # Minutes, whatever the table shows.
    shown <- !is.null(input$doses)
    d <- if (shown) rhandsontable::hot_to_r(input$doses) else doseTable()
    names(d)[1:3] <- c("Time", "Dose", "Units")
    d <- d[is.finite(suppressWarnings(as.numeric(d$Time))) &
             is.finite(suppressWarnings(as.numeric(d$Dose))) & !is.na(d$Units) & nzchar(d$Units), ,
           drop = FALSE]
    d$Time <- as.numeric(d$Time) * if (shown) unitMinutes() else 1
    d$Dose <- as.numeric(d$Dose)
    d[order(d$Time), , drop = FALSE]
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
    shiny::validate(shiny::need(nrow(d) > 0 && any(d$Dose > 0), "Enter a dose."))
    drug <- paperReviewDrug(r$extraction, p, r$identifiability$closed,
                            absorption = simAbsorption(), absorptions = r$absorptions)
    shiny::validate(shiny::need(drug$ok, paste(drug$problems, collapse = " ")))
    maximum <- max((input$maximum %or% 240) * unitMinutes(), max(d$Time) + 1)
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
