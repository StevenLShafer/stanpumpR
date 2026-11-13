# Single Paper Review: the review report, as HTML.
#
# Built with htmltools from the review (paperReviewRun()) and nothing else.
# The same tags are shown in the app and, when downloads are allowed, written
# into a self-contained page; plots are embedded as base64 PNGs, rendered to a
# temporary file that is read and deleted at once (as Shiny's own renderPlot()
# does).  (Claude Code, 2026-10-30, at the request of Steven L. Shafer.)

PAPER_REVIEW_SEVERITY_ORDER <- c("error", "concern", "info", "ok")
PAPER_REVIEW_SEVERITY_LABEL <- c(error = "Error", concern = "Concern", info = "Note", ok = "Checks out")
PAPER_REVIEW_SECTIONS <- c("Identifiability", "Model", "Internal consistency", "Covariates",
                           "Assumptions and the library's rules", "Figures", "Claude's reading")

paperReviewCss <- function()
  htmltools::tags$style(htmltools::HTML("
.pr-report { max-width: 980px; }
.pr-finding { border-left: 4px solid #999; padding: 6px 10px; margin: 6px 0; background: #fafafa; }
.pr-finding.pr-error { border-color: #b3261e; } .pr-finding.pr-concern { border-color: #c77700; }
.pr-finding.pr-info { border-color: #1f4e9c; } .pr-finding.pr-ok { border-color: #2e7d32; }
.pr-badge { display: inline-block; font-size: 0.75em; padding: 1px 6px; border-radius: 3px; color: #fff; margin-right: 6px; }
.pr-badge.pr-error { background: #b3261e; } .pr-badge.pr-concern { background: #c77700; }
.pr-badge.pr-info { background: #1f4e9c; } .pr-badge.pr-ok { background: #2e7d32; }
.pr-source { font-size: 0.8em; color: #555; }
.pr-where { font-size: 0.85em; color: #333; }
.pr-table { border-collapse: collapse; font-size: 0.85em; margin: 8px 0; }
.pr-table th, .pr-table td { border: 1px solid #ccc; padding: 3px 6px; text-align: left; vertical-align: top; }
.pr-banner { background: #fff4e5; border: 1px solid #c77700; padding: 8px 12px; margin-bottom: 12px; }
"))

#' A data frame as an HTML table
#' @keywords internal
paperReviewTable <- function(df)
{
  if (is.null(df) || nrow(df) == 0) return(htmltools::tags$p(htmltools::tags$em("None.")))
  htmltools::tags$table(class = "pr-table",
    htmltools::tags$thead(htmltools::tags$tr(lapply(names(df), htmltools::tags$th))),
    htmltools::tags$tbody(lapply(seq_len(nrow(df)), function(i)
      htmltools::tags$tr(lapply(df[i, ], function(x) htmltools::tags$td(as.character(x)))))))
}

paperReviewFindingTag <- function(f)
{
  htmltools::div(class = paste0("pr-finding pr-", f$severity),
    htmltools::span(class = paste0("pr-badge pr-", f$severity), PAPER_REVIEW_SEVERITY_LABEL[[f$severity]]),
    htmltools::strong(f$title),
    if (nzchar(f$detail)) htmltools::div(f$detail),
    if (length(f$locations)) htmltools::div(class = "pr-where",
      htmltools::em("Where in the paper: "), paste(f$locations, collapse = "; ")),
    htmltools::div(class = "pr-source",
      if (isTRUE(f$computed)) "Computed in stanpumpR from the extracted numbers."
      else "Claude's reading of the paper, with stanpumpR's rule applied; not computed."))
}

#' The extracted model as tables, so the reader can check the extraction
#' @keywords internal
paperReviewExtractionTables <- function(extraction)
{
  unc <- function(u) {
    if (is.null(u) || (u$type %or% "not_reported") %in% c("not_reported")) return("")
    vals <- c(u$value, u$lower, u$upper)
    paste(u$type, paste(unlist(vals), collapse = " to "))
  }
  params <- extraction$parameters %or% list()
  pt <- if (length(params)) data.frame(
    Parameter = vapply(params, function(p) p$name_in_paper %or% "", character(1)),
    Model = vapply(params, function(p) p$group %or% "", character(1)),
    Role = vapply(params, function(p) p$role %or% "", character(1)),
    Printed = vapply(params, function(p) p$value_text %or% "", character(1)),
    Units = vapply(params, function(p) p$units %or% "", character(1)),
    Uncertainty = vapply(params, function(p) unc(p$uncertainty), character(1)),
    Status = vapply(params, function(p) p$status %or% "", character(1)),
    Where = vapply(params, function(p) p$location %or% "", character(1)),
    stringsAsFactors = FALSE) else NULL
  if (!is.null(pt) && !any(nzchar(pt$Model))) pt$Model <- NULL
  covs <- extraction$covariate_effects %or% list()
  ct <- if (length(covs)) data.frame(
    Parameter = vapply(covs, function(e) e$parameter_role %or% "", character(1)),
    Covariate = vapply(covs, function(e) e$covariate %or% "", character(1)),
    Form = vapply(covs, function(e) e$form %or% "", character(1)),
    Coefficient = vapply(covs, function(e) as.character(e$coefficient %or% ""), character(1)),
    Reference = vapply(covs, function(e) as.character(e$reference_value %or% ""), character(1)),
    `As printed` = vapply(covs, function(e) e$equation_as_printed %or% "", character(1)),
    Where = vapply(covs, function(e) e$location %or% "", character(1)),
    check.names = FALSE, stringsAsFactors = FALSE) else NULL
  regs <- extraction$regimens %or% list()
  rt <- if (length(regs)) data.frame(
    Id = vapply(regs, function(r) r$id %or% "", character(1)),
    Regimen = vapply(regs, function(r) r$description %or% "", character(1)),
    Where = vapply(regs, function(r) r$location %or% "", character(1)),
    stringsAsFactors = FALSE) else NULL
  htmltools::tagList(
    htmltools::h4("Parameters"), paperReviewTable(pt),
    htmltools::h4("Covariate effects"), paperReviewTable(ct),
    htmltools::h4("Regimens"), paperReviewTable(rt))
}

#' A readable time unit for a plot running to `maximum` minutes
#' @returns a list: \code{minutes} per unit, and its \code{label}
#' @keywords internal
paperReviewTimeAxis <- function(maximum)
{
  if (!is.finite(maximum) || maximum <= 6 * 60) list(minutes = 1, label = "minutes")
  else if (maximum <= 4 * 1440) list(minutes = 60, label = "hours")
  else if (maximum <= 120 * 1440) list(minutes = 1440, label = "days")
  else list(minutes = 7 * 1440, label = "weeks")
}

#' A figure's digitised points over the model's curve
#'
#' @param figure one entry of the review's \code{figures}
#' @param concUnits the model's concentration units
#' @returns a ggplot
#' @keywords internal
paperReviewFigurePlot <- function(figure, concUnits)
{
  # Columns, named for R CMD check; the data mask takes precedence.
  Time <- Cp <- time <- observed <- series <- lower <- upper <- regimen <- NULL
  curve <- figure$curve[figure$curve$Cp > 0 | !figure$logY, ]
  axis <- paperReviewTimeAxis(max(c(curve$Time, figure$points$time), na.rm = TRUE))
  curve$Time <- curve$Time / axis$minutes
  figure$points$time <- figure$points$time / axis$minutes
  if (identical(figure$kind, "decrement")) {
    # Both axes are times: the decrement time in the same unit as the duration.
    curve$Cp <- curve$Cp / axis$minutes
    figure$points$observed <- figure$points$observed / axis$minutes
  }
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(data = curve, ggplot2::aes(x = Time, y = Cp, group = regimen),
                       colour = "#1f4e9c", linewidth = 1) +
    ggplot2::geom_point(data = figure$points,
                        ggplot2::aes(x = time, y = observed, shape = series),
                        size = 2.5) +
    ggplot2::labs(x = paste0(figure$xLabel %or% "Time", " (", axis$label, ")"),
                  y = if (identical(figure$kind, "decrement"))
                        paste0(figure$yLabel, " (", axis$label, ")")
                      else if (identical(figure$kind, "normalized")) paste0(figure$yLabel, " (%)")
                      else paste0("Concentration (", concUnits, "/mL)"),
                  title = figure$id, subtitle = paste("Line: stanpumpR, reference patient.",
                                                     "Points: digitised approximately from the paper."),
                  shape = NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
  bars <- figure$points[is.finite(figure$points$lower) & is.finite(figure$points$upper), ]
  if (nrow(bars))
    p <- p + ggplot2::geom_errorbar(data = bars,
      ggplot2::aes(x = time, ymin = lower, ymax = upper), width = 0)
  if (figure$logY) p <- p + ggplot2::scale_y_log10()
  p
}

# A ggplot as an <img> with its PNG inlined.  The temporary file is deleted
# before this returns.
paperReviewPlotImg <- function(plot, width = 7, height = 4.5)
{
  file <- tempfile(fileext = ".png")
  on.exit(unlink(file), add = TRUE)
  grDevices::png(file, width = width, height = height, units = "in", res = 110)
  tryCatch(print(plot), finally = grDevices::dev.off())
  bytes <- readBin(file, "raw", file.info(file)$size)
  htmltools::tags$img(src = paste0("data:image/png;base64,", jsonlite::base64_enc(bytes)),
                      style = "max-width: 100%;")
}

#' The review report
#'
#' @param review the output of \code{paperReviewRun()}
#' @param plots include the figure plots (as inline PNGs)?
#' @returns an htmltools tag list
#' @keywords internal
paperReviewReportTags <- function(review, plots = TRUE)
{
  x <- review$extraction
  paper <- x$paper %or% list()
  cite <- paste(c(paper$first_author, if (!is.null(paper$year)) paste0("(", paper$year, ")"),
                  paper$title, paper$journal, paper$doi), collapse = ". ")
  # A quick review says so, whatever it found (CodeRabbit, PR #720: a quick
  # review that missed the model is where the warning matters most).
  quickBanner <- if (isTRUE(review$quick)) htmltools::div(class = "pr-banner",
    htmltools::strong("A quick review. "),
    "The full review was taking longer than usual, so Claude read the paper again at a lower effort. ",
    "A quick review reads a paper with one model as fully, but may leave out secondary models (other ",
    "populations, drugs or endpoints) and some of a per-patient table's detail.")
  # No model at all (a document that is not a pharmacokinetic paper): one
  # line, and Claude's own notes, not a review built around nothing ("0
  # compartments", "Drug: (, , other)"; Devin's stress test, 2026-11-08).
  if (paperReviewNoModel(x))
    return(htmltools::div(class = "pr-report", paperReviewCss(),
      quickBanner,
      htmltools::h2("Single Paper Review"),
      if (nzchar(cite)) htmltools::p(htmltools::strong("Document: "), cite),
      htmltools::p(htmltools::strong(
        "Claude found no pharmacokinetic model in this document, so there is nothing to check.")),
      lapply(paperReviewClaudeNotes(x), paperReviewFindingTag),
      if (nzchar(x$extraction_limitations %or% ""))
        htmltools::p(htmltools::em("Limitations Claude noted: "), x$extraction_limitations)))
  findings <- review$findings
  counts <- table(factor(vapply(findings, function(f) f$severity, character(1)),
                         levels = PAPER_REVIEW_SEVERITY_ORDER))
  models <- review$models
  body <- if (is.null(models)) {
    paperReviewModelTags(review, PAPER_REVIEW_SECTIONS, findings, plots)
  } else {
    # Several models: each one's own review, then what is about the paper.
    htmltools::tagList(
      htmltools::p(paste0("This paper reports ", length(models), " models, each reviewed on its own: ",
                          paste(names(models), collapse = "; "), ".")),
      lapply(names(models), function(g) htmltools::tagList(
        htmltools::h2(g),
        paperReviewModelTags(models[[g]], PAPER_REVIEW_MODEL_SECTIONS, models[[g]]$findings, plots))),
      htmltools::h2("The paper as a whole"),
      paperReviewSectionTags(review, setdiff(PAPER_REVIEW_SECTIONS, PAPER_REVIEW_MODEL_SECTIONS),
                             review$paperFindings, plots = FALSE))
  }
  htmltools::div(class = "pr-report", paperReviewCss(),
    htmltools::div(class = "pr-banner",
      htmltools::strong("Draft review, for the reader's judgment. "),
      "The numbers were read from the paper by Claude and may be misread; every finding says where in ",
      "the paper it comes from, so check it there. Computed findings are only as good as the extraction."),
    quickBanner,
    htmltools::h2("Single Paper Review"),
    htmltools::p(htmltools::strong("Paper: "), cite),
    htmltools::p(htmltools::strong("Drug: "),
                 paste0(x$drug$name, " (", x$drug$analyte, ", ", x$drug$matrix, ", ",
                        x$drug$concentration_units, ")")),
    htmltools::p(htmltools::strong("Findings: "),
                 paste(paste(counts, tolower(PAPER_REVIEW_SEVERITY_LABEL[names(counts)])),
                       collapse = ", ")),
    body,
    htmltools::h3("What Claude extracted"),
    htmltools::p("Check these against the paper before trusting anything above."),
    paperReviewExtractionTables(x),
    if (nzchar(x$extraction_limitations %or% ""))
      htmltools::p(htmltools::em("Limitations Claude noted: "), x$extraction_limitations))
}

#' Does the extraction hold no model: no parameter and no structure of one
#' to three compartments?
#' @keywords internal
paperReviewNoModel <- function(x)
{
  n <- paperReviewNum(x$structure$compartments)
  length(x$parameters %or% list()) == 0 && !isTRUE(n %in% 1:3)
}

# The sections that belong to one model; the rest are about the paper.
PAPER_REVIEW_MODEL_SECTIONS <- c("Identifiability", "Model", "Internal consistency", "Covariates", "Figures")

#' One model's summary lines and sections
#' @keywords internal
paperReviewModelTags <- function(rv, sections, findings, plots)
{
  x <- rv$extraction
  ref <- rv$reference
  ident <- rv$identifiability
  verdictText <- c(printed = "The printed parameters close the model.",
                   derived = paste("The model is closed through", ident$route, "and is stable."),
                   ill_conditioned = paste("Formally closed through", ident$route, "but ill-conditioned."),
                   derived_from_curve = paste("The printed numbers do not close the model; it is closed",
                                              "from", ident$route, "."),
                   underdetermined = "The paper does not print enough to close the model.")[[ident$verdict]]
  htmltools::tagList(
    htmltools::p(htmltools::strong("Model: "),
                 paste0(x$structure$compartments, " compartment(s), ",
                        gsub("_", " ", x$structure$elimination), " elimination, absorption ",
                        gsub("_", " ", x$structure$absorption), ", effect site ",
                        gsub("_", " ", x$structure$effect_site), ". ", verdictText)),
    htmltools::p(htmltools::strong("Reference patient: "),
                 paste0(prFmt(ref$weight), " kg (", ref$source[["weight"]], "), ",
                        prFmt(ref$height), " cm (", ref$source[["height"]], "), ",
                        prFmt(ref$age), " years (", ref$source[["age"]], "), ", ref$sex,
                        " (", ref$source[["sex"]], ").")),
    paperReviewSectionTags(rv, sections, findings, plots))
}

#' Findings by section, with the consistency table and the figures' plots
#' @keywords internal
paperReviewSectionTags <- function(rv, sections, findings, plots)
{
  lapply(sections, function(s) {
    fs <- Filter(function(f) identical(f$section, s), findings)
    if (length(fs) == 0) return(NULL)
    fs <- fs[order(match(vapply(fs, function(f) f$severity, character(1)), PAPER_REVIEW_SEVERITY_ORDER))]
    htmltools::tagList(htmltools::h3(s),
      if (s == "Internal consistency") paperReviewTable(rv$consistency),
      lapply(fs, paperReviewFindingTag),
      if (s == "Identifiability" && !is.null(rv$pkTable)) htmltools::tagList(
        htmltools::h4("Every parameter of the model, at the reference patient"),
        paperReviewTable(rv$pkTable)),
      if (s == "Figures" && plots) lapply(rv$figures, function(f)
        paperReviewPlotImg(paperReviewFigurePlot(f, rv$drug$defaults$Concentration.Units))))
  })
}

#' The report as a self-contained HTML page, for download
#' @keywords internal
paperReviewReportPage <- function(review)
{
  paste0("<!DOCTYPE html>
", as.character(htmltools::tags$html(
    htmltools::tags$head(htmltools::tags$meta(charset = "utf-8"),
                         htmltools::tags$title("Single Paper Review")),
    htmltools::tags$body(style = "font-family: sans-serif; margin: 24px;",
                         paperReviewReportTags(review, plots = TRUE)))))
}
