attachClass <- function(tag, class) {
  htmltools::tagAppendAttributes(tag, class = class)
}

# The links in a citation: every https URL on one of the CITATION_WEBSITES
# hosts, wherever it stands in the string, not only at its end.  Punctuation
# that closes the sentence or a parenthesis around the URL is left to the
# text: a DOI's own parentheses ("S0140-6736(99)01234-5") are balanced and
# are kept, a closing one with no opening one inside the URL is not part of
# it.  Returns a data frame of url (as written), link (trimmed), label and
# the character positions, or NULL.  (Claude Code, 2026-10-14, at the request
# of Steven L. Shafer: a trailing ")" once made a DOI link return 404, and a
# URL followed by any text was not linked at all.)
citationLinks <- function(reference) {
  hosts <- paste(gsub(".", "\\.", CITATION_WEBSITES, fixed = TRUE), collapse = "|")
  pattern <- paste0("https://(", hosts, ")/[^[:space:]]+")
  m <- gregexpr(pattern, reference, perl = TRUE)[[1]]
  if (m[1] == -1) return(NULL)
  urls <- regmatches(reference, list(m))[[1]]
  trimmed <- vapply(urls, citationTrimURL, character(1), USE.NAMES = FALSE)
  host <- sub("^https://([^/]+)/.*$", "\\1", trimmed)
  data.frame(
    url   = urls,
    link  = trimmed,
    label = names(CITATION_WEBSITES)[match(host, CITATION_WEBSITES)],
    start = as.integer(m),
    end   = as.integer(m) + nchar(trimmed) - 1L,
    stringsAsFactors = FALSE
  )
}

citationTrimURL <- function(url) {
  repeat {
    before <- url
    url <- sub("[.,;:]+$", "", url)
    unbalanced <- function(open, close)
      lengths(regmatches(url, gregexpr(close, url, fixed = TRUE))) >
      lengths(regmatches(url, gregexpr(open, url, fixed = TRUE)))
    if (endsWith(url, ")") && unbalanced("(", ")")) url <- sub("\\)$", "", url)
    if (endsWith(url, "]") && unbalanced("[", "]")) url <- sub("\\]$", "", url)
    if (identical(url, before)) return(url)
  }
}

# A citation as HTML: the text escaped, each recognised URL replaced by a
# link labelled with its site (citationLinks()), the investigators named in
# it linked to their pop-ups.
citationHTML <- function(reference) {
  links <- citationLinks(reference)
  if (is.null(links)) return(htmltools::htmlEscape(reference))
  pieces <- character(0)
  pos <- 1L
  for (i in seq_len(nrow(links))) {
    pieces <- c(pieces, htmltools::htmlEscape(substr(reference, pos, links$start[i] - 1L)))
    pieces <- c(pieces, as.character(tags$a(
      links$label[i],
      href = links$link[i],
      target = "_blank",
      rel = "noreferrer"
    )))
    pos <- links$end[i] + 1L
  }
  pieces <- c(pieces, htmltools::htmlEscape(substr(reference, pos, nchar(reference))))
  paste(pieces, collapse = "")
}

citationItemHTML <- function(drug, reference, color) {
  text <- if (is.null(reference) || !nzchar(reference)) {
    "Not Available"
  } else {
    # The investigators named in the citation link to their pop-ups
    # (R/help-investigators.R); a citation is where the program's own
    # author is linked too.  Text inside the site links is left alone.
    HTML(helpLinkInvestigators(citationHTML(reference), "citation"))
  }

  tags$div(
    tags$strong(
      style = paste0("color:", if (is.null(color)) "inherit" else color, ";"),
      helpDrugTitle(drug)
    ),
    ": ", text
  )
}

inlineUI <- function(tag) {
  attachClass(tag, "inline_ui")
}

# Add HTML attributes to a Shiny input tag
addInputAttributes <- function(tag, ...) {
  tagClasses <- htmltools::tagGetAttribute(tag, "class")
  selector <- if (grepl("shiny-input-textarea", tagClasses)) {
    "textarea"
  } else {
    "input"
  }
  htmltools::tagQuery(tag)$
    find(selector)$
    filter(function(x, i) i == 1)$
    addAttrs(...)$
    allTags()
}

nbsp <- shiny::HTML("&nbsp;", .noWS = "outside")

#' Create an input that has choices appended to the right of it. The choices are
#' technically radio buttons under the hood, but they appear as buttons.
#'
#' @param tag The main input that that should have choices next to it
#' @param choices The choices vector, this is passed directly to shiny's `radioButtons()`
#' @param inputId The input ID to use for the resulting radio buttons. If not provided,
#' the ID will be a concatenation of the provided input's ID together with "Choice".
#' @param selected Which initial choice is selected
inputWithChoices <- function(tag, choices, inputId = NULL, selected = NULL) {
  css <- '
.input-with-choices .main-choices-input {
  left: -4px;
  border-radius: 4px !important;
}
.input-with-choices .input-group-btn .shiny-input-radiogroup .shiny-options-group {
  margin-top: 0;
}
.input-with-choices .input-group-btn .form-group {
  margin-bottom: 0;
}
.input-with-choices .input-group-btn .radio-inline {
  padding: 0;
  margin: 0;
  margin-left: -5px;
}
.input-with-choices .input-group-btn .radio-inline:first-child {
  margin-left: 0;
}

.input-with-choices .input-group-btn input[type="radio"] {
  position: absolute;
  width: 0;
  height: 0;
  opacity: 0;
}

.input-with-choices .input-group-btn input[type="radio"]:checked + span {
  background-color: #2c3e50;
  border-color: #2c3e50;
  color: #ffffff;
  box-shadow: inset 0 3px 5px rgba(0,0,0,0.125);
}

.input-with-choices .input-group-btn .radio-inline span {
  display: inline-block;
  padding: 6px 12px 5px 12px;
  background: #ffffff;
  border: 1px solid #ced4da;
  color: #495057;
  cursor: pointer;
  transition: all 0.15s ease-in-out;
}

.input-with-choices .input-group-btn .radio-inline:first-child span {
  border-top-left-radius: 4px;
  border-bottom-left-radius: 4px;
}
.input-with-choices .input-group-btn .radio-inline:last-child span {
  border-top-right-radius: 4px;
  border-bottom-right-radius: 4px;
}

.input-with-choices .input-group-btn input[type="radio"]:focus + span {
  border-color: #00264e;
  box-shadow: 0 0 0 0.2rem rgba(0, 40, 75, 0.25);
}

.input-with-choices .input-group-btn .radio-inline span:hover {
  background-color: #f4f8fb;
  border-color: #b8c2cc;
  color: #0056b3;
}

.input-with-choices .input-group-btn input[type="radio"]:checked + span:hover {
  background-color: #0069d9;
  border-color: #0062cc;
}

.input-with-choices .input-group-btn .radio-inline span:active {
  background-color: #dae0e5;
  box-shadow: inset 0 3px 5px rgba(0, 0, 0, 0.125);
}
  '

  dep <- htmltools::htmlDependency(
    name = "input-with-choices-styles",
    version = "1.0.0",
    src = c(href = ""),
    head = sprintf("<style>%s</style>", css)
  )

  input <- tag$children[[2]]  # WARNING in the future, if shiny changes internals, this can break
  input <- htmltools::tagAppendAttributes(input, class = "main-choices-input")

  if (is.null(inputId)) {
    inputId <- paste0(input$attribs$id, "Choice")
  }

  input_group <- htmltools::tags$div(
    class = "input-group input-with-choices",
    input,
    htmltools::tags$div(
      class = "input-group-btn",
      radioButtons(inputId, NULL, choices = choices, selected = selected, inline = TRUE)
    )
  )
  tag$children[[2]] <- input_group

  htmltools::attachDependencies(tag, dep)
}

#' Create a button group for selecting a line type using SVG images instead of text
#'
#' @param inputId The input slot that will be used to access the value.
#' @param label Display label for the control.
#' @param selected The initially selected line type value.
#' @return A Shiny input tag.
#' @noRd
lineTypeSelector <- function(inputId, label, selected) {

  css <- '
.line-type-icon-wrapper {
  display: flex;
}
.line-type-icon {
  width: 100%;
  height: 12px;
}
.line-type-none-icon {
  font-size: 12px;
  opacity: 0.6;
  line-height: 1;
}
  '

  line_types <- list(
    list(value = "blank",   dasharray = NA,         tooltip = "None"),
    list(value = "solid",   dasharray = "none",     tooltip = "Solid"),
    list(value = "dashed",  dasharray = "6,3",      tooltip = "Dashed"),
    list(value = "dotted",  dasharray = "0,4",      tooltip = "Dotted"),
    list(value = "dotdash", dasharray = "0,3,6,3",  tooltip = "Dot-dash")
  )

  line_type_icon <- function(entry) {
    inner <- if (is.na(entry$dasharray)) {
      htmltools::tags$i(class = "fa fa-ban line-type-none-icon")
    } else {
      htmltools::HTML(sprintf(
        '<svg class="line-type-icon" viewBox="0 0 48 8" preserveAspectRatio="none"><line x1="2" y1="4" x2="46" y2="4" stroke="currentColor" stroke-width="2" stroke-linecap="round" vector-effect="non-scaling-stroke" stroke-dasharray="%s"/></svg>',
        entry$dasharray
      ))
    }
    htmltools::tags$span(class = "line-type-icon-wrapper", title = entry$tooltip, inner)
  }

  dep <- htmltools::htmlDependency(
    name = "line-type-radio-buttons",
    version = "1.0.0",
    src = c(href = ""),
    head = sprintf("<style>%s</style>", css)
  )

  tag <- shinyWidgets::radioGroupButtons(
    inputId = inputId,
    label = label,
    choiceNames = lapply(line_types, line_type_icon),
    choiceValues = lapply(line_types, function(x) x$value),
    selected = selected,
    justified = TRUE
  )
  tag <- htmltools::tagAppendAttributes(tag, class = "line-type-button-group")
  htmltools::attachDependencies(tag, dep)
}

#' Create an input that has its label on its left, and the input takes
#' the rest of the space
#'
#' @param tag A Shiny input tag.
#' @return The input tag, restyled with an inline label.
#' @noRd
inputWithInlineLabel <- function(tag) {
  css <- "
    .input-inline-label {
      display: flex;
      align-items: center;
      gap: 1.25rem;
      margin-bottom: 1rem;
    }
    .input-inline-label .form-group {
      margin-bottom: 0;
    }
    .input-inline-label .main-inline-input {
      flex: 1;
      min-width: 0;
    }
  "
  dep <- htmltools::htmlDependency(
    name = "input-inline-label-styles",
    version = "1.0.0",
    src = c(href = ""),
    head = sprintf("<style>%s</style>", css)
  )

  tq <- htmltools::tagQuery(tag)
  first_label <- tq$find(".control-label")$selectedTags()[[1]]
  tag <- tq$find(sprintf("#%s", first_label$attribs$id))$remove()$allTags()

  # For shinyWidgets::radioGroupButtons() we need to remove the extra <br>
  if (tq$hasClass('shiny-input-radiogroup')) {
    tag <- tq$find("br")$remove()$allTags()
  }

  tag <- div(
    class = "input-inline-label",
    first_label,
    div(tag, class = "main-inline-input")
  )
  htmltools::attachDependencies(tag, dep)
}

# A bslib tooltip shown only while the pointer is over its control.  The
# default trigger is "hover focus", so ticking a box focused it and pinned
# the tip until something else was clicked (Steven L. Shafer, 2026-10-28:
# it should go as soon as the cursor moves away).
hoverTooltip <- function(trigger, ..., placement = "auto") {
  bslib::tooltip(trigger, ..., placement = placement,
                 options = list(trigger = "hover"))
}

# A Patient Profile field or Graph Options control that the server shows only
# while a drug it applies to is in the dose table (relevantFields() in
# R/patient-fields.R); its id is "field_<name>".
relevantField <- function(name, ...) {
  div(id = paste0("field_", name), class = "relevant-field", ...)
}
