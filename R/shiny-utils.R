attachClass <- function(tag, class) {
  htmltools::tagAppendAttributes(tag, class = class)
}

inlineUI <- function(tag) {
  attachClass(tag, "inline_ui")
}

nbsp <- shiny::HTML("&nbsp;", .noWS = "outside")

addHotHooks <- function(hot, filterKeys = TRUE, sanitize = TRUE, ...) {
  hooks <- list(...)

  if (filterKeys) hooks$beforeKeyDown <- c("hookFilterKeys", hooks$beforeKeyDown)
  if (sanitize)   hooks$beforeChange <- c("hookSanitize", hooks$beforeChange)

  js <- c("function(el, x) {", "  var hot = this.hot;")

  for (hookType in names(hooks)) {
    for (fxn in hooks[[hookType]]) {
      js <- c(js,
              sprintf("  hot.removeHook('%s', %s);", hookType, fxn),
              sprintf("  hot.addHook('%s', %s);", hookType, fxn)
      )
    }
  }

  js <- c(js, "}")
  htmlwidgets::onRender(hot, paste(js, collapse = "\n"))
}

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
.input-with-choices .input-group-btn {
  margin-top: 1px;
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

#' Create an input that has its label on its left, and the input takes
#' the rest of the space
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
