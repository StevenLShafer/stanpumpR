attachClass <- function(tag, class) {
  htmltools::tagAppendAttributes(tag, class = class)
}

inlineUI <- function(tag) {
  attachClass(tag, "inline_ui")
}

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
  input <- tag$children[[2]]  # WARNING in the future, if shiny changes internals, this can break
  input <- htmltools::tagAppendAttributes(input, class = "main-input")

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

  tag
}

#' Create an input that has its label on its left, and the input takes
#' the rest of the space
inputWithInlineLabel <- function(tag) {
  css <- "
    .input-inline-label {
      display: flex;
      align-items: center;
      gap: 1rem;
      margin-bottom: 1rem;
    }
    .input-inline-label .form-group {
      margin-bottom: 0;
    }
    .input-inline-label .main-input {
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
    div(tag, class = "main-input")
  )
  htmltools::attachDependencies(tag, dep)
}
