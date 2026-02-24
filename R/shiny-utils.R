attachClass <- function(tag, class) {
  htmltools::tagAppendAttributes(tag, class = class)
}

inlineUI <- function(tag) {
  attachClass(tag, "inline_ui")
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
