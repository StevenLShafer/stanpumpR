# Extra functions
#' @name inumericInput
#' @title Create a numeric input with a label either left, above, or both left and above the input. With no label it becomes an 'inline' input suitable for embedding in a table.
#' @param inputId The DOM ID.
#' @param label The label to display to the left of the input.
#' @param label2 The optional label to display to the above of the input.
#' @param value The initial value for the input.
#' @param min The minimum acceptable value for the input.
#' @param max The maximum acceptable value for the input.
#' @param step The step increment value for the input.
#' @param label.style Inline css to apply to the left-of-input label.
#' @param input.style Inline css to apply to the main input tag.
#' @param container.style Inline css to apply to the outer container.
#' @param container A named list of containers to apply. May length one or two, with names in c('outer', 'inner'); if length one the value will be applied to the outer container, and the inner will be a span by default. Should be either a block container 'div' or an inline container 'span'.
#' @export
#' @author Jon Katz
#' @examples
#' inumericInput(inputId='test', label='test label', value=1)
#' @keywords misc


inumericInput <- function (inputId, label, label2=NULL, value, min = NA, max = NA, step = NA,
                           label.style='float:left;padding:0.5em 1em 0.5em 0em;', input.style='width:75%;border-radius:4px;', container.style="margin-bottom:0px;", container=list(outer=shiny::div, inner=shiny::span))
{
  inputTag <- shiny::tags$input(id = inputId, type = "number", class = "form-control", style=input.style, value = shiny:::formatNoSci(value))
  if (!is.na(min))
    inputTag$attribs$min = min
  if (!is.na(max))
    inputTag$attribs$max = max
  if (!is.na(step))
    inputTag$attribs$step = step

  if(length(container) == 1) {
    container <- c(container, list(inner=shiny::span))
    names(container)[1] <- 'outer'
  }

  container[['outer']](class = "form-group shiny-input-container", style=container.style,
                       if(!is.null(label2)) shiny::tags$label(label2),
                       container[['inner']](
                         if(label != '') shiny::tags$label(style=
                                                             label.style, label, `for` = inputId
                         ),
                         inputTag
                       )
  )
}

#' @name itextInput
#' @title Create a text input with a label either left, above, or both left and above the input. With no label it becomes an 'inline' input suitable for embedding in a table.
#' @param inputId The DOM ID.
#' @param label The label to display to the left of the input.
#' @param label2 The optional label to display to the above of the input.
#' @param value The initial value for the input.
#' @param placeholder The html placeholder.
#' @param label.style Inline css to apply to the left-of-input label.
#' @param input.style Inline css to apply to the main input tag.
#' @param container.style Inline css to apply to the outer container.
#' @param container A named list of containers to apply. May length one or two, with names in c('outer', 'inner'); if length one the value will be applied to the outer container, and the inner will be a span by default. Should be either a block container 'div' or an inline container 'span'.
#' @export
#' @author Jon Katz
#' @examples
#' itextInput(inputId='test', label='test label', value=1)
#' @keywords misc


itextInput <- function (inputId, label, label2=NULL, value = "", width = NULL, placeholder = NULL, label.style='float:left;padding:0.5em 1em 0.5em 0em;', input.style='width:75%;border-radius:4px;', container.style="margin-bottom:0px;", container=list(outer=shiny::div, inner=shiny::span))
{
  container[['outer']](class = "form-group shiny-input-container", style=container.style,
                       if(!is.null(label2)) shiny::tags$label(label2),
                       container[['inner']](
                         if(label != '') shiny::tags$label(style=
                                                             label.style, label, `for` = inputId
                         ),
                         shiny::tags$input(
                           style = input.style,
                           id = inputId,
                           type = "text", class = "form-control", value = value,
                           placeholder = placeholder
                         )
                       )
  )
}


#' @name iradioButtons
#' @title Create a radio button input with a label to the left of the input. With no label it becomes an 'inline' input suitable for embedding in a table.
#' @param inputId The DOM ID.
#' @param label The label to display to the left of the input.
#' @param choices Named or unnamed character vector of selectable choices.
#' @param selected The initial value for the input.
#' @param inline Place options inline (TRUE) or stacked (FALSE).
#' @param label.style Inline css to apply to the left-of-input label.
#' @param input.style Inline css to apply to the main input tag.
#' @param container.style Inline css to apply to the outer container.
#' @param container A named list of containers to apply. May length one or two, with names in c('outer', 'inner'); if length one the value will be applied to the outer container, and the inner will be a span by default. Should be either a block container 'div' or an inline container 'span'.
#' @export
#' @author Jon Katz
#' @examples
#' iradioButtons(inputId='test', label='test label', value=1)
#' @keywords misc

iradioButtons <- function (inputId, label, choices, selected = NULL, inline = FALSE,
                           width = NULL, padding='0.5em 1em 0.5em 0em', thin=FALSE, container=shiny::div)
{
  choices <- shiny:::choicesWithNames(choices)
  selected <- if (is.null(selected))
    choices[[1]]
  else {
    shiny:::validateSelected(selected, choices, inputId)
  }
  if (length(selected) > 1)
    stop("The 'selected' argument must be of length 1")
  options <- shiny:::generateOptions(inputId, choices, selected, inline,
                                     type = "radio")
  divClass <- "form-group shiny-input-radiogroup shiny-input-container"
  if (inline)
    divClass <- paste(divClass, "shiny-input-container-inline")
  divStyle <- NULL
  if (!is.null(width))
    divStyle <- paste0("width: ", shiny::validateCssUnit(width), ";")
  if(thin)
    divStyle <- paste0(divStyle, "margin-bottom:0px;")

  container(id = inputId, style = divStyle, class = divClass,
            shiny::tags$span(style=paste0("padding:", padding, ";"),
                             if(label != '') label, options)
  )
}
