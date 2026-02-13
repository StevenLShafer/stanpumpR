attachClass <- function(tag, class) {
  htmltools::tagAppendAttributes(tag, class = class)
}

inlineUI <- function(tag) {
  attachClass(tag, "inline_ui")
}
