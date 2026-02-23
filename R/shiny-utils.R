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
