makeReactiveTrigger <- function() {
  rv <- shiny::reactiveValues(a = 0)
  list(
    depend = function() {
      rv$a
      invisible()
    },
    trigger = function() {
      rv$a <- shiny::isolate(rv$a + 1)
    }
  )
}

# When the given element is inside a modal, make sure its first input gets focus
# when the modal opens
modalFocus <- function(tag) {
  if (tag$name == "input") {
    htmltools::tagAppendAttributes(tag, class = "modal-focusme")
  } else {
    htmltools::tagQuery(tag)$find("input")$addClass("modal-focusme")$allTags()
  }
}

# Add hooks to a handsontable. This is not strictly a generic utility function
# because it does use names of specific hooks used in this package, but otherwise
# it's a fairly generic function
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
