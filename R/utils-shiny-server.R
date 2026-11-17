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

