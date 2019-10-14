outputComments <- function(text, commentsLog)
{
  commentsLog <<- paste0(commentsLog, "\n", text)
  output$plotInfo <- renderText(commentsLog)
}
