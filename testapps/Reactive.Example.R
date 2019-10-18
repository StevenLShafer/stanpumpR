library(shiny)



library(shiny)

ui <- fluidPage(
  textInput("a",""),
  textInput("z", ""),
  textOutput("b")
)
server <-
  function(input,output){
     rv <- reactiveValues()
     rv$number <- 5

    re <- reactive({
       cat("In re\n")
      cat("rv$number = ",rv$number,"\n")
      if (rv$number < 20) rv$number <- rv$number + 1
      paste(input$a,input$z)
        })

      output$b <- renderText({
        re()
      })
  }

shinyApp(ui, server)
