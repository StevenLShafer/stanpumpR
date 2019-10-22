library(shiny)


ui <- shinyUI(fluidPage(
  ## Uncomment the following line and the first condition will evaluate TRUE
  h4(textOutput(outputId = 'SimulationMode_optionFlag')),
  conditionalPanel(condition = "output.SimulationMode_optionFlag == ''", p('This is blank...')),
  conditionalPanel(condition = "output.SimulationMode_optionFlag != ''", p('Enter your options.'))
))


server <- shinyServer(function(input, output, session) {
  output$SimulationMode_optionFlag <- renderText('')
  output$SimulationMode_optionFlag <- renderText('Graph Options')
#  outputOptions(output, "SimulationMode_optionFlag", suspendWhenHidden = FALSE)
})


shinyApp(ui = ui, server = server)
