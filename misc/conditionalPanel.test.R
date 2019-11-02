library(shiny)
library(shinyjs)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  useShinyjs(),
  checkboxGroupInput(inputId = "bins",
    label = "Number of bins:",
    choices = c("Hello","Fun","Is","Now")
  ),
  checkboxInput(
    inputId = "Test",
    label = "Test checkBox",
    value = FALSE
  ),
# Main panel for displaying outputs ----
  textOutput(outputId = "textPlot")
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  output$textPlot <- renderText({paste(input$bins)})
  observe(
    {
      cat(input$bins, "\n")
      cat(grepl("Is", input$bins), "\n")
    if (sum(grepl("Is",input$bins)) > 0)
    {
      cat("turning off Test\n")
      hideElement("Test")
    } else {
      cat("turning on Test\n")
      showElement("Test")
    }
    }
  )
}

shinyApp(ui = ui, server = server)

