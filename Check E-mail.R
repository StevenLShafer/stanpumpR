library(shiny)
library(shinyjs)

#var regex = /^\\w+([-+.']\\w+)*@\\w+([-.]\\w+)*\\.\\w{2,}([-.]\\w+)*$/;

jscode <- HTML("
               shinyjs.validate = function() {
               var regex = /^\\w+([-+.']\\w+)*@\\w+([-.]\\w+)*\\.\\w{2,}([-.]\\w+)*$/;
               if(!regex.test($('#emailField').val())) {
               alert('not a proper e-mail format.');
               }
               }
               ")

runApp(shinyApp(
  ui = fluidPage(
    useShinyjs(),
    extendShinyjs(text = jscode),
    textInput('emailField', "Enter email",'test@test.com'),
    actionButton('okButton', 'OK')
  ),
  server = function(input, output, session) {
    observeEvent(input$okButton, {
      js$validate()
    })
  }
))
