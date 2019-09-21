# UI for stanpumpR

source("global.r")

#enableBookmarking(store = "url")

# UI ------------------------------------------------------
ui <- function(request) { 
  dashboardPage(
  dashboardHeader(
    title = config$title
    # Dropdown menu for messages
    ),
  dashboardSidebar(
     collapsed = TRUE,
     width = "200px",
     sidebarMenu(
       id = "simType",
       menuItem("Basic Simulation", tabName = "SimulationMode", selected = TRUE),
       menuItem("Target Controlled Infusion", tabName = "TCIMode", selected = FALSE),
       menuItem("Time of Peak Effect", tabName = "tPeakMode", selected = FALSE),
       menuItem("Dose Calculations", tabName = "DoseMode", selected = FALSE),
       menuItem("Convert PK Parameters", tabName = "ParameterMode", selected = FALSE)
     )
   ),

  dashboardBody(
    style = "max-height: 95vh; overflow-y: auto;" ,
    tags$style(
      HTML(
        '.form-first-row {
          height: 100px;
      }
        .cancel-margin > .form-group {
        margin: 0;
        }
        '
      )
    ),
    fluidRow(
      column(
        width=4,
        HTML('<input type="text" id="client_time" name="client_time" style="display: none;"> '),
        
        tags$script('
  $(function() {
    var time_now = new Date()
    $("input#client_time").val(time_now.toLocaleTimeString())
  });    
'),
        
        h4("Patient Covariates"),
        fluidRow(  # start of fluid row within this column
          column(
            width = 6,
            div(
              class = "form-first-row",
              div(
                class = "cancel-margin",
                numericInput(
                  inputId = "age",
                  label = "Age",
                  value = defaultAge,
                  step = 1,
                  min = 1,
                  max = 110
                  ),
                bsTooltip(
                  id = "age", 
                  title = "Enter  age and select years or months",
                  placement = "top", 
                  options = list(container = "body")
                  )
                ),
              radioButtons(
              inputId = "ageUnit",
              label = NULL,
              choiceNames = c("Years","Months"),
              choiceValues = c(1,0.08333333),
              inline = TRUE,
              selected = defaultAgeUnit
            )
            ),
            radioButtons(
              inputId = "sex",
              label = "Sex",
              choiceNames = c("Woman","Man"),
              choiceValues = c("woman","man"),
              inline = TRUE,
              selected = defaultSex
            )
          ),
          column(
            width = 6,
            div(
              class = "form-first-row",
              div(
                class = "cancel-margin",
                numericInput(
              inputId = "weight",
              label = "Weight",
              value = defaultWeight,
              step = 1,
              min = 5
            ),
            bsTooltip(
              id = "weight", 
              title = "Enter weight and select kilograms or pounds",
              placement = "top", 
              options = list(container = "body")
            )
            
            ),
            radioButtons(
              inputId = "weightUnit",
              label = NULL,
              choiceNames = c("kg", "lb"),
              choiceValues = c(1,0.453592),
              inline = TRUE,
              selected = defaultWeightUnit
            )
            ),
            div(
              class = "cancel-margin",
              numericInput(
              inputId = "height",
              label = "Height",
              value = defaultHeight,
              step = 1,
              min = 20
            ),
            bsTooltip(
              id = "height", 
              title = "Enter height and select centimeters or inches",
              placement = "right", 
              options = list(container = "body")
            )
          ),
            radioButtons(
              inputId = "heightUnit",
              label = NULL,
              choiceNames = c("cms","inches"),
              choiceValues = c(1, 2.56),
              inline = TRUE,
              selected = defaultHeightUnit
            )
          )
        ), # fluidrow within column
        fluidRow(
          column(
            width=12,
            conditionalPanel(
              condition = "input.heightUnit && input.weightUnit && input.ageUnit && 
              input.height && input.weight && input.age && input.sex",
              fluidRow(
                column(
                  width = 6,
                  h4("Reference Time")
                ),
                uiOutput( # Width = 1
                  outputId = "getReferenceTime"
                )
            ), # end of fluid row
              # helpText("Enter one dose per line. Right click to add lines."),
              # .ht_clone_top table.htCore > tbody td.htAutocomplete ,
              # .ht_master table.htCore > tbody td.htAutocomplete {
              #   min-width: 70px;
              # }
              tags$style(
                HTML(
                  '.ht_master table.htCore > tbody td {
                  position: relative;
                  overflow: hidden;
                  text-overflow: ellipsis;
                  padding-right: 10px;
                  }
                  .htAutocompleteArrow {
                  position: absolute;
                  right: 0;
                  }
                  '
                )
                ),
              rHandsontableOutput(outputId = "doseTableHTML"),
              bsTooltip(
                id = "doseTableHTML", 
                title = "Enter the drug and the units by typing or by using the pull down menu. Clock times are entered as HH:MM.", 
                placement = "right", 
                options = list(container = "body")
              )
            )
          )
        ) # end of second fluid row in width 5 column
      ), # end of width 5 column
      column(
        width=8,
        fillPage(
          plotOutput(
            outputId = "PlotSimulation",
            width="100%",
            height="500px",
            click = clickOpts(
              id = "plot_click",
              clip = FALSE
              ),
            dblclick = dblclickOpts(
              id = "plot_dblclick",
              clip=FALSE
              ),
            hover = hoverOpts(
              id = "plot_hover",
              delay = 500,
              delayType = "debounce",
              clip = TRUE,
              nullOutside = TRUE
            )
            ),
          tags$style(
            type='text/css', 
            '#plotInfo {
              background-color: transparent; 
              color: black;
              border-style: none;}'
            ), 
          verbatimTextOutput("plotInfo")
        )
      )
    ), # end of first fluid row
    fluidRow(
      h4(textOutput(outputId = 'optionFlag'))
    ),
    conditionalPanel(
      condition = "output.optionFlag == 'Graph Options'",
      fluidRow(
        column( # Column 1, Simulation Mode
          width = 2,
          textInput(
            inputId = "title",
            label = "Title",
            value = paste("Simulation on",Sys.time())
          ),
          bsTooltip(
            id = "title", 
            title = "Enter a title for your simulation",
            placement = "top", 
            options = list(container = "body")
          ),
          textInput(
            inputId = "caption",
            label = "Caption",
            value = "",
            placeholder = "Enter figure caption"
          ),
          bsTooltip(
            id = "caption", 
            title = "Text to appear below your simulation",
            placement = "bottom", 
            options = list(container = "body")
          )
        ),
        # Column 2, Simulation Mode
        column(
          width = 1,
          radioButtons(
            inputId = "typical",
            label = "Show typical:",
            choices = c("none","Mid", "Range"),
            selected = "Range",
            inline = FALSE
          ),
          bsTooltip(
            id = "typical", 
            title = "Show typical clinical values",
            placement = "top", 
            options = list(container = "body")
          )
        ),
        # Column 3, Simulation Mode
        column(
          width = 1,
          radioButtons(
            inputId = "normalization",
            label = "Normalize to:",
            choices = c("none","Peak plasma", "Peak effect site"),
            selected = "none",
            inline = FALSE
          ),
          bsTooltip(
            id = "normalization", 
            title = "Normalization can help show relationships",
            placement = "top", 
            options = list(container = "body")
          )
        ),
        # Select MEAC, Select Interaction
        column(
          width = 1,
          checkboxGroupInput(
            inputId = "addedPlots",
            label = "Additional Plots",
            choices = c("MEAC", "Interaction", "Events", "Recovery"),
          ),
          bsTooltip(
            id = "addedPlots", 
            title = "MEAC normalizes each opioid to the minimum effective analgesic concentration, a measure of opioid potency. Interation shows the opioid hypnotic interaction. It is very preliminary.",
            placement = "top", 
            options = list(container = "body")
          )
        ),
        # Column 5, Simulation Mode
        column(
          width = 1,
          selectInput(
            inputId = "maximum",
            label = "Max Time",
            choices = maxtimes$times,
            selected = 60
          ),
          bsTooltip(
            id = "maximum", 
            title = "Maximum time. Axis will automatically expand as you enter more doses, unless maximum is set to 10 minutes",
            placement = "top", 
            options = list(container = "body")
          ),
          checkboxInput(
            inputId = "logY",
            label = "Log Y axis",
            value = FALSE
          ),
          bsTooltip(
            id = "logY", 
            title = "Plot Y axis on a log scale",
            placement = "top", 
            options = list(container = "body")
          )
        ),
        uiOutput( # Width = 1
          outputId = "Linetype"
          ),
        column(
          width=1,
          actionButton(
            inputId = "setTarget",
            label = "Suggest",
            icon=icon("fas fa-syringe"),
            width = NULL
          )
        ),
        column(
          offset = 0,
          width = 4,
          fluidRow(
            column(
              width = 8,
              textInput(
                inputId = "recipient",
                label = "Email slide to:"
              ),
              bsTooltip(
                id = "recipient", 
                title = "Enter a valid e-mail address",
                placement = "top", 
                options = list(container = "body")
              )
            ),
            column(
              width = 1,
              uiOutput("EmailButton")
            )
          ),
          fluidRow(
            plotOutput(outputId = "sentPlot")
          )
        ) # endcolumnn 
      ) # end fluidRow
    ) # End conditional panel
  ) # end dashboardBody
) # end dashboardPage
}

