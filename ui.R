# UI for stanpumpR

# UI ------------------------------------------------------
ui <- function(request) {
  dashboardPage(
    dashboardHeader(
      title = config$title,
      tags$li(
        class = "dropdown",
        tags$a(
          href = config$help_link,
          id = "help_link",
          "Examples and Help",
          target = "_blank"
        )
      )
    ),
    dashboardSidebar(
      collapsed = TRUE,
      disable = TRUE,
      width = "200px",
      sidebarMenu(
        id = "simType",
        menuItem("stanpumpR", tabName = "SimulationMode", selected = TRUE)
      )
    ),

    dashboardBody(
      useShinyjs(),
      useWaiter(),
      tags$script(src = "app.js"),
      tags$head(tags$link(href = "app.css", rel = "stylesheet")),
      style = "max-height: 95vh; overflow-y: auto;" ,
      fluidRow(
        column(
          width=4,
          fluidRow(  # start of fluid row within this column
            h4("Patient Covariates"),
            style = "border-style: solid; border-color: white;  border-radius: 5px;",
            column(
              width = 4,
              div(
                class = "form-first-row",
                div( # Age ************************************
                  class = "cancel-margin",
                  numericInput(
                    inputId = "age",
                    label = "Age",
                    value = defaultAge,
                    min = MIN_AGE,
                    max = MAX_AGE
                  )
                ), # end of cancel-margin div
                div( #style = "font-size: 10px;",
                  radioButtons(
                    inputId = "ageUnit",
                    label = NULL,
                    choiceNames = c("years","months"),
                    choiceValues = c(UNIT_YEAR, UNIT_MONTH),
                    inline = TRUE,
                    selected = defaultAgeUnit
                  )
                )
              ), # end of form-first-row div
              div( # weight *****************************************
                class = "cancel-margin",
                numericInput(
                  inputId = "weight",
                  label = "Weight",
                  value = defaultWeight,
                  min = MIN_WEIGHT,
                  max = MAX_WEIGHT
                )
              ), # End of class cancel-margin div
              radioButtons(
                inputId = "weightUnit",
                label = NULL,
                choiceNames = c("kg", "lb"),
                choiceValues = c(UNIT_KG, UNIT_LB),
                inline = TRUE,
                selected = defaultWeightUnit
              ),
              div( # Height ********************************************
                class = "cancel-margin",
                numericInput(
                  inputId = "height",
                  label = "Height",
                  value = defaultHeight,
                  min = MIN_HEIGHT,
                  max = MAX_HEIGHT
                )
              ), # end of cancel-margin div
              radioButtons(
                inputId = "heightUnit",
                label = NULL,
                choiceNames = c("cms","inches"),
                choiceValues = c(UNIT_CM, UNIT_INCH),
                inline = TRUE,
                selected = defaultHeightUnit
              )
            ), # end of column
            column(
              width = 6,
              # div(
              #   class = "form-first-row",
              radioButtons(
                inputId = "sex",
                label = "Sex",
                choiceNames = c("Male","Female"),
                choiceValues = c("male","female"),
                inline = TRUE,
                selected = defaultSex
              ),
              #            ), # end of class: form-first-row div
              disabled(conditionalPanel(
                condition = "input.age && input.ageUnit && input.ageUnit == 1 && input.age > 11 & input.age < 60 && input.sex == 'female'",
                radioButtons(
                  inputId = "pregnant",
                  label = "Pregnant",
                  choiceNames = c("Yes","No"),
                  choiceValues = c(TRUE, FALSE),
                  inline = TRUE,
                  selected = FALSE
                )
              )),
              disabled(radioButtons(
                inputId = "cyp2d6",
                label = "CYP 2D6",
                choiceNames = c("Rapid","Typical", "Slow"),
                choiceValues = c("rapid","typical", "slow"),
                inline = FALSE,
                selected = "typical"
              )),
              disabled(radioButtons(
                inputId = "renal",
                label = "Renal Function",
                choiceNames = c("Normal","Impaired", "ESRD"),
                choiceValues = c("normal","impaired", "ESRD"),
                inline = FALSE,
                selected = "normal"
              ))
            )
          ), # fluidrow within column
          fluidRow(
            h4("Dose Table"),
            style = "border-style: solid; border-color: white;  border-radius: 5px; height: 450px; ",

            column(
              width=12,
              fluidRow(
                column(
                  width = 4,
                  textInput("referenceTime", "Reference Time", placeholder = "HH:MM")
                )
              ), # end of fluid row
              # Note to Dean: Here is where the handsontable is output. My guess is that this is where
              # you would replace the existing "rHandsontableOutput()" function with JavaScript code that
              # will process and validate the table.
              tags$script(HTML(js_drug_defaults)),
              tags$script(src = "hot_funs.js"),
              rHandsontableOutput(outputId = "doseTableHTML")
            ) # End of column
          ) # end of second fluid row in width 5 column
        ), # end of width 5 column
        column(
          width=8,
          fillPage(
            div(
              style = "position:relative",
              # Start of plot Output
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
                  clip = FALSE,
                  nullOutside = FALSE
                )
              ), # End of plotOutput
              uiOutput("hover_info")
            ) # End of div
          ), # End of fillPage
          ############################################################################
          fluidRow(
            style = "border-style: solid; border-color: white;  border-radius: 5px; height: 300px",
            h4("Graph Options"),
            column( # Column 1, Simulation Mode
              width = 2,
              textInput(
                inputId = "title",
                label = "Title",
                value = paste("Simulation on",format(Sys.time()))
              ),
              textInput(
                inputId = "caption",
                label = "Caption",
                value = "",
                placeholder = "Enter figure caption"
              ),
              div(
                style = "padding-bottom: 10px;",
                actionButton(
                  inputId = "setTarget",
                  label = "Suggest",
                  icon=icon("fas fa-syringe"),
                  width = NULL
                )
              ),
              actionButton(
                inputId = "editDrugs",
                label = "Edit Drugs",
                icon=icon("fas fa-syringe"),
                width = NULL
              )
            ), # End of column
            # Column 2, Simulation Mode
            column(
              width = 2,
              radioButtons(
                inputId = "typical",
                label = "Show typical:",
                choices = c("none","Mid", "Range"),
                selected = "Range",
                inline = FALSE
              ),
              radioButtons(
                inputId = "normalization",
                label = "Normalize to:",
                choices = c("none","Peak plasma", "Peak effect site"),
                selected = "none",
                inline = FALSE
              )
            ),
            # Select MEAC, Select Interaction, events, and time to emergence
            column(
              width = 2,
              checkboxGroupInput(
                inputId = "addedPlots",
                label = "Additional Plots",
                choiceNames = c("MEAC", "Interaction", "Events", "Time until ___"),
                choiceValues = c("MEAC", "Interaction", "Events", "Time Until")
              ),
              conditionalPanel(
                condition = "!(input.addedPlots.includes('Time Until') || input.addedPlots.includes('Events') || input.addedPlots.includes('Interaction'))",
                checkboxInput(
                  inputId = "logY",
                  label = "Log Y axis",
                  value = FALSE
                )
              )
            ),
            # Column 5, Simulation Mode
            column(
              width = 2,
              selectInput(
                inputId = "maximum",
                label = "Max Time",
                choices = maxtimes$times,
                selected = 60
              ),
              selectInput(
                inputId = "plasmaLinetype",
                label = "Plasma",
                selected = "blank",
                choices = c("solid",
                            "dashed",
                            "longdash",
                            "dotted",
                            "dotdash",
                            "twodash",
                            "blank")
              ),
              selectInput(
                inputId = "effectsiteLinetype",
                label = "Effect site",
                selected = "solid",
                choices = c("solid",
                            "dashed",
                            "longdash",
                            "dotted",
                            "dotdash",
                            "twodash",
                            "blank")
              )
            ),
            column(
              offset = 0,
              width = 4,
              fluidRow(
                column(
                  width = 9,
                  textInput(
                    inputId = "recipient",
                    label = "Email slide to:"
                  )
                ),
                column(
                  width = 3,
                  div(
                    id = "sendSlideButton",
                    style = "padding-top: 25px;",
                    actionButton(
                      inputId = "sendSlide",
                      label = "GO!",
                      icon = icon("far fa-envelope")
                    )
                  ),
                  div(
                    id = "sendSlideError",
                    style = "padding-top: 25px;",
                    "Check address"
                  )
                )
              ), # End of fluid row
              imageOutput(
                outputId = "sentPlot",
                height = "100px",
                width = "166px"
              )
            ) # end columnn
          ) # end fluidRow
        ) # End of right hand colunn
      ), # end of first fluid row
      fluidRow(
        column(
          12,
          wellPanel(
            id = "logSection",
            uiOutput("logContent")
          )
        )
      )
    ) # end dashboardBody
  ) # end dashboardPage
}

