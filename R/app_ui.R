app_ui <- function() {
  js_drug_defaults <- paste0("var drug_defaults=", jsonlite::toJSON(stanpumpR::getDrugDefaultsGlobal()))
  config <- .sprglobals$config

  stanpumpr_theme <- bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2c3e50",
    secondary = "#7b8a8b",
    base_font = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Public Sans")
  )

  function(request) {
    bslib::page_navbar(
      title = span(config$title, class = if (config$long_title) "title-long"),
      theme = stanpumpr_theme,

      header = tags$head(
        shinyjs::useShinyjs(),
        tags$script(src = "stanpumpr-assets/app.js"),
        tags$script(HTML(js_drug_defaults)),
        tags$script(src = "stanpumpr-assets/hot_funs.js"),
        tags$link(href = "stanpumpr-assets/app.css", rel = "stylesheet")
      ),

      bslib::nav_panel(
        "Simulator test",
        icon = icon("chart-line"),
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            width = 300,
            bslib::accordion(
              open = FALSE,

              bslib::accordion_panel(
                "Patient Profile",
                icon = icon("user-injured"),

                numericInput(
                  inputId = "age",
                  label = "Age",
                  value = defaultAge,
                  min = MIN_AGE,
                  max = MAX_AGE
                ) |>
                  inputWithChoices(
                    c("yr" = UNIT_YEAR, "mo" = UNIT_MONTH),
                    inputId = "ageUnit",
                    selected = defaultAgeUnit
                  ),

                numericInput(
                  inputId = "weight",
                  label = "Weight",
                  value = defaultWeight,
                  min = MIN_WEIGHT,
                  max = MAX_WEIGHT
                ) |>
                  inputWithChoices(
                    c("kg" = UNIT_KG, "lb" = UNIT_LB),
                    inputId = "weightUnit",
                    selected = defaultWeightUnit
                  ),

                numericInput(
                  inputId = "height",
                  label = "Height",
                  value = defaultHeight,
                  min = MIN_HEIGHT,
                  max = MAX_HEIGHT
                ) |>
                  inputWithChoices(
                    c("in" = UNIT_INCH, "cm" = UNIT_CM),
                    inputId = "heightUnit",
                    selected = defaultHeightUnit
                  ),

                shinyWidgets::radioGroupButtons(
                  inputId = "sex", label = "Sex",
                  choiceNames = list(span(icon("mars"), "Male"), span(icon("venus"), "Female")),
                  choiceValues = c("male", "female"),
                  justified = TRUE,
                  selected = defaultSex
                ),

                conditionalPanel(
                  condition = "input.age && input.ageUnit && input.ageUnit == 1 && input.age > 11 & input.age < 60 && input.sex == 'female'",
                  shinyWidgets::radioGroupButtons(
                    inputId = "pregnant",
                    label = "Pregnant",
                    choiceNames = list(
                      span(icon("check"), "Yes"),
                      span(icon("xmark"), "No")
                    ),
                    choiceValues = c(TRUE, FALSE),
                    justified = TRUE,
                    selected = FALSE
                  ) |>
                    shinyjs::disabled()
                ),

                selectInput(
                  inputId = "cyp2d6",
                  label = "CYP 2D6",
                  c("Rapid" = "rapid", "Typical" = "typical", "Slow" = "slow"),
                  selected = "typical"
                ) |>
                  shinyjs::disabled(),

                selectInput(
                  inputId = "renal",
                  label = "Renal Function",
                  c("Normal" = "normal", "Impaired" = "impaired", "ESRD" = "ESRD"),
                  selected = "normal"
                ) |>
                  shinyjs::disabled()
              ),

              bslib::accordion_panel(
                "Graph Options",
                icon = icon("sliders"),
                textInput("title", "Title", paste("Simulation on", format(Sys.time()))),
                textInput("caption", "Caption", "", placeholder = "Enter figure caption"),
                selectInput("typical", "Show typical:", c("none","Mid", "Range"), selected = "Range"),
                selectInput("normalization", "Normalize to:", c("none","Peak plasma", "Peak effect site")),
                conditionalPanel(
                  condition = "!(input.addedPlots.includes('Time Until') || input.addedPlots.includes('Events') || input.addedPlots.includes('Interaction'))",
                  checkboxInput(
                    inputId = "logY",
                    label = "Log Y axis",
                    value = FALSE
                  )
                ),
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

              bslib::accordion_panel(
                "Additional Plots",
                icon = icon("chart-line"),
                checkboxGroupInput(
                  inputId = "addedPlots",
                  label = NULL,
                  choiceNames = c("MEAC", "Interaction", "Events", "Time until ___"),
                  choiceValues = c("MEAC", "Interaction", "Events", "Time Until")
                )
              ),

              bslib::accordion_panel(
                "Email Slide",
                icon = icon("envelope"),
                textInput("recipient", NULL, "", placeholder = "Enter email address"),
                actionButton("sendSlide", "Send", class = "btn-primary")
              )
            ),

            actionButton("setTarget", "Suggest Dosing", class = "btn-outline-primary", icon = icon("fas fa-wand-magic-sparkles")),
            actionButton("editDrugs", "Modify Drug Library", class = "btn-outline-primary", icon = icon("fas fa-pencil"))
          ),

          bslib::layout_columns(
            style = "grid-template-columns: 1fr 450px;",

            bslib::card(
              class = "overflow-hidden",
              plotOutput(
                outputId = "PlotSimulation",
                width = "100%",
                height = "auto",
                click = clickOpts(
                  id = "plot_click",
                  clip = FALSE
                ),
                dblclick = dblclickOpts(
                  id = "plot_dblclick",
                  clip = FALSE
                ),
                hover = hoverOpts(
                  id = "plot_hover",
                  delay = 500,
                  delayType = "debounce",
                  clip = FALSE,
                  nullOutside = FALSE
                )
              ) |>
                shinycssloaders::withSpinner(hide.ui = FALSE) |>
                bslib::as_fill_carrier(),  # TODO this is a temporary hack until shinycssloaders v > 1.1.0 is released
              uiOutput("hover_info"),

              bslib::card_footer(
                class = "small text-muted",
                "Hover for precise concentration. Click to add new dose or edit existing doses for a specific drug. Double click to delete."
              )
            ),

            bslib::card(
              bslib::card_header(icon("syringe"), "Doses"),

              bslib::layout_columns(
                selectInput(
                  "timeMode",
                  "Time Display",
                  c("Actual time" = "clock",
                    "Elapsed minutes" = "relative")
                ),
                conditionalPanel(
                  "input.timeMode == 'clock'",
                  textInput("referenceTime", "Procedure start", placeholder = "HH:MM")
                )
              ),
              br(),
              rhandsontable::rHandsontableOutput("doseTableHTML"),

              bslib::card_footer(
                div(
                  class = "d-grid",
                  style = "grid-template-columns: 1fr auto auto; gap: 0.25rem",
                  actionButton("dosetable_apply", "Apply Changes", icon = icon("circle-check"), class = "btn-primary my-0 btn-lg"),
                  actionButton("dosetable_undo", NULL, icon = icon("undo"), title = "Undo", class = "my-0 btn-lg btn-outline-primary"),
                  actionButton("dosetable_redo", NULL, icon = icon("redo"), title = "Redo", class = "my-0 btn-lg btn-outline-primary")
                )
              )
            )
          ),

          bslib::accordion(
            id = "debug_area",
            open = FALSE,
            bslib::accordion_panel(
              "Debug Section",
              icon = icon("terminal"),
              bslib::layout_columns(
                div(
                  div("Log", class = "debug_section_head"),
                  div(
                    "Debug level", HTML("&nbsp;"),
                    selectInput("debug_level", NULL, width = 100, selectize = FALSE,
                                choices = c("Normal" = DEBUG_LEVEL_NORMAL,
                                            "Verbose" = DEBUG_LEVEL_VERBOSE)) |>
                      inlineUI()
                  ),
                  verbatimTextOutput("logContent")
                ),
                div(
                  div("Profiler", class = "debug_section_head"),
                  div(
                    "Only show functions taking longer than", HTML("&nbsp;"),
                    numericInput("profiler_threshold", NULL, min = 0, max = 1000,
                                 value = 100, width = 40, updateOn = "blur") |>
                      inlineUI() |>
                      attachClass("no-spinners"),
                    HTML("&nbsp;"), "milliseconds"
                  ),
                  verbatimTextOutput("profiling")
                )
              )
            )
          ) |> shinyjs::hidden()
        )
      ),

      bslib::nav_spacer(),
      bslib::nav_item(
        tags$a(
          icon("circle-info"),
          "Examples and Help",
          href = config$help_link,
          target = "_blank"
        )
      )
    )
  }
}
