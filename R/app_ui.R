app_ui <- function() {
  js_drug_defaults <- paste0("var drug_defaults=", jsonlite::toJSON(stanpumpR::getDrugDefaultsGlobal()))
  config <- .sprglobals$config

  # Helper: wraps SVG body content in a titled 20×14 SVG element.
  linetypeSVG <- function(content, label) {
    HTML(paste0('<svg width="20" height="14"><title>', label, "</title>", content, "</svg>"))
  }

  linetypeChoiceNames <- list(
    # none: ✕
    linetypeSVG(paste0(
      '<line x1="4"  y1="2" x2="16" y2="12" stroke="currentColor" stroke-width="2"/>',
      '<line x1="16" y1="2" x2="4"  y2="12" stroke="currentColor" stroke-width="2"/>'
    ), "none"),
    # solid: full-width line with vertical end caps
    # vector-effect="non-scaling-stroke" keeps stroke widths constant while
    # preserveAspectRatio="none" lets the horizontal line stretch freely
    HTML(paste0(
      '<svg width="100%" height="14" viewBox="0 0 100 14" preserveAspectRatio="none">',
      '<title>solid</title>',
      '<line x1="1"  y1="7"  x2="99" y2="7"  stroke="currentColor" stroke-width="2" vector-effect="non-scaling-stroke"/>',
      '<line x1="1"  y1="4"  x2="1"  y2="10" stroke="currentColor" stroke-width="2" vector-effect="non-scaling-stroke"/>',
      '<line x1="99" y1="4"  x2="99" y2="10" stroke="currentColor" stroke-width="2" vector-effect="non-scaling-stroke"/>',
      '</svg>'
    )),
    # dashed: one centred dash
    linetypeSVG(
      '<line x1="4" y1="7" x2="16" y2="7" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>',
      "dashed"
    ),
    # longdash: one long dash
    linetypeSVG(
      '<line x1="2" y1="7" x2="18" y2="7" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>',
      "longdash"
    ),
    # dotted: row of round dots
    linetypeSVG(
      '<line x1="1" y1="7" x2="19" y2="7" stroke="currentColor" stroke-width="2.5" stroke-dasharray="0.1,3.9" stroke-linecap="round"/>',
      "dotted"
    ),
    # dotdash: one dot then one dash
    linetypeSVG(paste0(
      '<circle cx="4" cy="7" r="1.5" fill="currentColor"/>',
      '<line x1="9" y1="7" x2="18" y2="7" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>'
    ), "dotdash"),
    # twodash: two equal dashes
    linetypeSVG(paste0(
      '<line x1="2"  y1="7" x2="8"  y2="7" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>',
      '<line x1="12" y1="7" x2="18" y2="7" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>'
    ), "twodash")
  )
  linetypeChoiceValues <- c("blank", "solid", "dashed", "longdash", "dotted", "dotdash", "twodash")


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
        "Simulator",
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
                selectInput("typical", "Show typical", c("<none>" = "none","Mid", "Range"), selected = "Range"),
                selectInput("normalization", "Normalize to", c("<none>" = "none","Peak plasma", "Peak effect site")),
                selectInput(
                  inputId = "maximum",
                  label = "Max time (minutes)",
                  choices = setNames(maxtimes$times, format(maxtimes$times, scientific = FALSE, trim = TRUE, big.mark = ",")),
                  selected = 60
                ),
                selectInput(
                  inputId = "plasmaLinetype",
                  label = "Plasma line",
                  selected = "blank",
                  choices = c(
                    "<none>" = "blank",
                    "solid",
                    "dashed",
                    "longdash",
                    "dotted",
                    "dotdash",
                    "twodash"
                  )
                ),
                selectInput(
                  inputId = "effectsiteLinetype",
                  label = "Effect site line",
                  selected = "solid",
                  choices = c(
                    "<none>" = "blank",
                    "solid",
                    "dashed",
                    "longdash",
                    "dotted",
                    "dotdash",
                    "twodash"
                  )
                ),
                sliderInput("yaxisHeight", "Y axis height", 150, 350, 200, ticks = FALSE),
                checkboxInput(
                  "showThreshold",
                  "Time until threshold",
                  value = FALSE
                ),
                conditionalPanel(
                  condition = sprintf("!(input.showThreshold || input.addedPlots.includes('%s') || input.addedPlots.includes('Interaction'))", DRUG_NAME_EVENTS),
                  checkboxInput(
                    inputId = "logY",
                    label = "Log Y axis",
                    value = FALSE
                  )
                ),
              ),

              bslib::accordion_panel(
                "Additional Plots",
                icon = icon("chart-line"),
                checkboxGroupInput(
                  inputId = "addedPlots",
                  label = NULL,
                  choices = c("MEAC", "Interaction", DRUG_NAME_EVENTS)
                )
              ),

              bslib::accordion_panel(
                "Email Slide",
                icon = icon("envelope"),
                textInput("recipient", NULL, "", placeholder = "Enter email address"),
                actionButton("sendSlide", "Send", class = "btn-primary")
              )
            ),

            actionButton("setTarget", "Suggest Dosing", class = "btn-outline-primary", icon = icon("fas fa-prescription")),
            actionButton("editDrugs", "Drug Library", class = "btn-outline-primary", icon = icon("fas fa-capsules")),
            actionButton("editThresholds", "Drug Thresholds", class = "btn-outline-primary", icon = icon("fas fa-bullseye"))
          ),

          bslib::layout_columns(
            style = "grid-template-columns: 1fr 450px;",

            bslib::card(
              id = "plotContainer",
              class = "overflow-hidden",
              plotOutput(
                outputId = "PlotSimulation",
                width = "100%",
                height = "auto",
                click = clickOpts("plot_click"),
                dblclick = dblclickOpts("plot_dblclick"),
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
                "Hover for precise concentration. Click to add new dose. Double click to edit or delete a drug's doses."
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
                    "Debug level", nbsp,
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
                    "Only show functions taking longer than", nbsp,
                    numericInput("profiler_threshold", NULL, min = 0, max = 1000,
                                 value = 100, width = 40, updateOn = "blur") |>
                      inlineUI() |>
                      attachClass("no-spinners"),
                    nbsp, "milliseconds"
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
