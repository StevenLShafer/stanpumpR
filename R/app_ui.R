# UI for stanpumpR

# UI ------------------------------------------------------
app_ui <- function() {
  js_drug_defaults <- paste0("var drug_defaults=", jsonlite::toJSON(stanpumpR::getDrugDefaultsGlobal()))
  config <- .sprglobals$config
  my_theme <- bs_theme(
    version = 5,
    bootswatch = "minty",
    primary = "#2c3e50",
    base_font = font_google("Inter"),
    heading_font = font_google("Work Sans"),
    "card-border-radius" = "12px"
  )

  function(request) {
    page_sidebar(
      theme = my_theme,
      title = config$title,

      # Sidebar for Inputs
      sidebar = sidebar(
        width = 350,
        title = "Simulation Parameters",

        accordion(
          open = "Patient Covariates",

          # Accordion Item 1: Patient
          accordion_panel(
            "Patient Covariates",
            icon = icon("user-clock"),
            layout_column_wrap(
              width = 1/2,
              numericInput("age", "Age", value = defaultAge) |>
                inputWithChoices(c("yr" = UNIT_YEAR, "mo" = UNIT_MONTH), inputId = "ageUnit", selected = defaultAgeUnit),
              numericInput("weight", "Weight", value = defaultWeight) |>
                inputWithChoices(c("kg" = UNIT_KG, "lb" = UNIT_LB), inputId = "weightUnit", selected = defaultWeightUnit)
            ),
            numericInput("height", "Height", value = defaultHeight) |>
              inputWithChoices(c("in" = UNIT_INCH, "cm" = UNIT_CM), inputId = "heightUnit", selected = defaultHeightUnit),

            shinyWidgets::radioGroupButtons(
              inputId = "sex", label = "Sex",
              choices = c("Male" = "male", "Female" = "female"),
              justified = TRUE, selected = defaultSex
            ),

            # Advanced/Not Implemented yet in a sub-section
            hr(),
            helpText("Clinical Context (Beta)"),
            div(style = "opacity: 0.6;",
                radioButtons("pregnant", "Pregnant", c("Yes"=TRUE, "No"=FALSE), inline=TRUE),
                radioButtons("cyp2d6", "CYP 2D6", c("Rapid", "Typical", "Slow"), inline=TRUE),
                radioButtons("renal", "Renal Function", c("Normal", "Impaired", "ESRD"))
            )
          ),

          # Accordion Item 2: Dosing
          accordion_panel(
            "Dose Table",
            icon = icon("tablets"),
            shinyWidgets::radioGroupButtons(
              "timeMode", "Time Display",
              c("Clock" = "clock", "Elapsed" = "relative"),
              size = "sm", justified = TRUE
            ),
            conditionalPanel(
              "input.timeMode == 'clock'",
              textInput("referenceTime", "Start Time", placeholder = "HH:MM")
            ),
            actionButton("dosetable_apply", "Apply Changes",
                         class = "btn-primary w-100 mb-2", icon = icon("check-circle")),
            rHandsontableOutput("doseTableHTML")
          )
        )
      ),

      # Main Display Area
      layout_columns(
        col_widths = c(12, 12),

        # The Plot Card
        card(
          full_screen = TRUE,
          card_header(
            div(class = "d-flex justify-content-between align-items-center",
                span(icon("chart-line"), "Pharmacokinetic Simulation"),
                div(
                  downloadButton("downloadPlot", "Export", class = "btn-sm"),
                  actionButton("editDrugs", "Edit Drugs", icon = icon("pencil"), class = "btn-sm")
                )
            )
          ),
          plotOutput("PlotSimulation", height = "500px",
                     click = "plot_click", dblclick = "plot_dblclick", hover = "plot_hover") |>
            shinycssloaders::withSpinner(type = 5, color = "#2c3e50"),
          uiOutput("hover_info")
        ),

        # Options Area (using layout_column_wrap for responsiveness)
        card(
          card_header(icon("sliders"), "Graph & Export Options"),
          layout_column_wrap(
            width = 1/4,
            div(
              textInput("title", "Plot Title", value = paste("Simulation", format(Sys.time()))),
              textInput("caption", "Caption", placeholder = "Enter figure caption")
            ),
            div(
              radioButtons("typical", "Show typical:", c("none", "Mid", "Range"), selected = "Range"),
              radioButtons("normalization", "Normalize to:", c("none", "Peak plasma", "Peak effect site"))
            ),
            div(
              checkboxGroupInput("addedPlots", "Additional Plots",
                                 choices = c("Potency (MEAC)" = "MEAC", "Interaction", "Events", "Time Until")),
              checkboxInput("logY", "Log Y axis")
            ),
            div(
              selectInput("maximum", "Max Time (min)", choices = maxtimes$times, selected = 60),
              layout_column_wrap(
                width = 1/2,
                selectInput("plasmaLinetype", "Plasma", choices = c("solid", "dashed", "blank"), selected = "blank"),
                selectInput("effectsiteLinetype", "Effect", choices = c("solid", "dashed", "blank"), selected = "solid")
              )
            )
          ),
          card_footer(
            layout_columns(
              col_widths = c(9, 3),
              textInput("recipient", NULL, placeholder = "Email slide to..."),
              actionButton("sendSlide", "Send Email", icon = icon("paper-plane"), class = "btn-outline-primary")
            )
          )
        )
      ),

      # Debug Drawer (Hidden by default)
      div(id = "debug_area",
          accordion(
            open = FALSE,
            accordion_panel(
              "System Logs & Profiler",
              layout_columns(
                verbatimTextOutput("logContent"),
                verbatimTextOutput("profiling")
              )
            )
          )
      ) |> shinyjs::hidden()
    )
  }
}
