app_ui <- function() {
  js_drug_defaults <- paste0("var drug_defaults=", jsonlite::toJSON(getDrugDefaultsGlobal()))
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
    # The Time units, Max time and Time Display choices depend on one another,
    # and a restored bookmark's selections must be among the choices built
    # here: selectize ignores a selection it has no option for.  A bookmark
    # made before there were time units has none: it opens in days if its Max
    # time was more than a day (and the server converts its dose table).
    timeUnit0 <- shiny::restoreInput("timeUnits", NULL)
    if (!identical(validTimeUnit(timeUnit0), timeUnit0)) {
      timeUnit0 <- legacyTimeUnit(shiny::restoreInput("maximum", 60))
    }
    maximum0 <- snapMaximum(shiny::restoreInput("maximum", 60), timeUnit0)

    bslib::page_navbar(
      id = "mainNav",
      title = span(config$title, class = if (config$long_title) "title-long"),
      theme = stanpumpr_theme,

      header = tags$head(
        tags$meta(
          `http-equiv` = "Content-Security-Policy",
          content = paste(
            "default-src 'self';",
            "script-src 'self' 'unsafe-inline' 'unsafe-eval';",  # many htmlwidgets use inline scripts
            "style-src 'self' 'unsafe-inline';",
            "img-src 'self' data:;",
            "font-src 'self' data:;",
            "connect-src 'self';",
            "object-src 'none';",
            "base-uri 'self';"
          )
        ),
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
          # The options, in a sidebar that its arrow collapses; open on a desktop
          # (closed on a phone), as the dose table's sidebar at the right is.
          sidebar = bslib::sidebar(
            width = 300,
            resizable = FALSE,
            open = "desktop",
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
                  ) |>
                  addInputAttributes(
                    oninput = glue::glue(
                      "if (this.value > {MAX_AGE}) {{ this.value = {MAX_AGE}; }}"
                    )
                  ),

                conditionalPanel(
                  glue::glue("input.age >= {MAX_AGE}"),
                  div(
                    class = "info-note",
                    icon("circle-info"),
                    glue::glue("An age of {MAX_AGE} or above is PHI. Ages > {MAX_AGE} are entered as {MAX_AGE}.")
                  )
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

                # Most models scale to the patient's fat-free mass rather than to
                # total body weight; see docs/weight-adjustment.md.
                bslib::tooltip(
                  checkboxInput(
                    inputId = "adjustToFFM",
                    label = "Adjust weight to fat-free mass",
                    value = TRUE
                  ),
                  paste(
                    "The pharmacokinetic weight is scaled to the patient's fat-free",
                    "mass, calculated from weight, height, age and sex",
                    "(Al-Sallami et al., Clin Pharmacokinet 2015), relative to a",
                    "70 kg, 170 cm man. Doses entered per kg still use total body",
                    "weight. Propofol and remifentanil already include fat-free",
                    "mass in their models and are not affected."
                  ),
                  placement = "right"
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
                  choiceValues = SEX_VALUES,
                  justified = TRUE,
                  selected = defaultSex
                ),

                # Read only by the eight models R/pregnancy.R adjusts
                # (R/pregnancy.R; the help's Cautions page lists them).
                # Offered for a woman of PREGNANCY_MIN_AGE to PREGNANCY_MAX_AGE
                # years, whatever unit the age is entered in; the server
                # applies the same rule (pregnancyPossible()), so a control
                # hidden by a change of age or sex leaves the flag off.
                conditionalPanel(
                  condition = sprintf(
                    "input.age && input.ageUnit && input.sex == 'female' && (input.age * input.ageUnit) >= %s && (input.age * input.ageUnit) < %s",
                    PREGNANCY_MIN_AGE, PREGNANCY_MAX_AGE
                  ),
                  bslib::tooltip(
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
                    ),
                    paste(
                      "Applies the pharmacokinetic changes of late pregnancy",
                      "from the review the library follows (Ansari, Carvalho,",
                      "Shafer and Flood 2016): its figures for morphine,",
                      "remifentanil, oxycodone, methadone and dexamethasone,",
                      "and its 50% rise in glomerular filtration for",
                      "cefazolin, gentamicin and vancomycin. Every other drug",
                      "is unchanged; the Help's Pregnancy page says what the",
                      "review found for each."
                    ),
                    placement = "right"
                  )
                ),

                # The four pharmacogenetic fields and the serum creatinine
                # each carry a hover tip naming the drugs that read them,
                # built from the model signatures (R/patient-fields.R), so
                # the tips follow the code.
                #
                # Four categories, not three, and named the way the
                # genotyping laboratories report them.  Codeine is the first
                # drug whose kinetics read this, so it is no longer disabled.
                bslib::tooltip(
                  selectInput(
                    inputId = "cyp2d6",
                    label = "CYP 2D6",
                    c("Ultrarapid"   = CYP2D6_ULTRARAPID,
                      "Normal"       = CYP2D6_NORMAL,
                      "Intermediate" = CYP2D6_INTERMEDIATE,
                      "Poor"         = CYP2D6_POOR),
                    selected = CYP2D6_DEFAULT
                  ),
                  patientFieldTooltip("cyp2d6"),
                  placement = "right"
                ),

                # Read by escitalopram and citalopram, whose published
                # clearances differ by CYP2C19 phenotype.  The five CPIC terms.
                bslib::tooltip(
                  selectInput(
                    inputId = "cyp2c19",
                    label = "CYP 2C19",
                    c("Ultrarapid"   = CYP2C19_ULTRARAPID,
                      "Rapid"        = CYP2C19_RAPID,
                      "Normal"       = CYP2C19_NORMAL,
                      "Intermediate" = CYP2C19_INTERMEDIATE,
                      "Poor"         = CYP2C19_POOR),
                    selected = CYP2C19_DEFAULT
                  ),
                  patientFieldTooltip("cyp2c19"),
                  placement = "right"
                ),

                # Read by phenytoin, whose maximal elimination rate depends on
                # CYP2C9 diplotype, read by meloxicam and phenytoin (see
                # CYP2C9_VALUES in R/constants.R).  *1/*1 is the reference.
                bslib::tooltip(
                  selectInput(
                    inputId = "cyp2c9",
                    label = "CYP 2C9",
                    CYP2C9_VALUES,
                    selected = CYP2C9_DEFAULT
                  ),
                  patientFieldTooltip("cyp2c9"),
                  placement = "right"
                ),

                # Read by methadone, whose non-renal clearance of each
                # enantiomer depends on CYP2B6 (R/drugs_methadone.R).  The five
                # CPIC categories; "normal" (*1/*1) is the reference.
                bslib::tooltip(
                  selectInput(
                    inputId = "cyp2b6",
                    label = "CYP 2B6",
                    c("Ultrarapid"   = CYP2B6_ULTRARAPID,
                      "Rapid"        = CYP2B6_RAPID,
                      "Normal"       = CYP2B6_NORMAL,
                      "Intermediate" = CYP2B6_INTERMEDIATE,
                      "Poor"         = CYP2B6_POOR),
                    selected = CYP2B6_DEFAULT
                  ),
                  patientFieldTooltip("cyp2b6"),
                  placement = "right"
                ),

                # Read by warfarin: the -1639G>A genotype sets the sensitivity
                # (the S-warfarin C50 / EC50).  GG is the wild-type reference
                # (see VKORC1_VALUES in R/constants.R).
                bslib::tooltip(
                  selectInput(
                    inputId = "vkorc1",
                    label = "VKORC1",
                    VKORC1_VALUES,
                    selected = VKORC1_DEFAULT
                  ),
                  patientFieldTooltip("vkorc1"),
                  placement = "right"
                ),

                # Read only by the osmotic agents (mannitol), which plot the
                # serum osmolality they produce on top of this baseline.
                bslib::tooltip(
                  numericInput(
                    inputId = "osmolality",
                    label = "Baseline serum osmolality (mOsm/kg)",
                    value = OSMOLALITY_DEFAULT,
                    min = MIN_OSMOLALITY,
                    max = MAX_OSMOLALITY,
                    step = 1
                  ),
                  paste(
                    "The patient's measured serum osmolality before any",
                    "mannitol. Mannitol is plotted as the predicted serum",
                    "osmolality: this baseline plus the rise mannitol causes."
                  ),
                  placement = "right"
                ),

                # Read only by the models that name `creatinine` in their
                # signature (the hover tip and the help's Cautions page list
                # them, and tests keep both complete).  Blank means an assumed
                # normal creatinine for the patient's age and sex; see
                # R/renalFunction.R.
                bslib::tooltip(
                  numericInput(
                    inputId = "creatinine",
                    label = "Serum creatinine (mg/dL)",
                    value = NA,
                    min = MIN_CREATININE,
                    max = MAX_CREATININE,
                    step = 0.1
                  ),
                  patientFieldTooltip("creatinine"),
                  placement = "right"
                )
              ),

              bslib::accordion_panel(
                "Graph Options",
                icon = icon("sliders"),
                selectInput("typical", "Show typical", c("<none>" = "none", "Mid", "Range"), selected = "Range"),
                selectInput("normalization", "Normalize to", c("<none>" = NORMALIZE_NONE, "Peak plasma", "Peak effect site")),
                # Durations in minutes, labelled in the Time units; the server
                # swaps the list when the unit changes (syncMaxTimeChoices()).
                selectInput(
                  inputId = "maximum",
                  label = "Max time",
                  choices = maxTimeChoices(timeUnit0),
                  selected = maxTimeValue(maximum0)
                ),
                lineTypeSelector(
                  inputId = "plasmaLinetype",
                  label = "Plasma line",
                  selected = "blank"
                ),
                lineTypeSelector(
                  inputId = "effectsiteLinetype",
                  label = "Effect site line",
                  selected = "solid"
                ),
                sliderInput("yaxisHeight", "Y axis height", MIN_YAXIS_HEIGHT, MAX_YAXIS_HEIGHT, 200, ticks = FALSE),
                conditionalPanel(
                  condition = "input.normalization === 'none'",
                  checkboxInput(
                    "showThreshold",
                    "Time until threshold",
                    value = FALSE
                  ),
                  # Each opioid's % MEAC on its own panel, on one scale shared
                  # by all of them, read at the right-hand edge where the time
                  # until threshold's labels go; ticking either unticks the
                  # other (R/meacOverlay.R).
                  bslib::tooltip(
                    checkboxInput(
                      "showMEAC",
                      "% MEAC on opioid panels",
                      value = FALSE
                    ),
                    paste(
                      "Draw each opioid's effect-site concentration as a",
                      "percentage of its MEAC on that opioid's panel, read at",
                      "the right-hand edge. One scale is shared by every opioid",
                      "panel, set by the highest % MEAC any of them reaches, so",
                      "the line that rises highest is the opioid carrying the",
                      "analgesia. Cannot be shown with Time until threshold."
                    ),
                    placement = "right"
                  )
                ),
                conditionalPanel(
                  condition = sprintf("!(input.showThreshold || input.showMEAC || input.addedPlots.includes('%s') || input.addedPlots.includes('%s'))", PLOT_ID_EVENTS, PLOT_ID_INTERACTION),
                  checkboxInput(
                    inputId = "logY",
                    label = "Log Y axis",
                    value = FALSE
                  )
                ),
                # Opioids lower MAC.  When ticked, the MAC series is reported in
                # multiples of the opioid-reduced MAC; see R/opioidMacInteraction.R.
                checkboxInput(
                  inputId = "opioidMacInteraction",
                  label = "Include opioid - MAC interaction",
                  value = FALSE
                ),
                # The Schnider propofol and Minto remifentanil models, the
                # models in most TCI pumps, run beside the library's Eleveld
                # models and drawn as a second line in the same panels
                # (R/alternativeModels.R).  The server enables the box while
                # propofol or remifentanil is in the dose table.
                bslib::tooltip(
                  checkboxInput(
                    inputId = "mintoSchnider",
                    label = ALTERNATIVE_MODELS_LABEL,
                    value = FALSE
                  ) |>
                    shinyjs::disabled(),
                  paste(
                    "Also run the Schnider (1998) propofol and Minto (1997)",
                    "remifentanil models, the models in most TCI pumps, and",
                    "draw them beside the Eleveld curves as a second line in",
                    "the drug's panel. Under a target-controlled infusion the",
                    "second model drives a pump of its own, drawn as a second",
                    "line in the TCI rate panel. Time until threshold stays on",
                    "the Eleveld model. Available when propofol or remifentanil",
                    "is in the dose table."
                  ),
                  placement = "right"
                ),
                # The Hamberg (2007) warfarin genotype model runs beside the
                # library's (Xue) model as a second INR line in warfarin's
                # panel (R/alternativeModels.R).  The server enables the box
                # while warfarin is in the dose table.
                bslib::tooltip(
                  checkboxInput(
                    inputId = "hamberg",
                    label = COMPANION_MODEL_GROUPS$hamberg$label,
                    value = FALSE
                  ) |>
                    shinyjs::disabled(),
                  paste(
                    "Also run the Hamberg (2007) warfarin model, built to show",
                    "how age, CYP2C9 and VKORC1 genotype shape the INR, and",
                    "draw it beside the library's INR curve as a second line.",
                    "Set the CYP2C9 and VKORC1 fields in the Patient Profile.",
                    "A research and teaching model, not dosing guidance.",
                    "Available when warfarin is in the dose table."
                  ),
                  placement = "right"
                ),
              ),

              bslib::accordion_panel(
                "Additional Plots",
                icon = icon("chart-line"),
                checkboxGroupInput(
                  inputId = "addedPlots",
                  label = NULL,
                  choices = c(PLOT_ID_MEAC, PLOT_ID_INTERACTION, PLOT_ID_EVENTS)
                )
              ),

              # The time settings.  The header reads them out while the panel
              # is closed: "Time (hours, Actual time)".
              bslib::accordion_panel(
                span(
                  "Time",
                  textOutput("timeSummary", inline = TRUE) |>
                    htmltools::tagAppendAttributes(class = "small text-muted")
                ),
                value = "time",
                icon = icon("clock"),

                # Time units: what a number typed in the dose table means, and
                # the unit of the time axis.  Changing it converts the dose
                # table (R/utils-time.R).  Actual (clock) time is offered for
                # minutes and hours only.
                selectizeInput(
                  "timeUnits",
                  "Time units",
                  stats::setNames(names(TIME_UNITS), tools::toTitleCase(names(TIME_UNITS))),
                  selected = timeUnit0,
                  options = list(dropdownParent = "body")
                ),
                selectizeInput(
                  "timeMode",
                  "Time Display",
                  timeModeChoices(timeUnit0),
                  options = list(dropdownParent = "body")
                ),
                conditionalPanel(
                  sprintf(
                    "input.timeMode == 'clock' && %s.indexOf(input.timeUnits) >= 0",
                    jsonlite::toJSON(CLOCK_TIME_UNITS)
                  ),
                  textInput("referenceTime", "Procedure start", placeholder = "HH:MM")
                )
              ),

              # The dose table's options, so the table's own sidebar holds
              # nothing but the table.
              bslib::accordion_panel(
                "Dose table",
                icon = icon("syringe"),
                # Opt-in for the drug-of-abuse research models (off by
                # default); when on, they are offered in the dose table and
                # the Add a dose dialog, with their names shown in red.
                bslib::tooltip(
                  checkboxInput(
                    "showIllicitDrugs",
                    "Show illicit drugs",
                    value = FALSE
                  ),
                  paste(
                    "Show evidence-labelled research models of drugs of",
                    "abuse (for example cocaine or psilocybin). Off by default. These",
                    "are plasma-exposure estimates for research, not dosing",
                    "advice or safety thresholds."
                  ),
                  placement = "right"
                ),
                actionLink("setTarget", "Suggest Dosing")
              ),

              bslib::accordion_panel(
                "Email Slide",
                icon = icon("envelope"),
                if (is.null(config$email_username) || is.null(config$email_password)) {
                  div(
                    class = "info-note",
                    icon("circle-info"),
                    "Email is not configured. Please ask admin to set email username and password in app configuration."
                  )
                } else {
                  tagList(
                    textInput("recipient", NULL, "", placeholder = "Enter email address"),
                    textAreaInput("emailComments", NULL, "", placeholder = "Comments (optional)", rows = 3) |>
                      addInputAttributes(maxlength = MAX_INPUT_TEXT),
                    checkboxInput("commentSafe", "This comment does not contain PHI", FALSE) |>
                      htmltools::tagAppendAttributes(class = "micro"),
                    actionButton("sendSlide", "Send", class = "btn-primary")
                  )
                }
              )
            )
          ),

          # The plot, with the dose table in a sidebar at its right that its
          # arrow collapses (open on a desktop, closed on a phone), so the plot
          # can have the whole width once the doses are in.  That sidebar holds
          # the table and its Apply, Undo and Redo buttons and nothing else:
          # the time settings and the table's options are panels of the left
          # sidebar.  The table fills the sidebar's height and scrolls inside
          # it (.dose-sidebar in app.css).
          bslib::layout_sidebar(
            fillable = TRUE,
            border = FALSE,
            padding = 0,
            sidebar = bslib::sidebar(
              position = "right",
              width = 450,
              open = "desktop",
              class = "dose-sidebar",
              title = span(
                icon("syringe"), "Doses",
                # "(times in days)": the unit of the times in the table
                textOutput("doseTimeUnits", inline = TRUE) |>
                  htmltools::tagAppendAttributes(class = "small text-muted")
              ),

              rhandsontable::rHandsontableOutput("doseTableHTML"),

              div(
                class = "d-grid mt-2",
                style = "grid-template-columns: 1fr auto auto; gap: 0.25rem",
                actionButton("dosetable_apply", "Apply Changes", icon = icon("circle-check"), class = "btn-primary my-0 btn-lg"),
                actionButton("dosetable_undo", NULL, icon = icon("undo"), title = "Undo", class = "my-0 btn-lg btn-outline-primary"),
                actionButton("dosetable_redo", NULL, icon = icon("redo"), title = "Redo", class = "my-0 btn-lg btn-outline-primary")
              )
            ),

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

              # The footer is the plot's lower-left corner: the Add new drug
              # button opens the Add a dose dialog for a drug not yet in the
              # table (input$addNewDrug observer in app_server.R), so a drug
              # can be added without going back to the dose table; the hint
              # text follows it (.plot-footer in app.css).
              bslib::card_footer(
                class = "plot-footer small text-muted",
                actionButton(
                  "addNewDrug", "Add new drug",
                  icon = icon("plus"),
                  class = "btn-sm btn-outline-primary"
                ),
                span("Hover for precise concentration. Click to add new dose. Double click to edit or delete a drug's doses.")
              )
            )
          ),

          bslib::accordion(
            open = FALSE,
            bslib::accordion_panel(
              "References",
              icon = icon("book"),
              uiOutput("drug_references", class = "small")
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

      # The help: pages in inst/help plus pages generated from the drug library
      # and the teaching scenarios.  See R/help-content.R.
      helpNavPanel(),

      bslib::nav_spacer(),
      bslib::nav_menu(
        "Settings",
        icon = icon("gear"),
        bslib::nav_item(
          actionLink(
            "editDrugs",
            "Drug Library",
            icon = icon("fas fa-capsules")
          )
        ),
        bslib::nav_item(
          actionLink(
            "editThresholds",
            "Drug Thresholds",
            icon = icon("fas fa-bullseye")
          )
        )
      ),
      bslib::nav_item(
        tags$a(
          icon("github"),
          "Source",
          href = config$source_link,
          target = "_blank",
          # The installed version and commit, as a tooltip (stanpumpRBuild())
          title = stanpumpRBuild()$label
        )
      )
    )
  }
}
