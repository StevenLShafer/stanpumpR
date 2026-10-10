# -----------------------------------------------------------------------------
# Teaching scenarios: a registry of simulations the help can load into the app
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code (Claude Fable 5.1), 2026-10-06, at the request of
# Steven L. Shafer.  Verified by tests/testthat/test-help-scenarios.R, which
# checks every scenario against the drug library (drug names, units, limits)
# and runs each one through the simulation engine.
#
# A scenario is a patient, a dose table, an optional event table and the graph
# options that make its point.  Its help page is generated from this registry
# (R/help-drugs.R has the drug pages; helpScenarioPageHTML() below has these)
# and followed by the narrative in inst/help/scenarios/<id>.md, which says what
# to look for and what to try next.  "Load into the simulator" on that page
# calls applyHelpScenario(), which sets the inputs and the tables and switches
# to the Simulator tab.
#
# To add a scenario: add a helpScenario() call to helpScenarios() and write
# inst/help/scenarios/<id>.md.  The tests insist on both.
# -----------------------------------------------------------------------------

HELP_SCENARIO_GROUPS <- c(
  "Intravenous basics",
  "Opioids",
  "Oral analgesics",
  "Interactions",
  "Special populations",
  "Recovery and emergence",
  "Inhaled anesthetics",
  "Regional anesthesia",
  "Long-term therapy"
)

#' Default graph options for a scenario
#' @noRd
helpScenarioDefaultOptions <- function() {
  list(
    # The Time units the scenario is shown in.  Its dose and event times, and
    # maximum, are minutes whatever the unit; maximum must be one of the
    # unit's Max time choices.
    timeUnits = TIME_UNIT_DEFAULT,
    maximum = 60,
    typical = "Range",
    normalization = NORMALIZE_NONE,
    plasmaLinetype = "blank",
    effectsiteLinetype = "solid",
    addedPlots = character(0),
    showThreshold = FALSE,
    logY = FALSE,
    opioidMacInteraction = FALSE
  )
}

#' Build one scenario
#'
#' @param id page id, lower case with hyphens; the narrative is
#'   inst/help/scenarios/<id>.md
#' @param title shown in the sidebar and as the page title
#' @param group one of HELP_SCENARIO_GROUPS
#' @param summary one sentence: the point the scenario makes
#' @param age,weight,height,sex the patient (years, kg, cm, "male"/"female")
#' @param doses data frame with Drug, Time (minutes), Dose, Units
#' @param events data frame with Time (minutes) and Event, or NULL
#' @param ... graph options overriding helpScenarioDefaultOptions()
#' @noRd
helpScenario <- function(id, title, group, summary,
                         age = 40, weight = 70, height = 170, sex = SEX_MALE,
                         cyp2d6 = CYP2D6_DEFAULT, adjustToFFM = TRUE,
                         doses, events = NULL, cyp2c19 = CYP2C19_DEFAULT, ...) {
  options <- utils::modifyList(helpScenarioDefaultOptions(), list(...))
  doses <- data.frame(
    Drug = as.character(doses$Drug),
    Time = as.numeric(doses$Time),
    Dose = as.numeric(doses$Dose),
    Units = as.character(doses$Units),
    stringsAsFactors = FALSE
  )
  if (!is.null(events)) {
    events <- data.frame(Time = as.numeric(events$Time), Event = as.character(events$Event),
                         stringsAsFactors = FALSE)
  }
  list(
    id = id, title = title, group = group, summary = summary,
    patient = list(age = age, weight = weight, height = height, sex = sex,
                   cyp2d6 = cyp2d6, cyp2c19 = cyp2c19, adjustToFFM = adjustToFFM),
    doses = doses, events = events, options = options
  )
}

helpDoses <- function(...) {
  # A compact way to write a dose table: rows of c(drug, time, dose, units)
  rows <- list(...)
  data.frame(
    Drug = vapply(rows, `[[`, character(1), 1),
    Time = as.numeric(vapply(rows, `[[`, character(1), 2)),
    Dose = as.numeric(vapply(rows, `[[`, character(1), 3)),
    Units = vapply(rows, `[[`, character(1), 4),
    stringsAsFactors = FALSE
  )
}

helpEvents <- function(...) {
  rows <- list(...)
  data.frame(
    Time = as.numeric(vapply(rows, `[[`, character(1), 1)),
    Event = vapply(rows, `[[`, character(1), 2),
    stringsAsFactors = FALSE
  )
}

#' The teaching scenarios
#'
#' @returns a list of scenarios, in the order they appear in the help
#' @noRd
helpScenarios <- function() {
  list(
    # --- Intravenous basics --------------------------------------------------
    helpScenario(
      "propofol-bolus",
      "A propofol bolus: plasma versus effect site",
      "Intravenous basics",
      "Why the effect site lags the plasma, and what the time to peak effect means.",
      doses = helpDoses(c("propofol", 0, 2, "mg/kg")),
      maximum = 60, plasmaLinetype = "dashed", typical = "Range"
    ),
    helpScenario(
      "propofol-induction-maintenance",
      "Propofol induction and maintenance infusion",
      "Intravenous basics",
      "A bolus bridges the gap while an infusion climbs towards a steady state it never quite reaches.",
      doses = helpDoses(
        c("propofol", 0, 2, "mg/kg"),
        c("propofol", 0, 150, "mcg/kg/min"),
        c("propofol", 30, 100, "mcg/kg/min"),
        c("propofol", 90, 0, "mcg/kg/min")
      ),
      maximum = 120, plasmaLinetype = "dashed", showThreshold = TRUE
    ),
    helpScenario(
      "methadone-bolus",
      "Methadone: fast onset, very long duration",
      "Intravenous basics",
      "A drug whose effect site equilibrates in minutes but whose elimination takes a day.",
      doses = helpDoses(c("methadone", 0, 10, "mg")),
      maximum = 1440, plasmaLinetype = "dashed"
    ),
    helpScenario(
      "ketamine-infusion",
      "Ketamine: analgesic bolus and infusion",
      "Intravenous basics",
      "Holding a sub-anesthetic analgesic concentration with a small bolus and a low-rate infusion.",
      doses = helpDoses(
        c("ketamine", 0, 0.5, "mg/kg"),
        c("ketamine", 0, 0.25, "mg/kg/hr"),
        c("ketamine", 120, 0, "mg/kg/hr")
      ),
      maximum = 240, plasmaLinetype = "dashed"
    ),
    helpScenario(
      "lidocaine-infusion",
      "Lidocaine infusion for analgesia",
      "Intravenous basics",
      "Staying inside a therapeutic window that has toxicity not far above it.",
      doses = helpDoses(
        c("lidocaine", 0, 100, "mg"),
        c("lidocaine", 0, 100, "mg/hr"),
        c("lidocaine", 240, 0, "mg/hr")
      ),
      maximum = 360, plasmaLinetype = "dashed"
    ),
    helpScenario(
      "remimazolam-sedation",
      "Remimazolam for procedural sedation",
      "Intravenous basics",
      "Titrating a short-acting benzodiazepine with repeated small boluses.",
      doses = helpDoses(
        c("remimazolam", 0, 5, "mg"),
        c("remimazolam", 5, 2.5, "mg"),
        c("remimazolam", 10, 2.5, "mg")
      ),
      maximum = 60, plasmaLinetype = "dashed"
    ),
    # The product label's first 24 hours of intravenous amiodarone: 150 mg
    # over 10 minutes, 360 mg over 6 hours, 540 mg over 18 hours (Claude Code,
    # 2026-10-08, at the request of Steven L. Shafer)
    helpScenario(
      "amiodarone-iv-loading",
      "Amiodarone IV: the label's 24-hour loading infusion",
      "Intravenous basics",
      "A rapid load, a slow load and a maintenance rate: why the concentration dips when the rate is halved.",
      age = 60, weight = 70, height = 170, sex = SEX_MALE,
      doses = helpDoses(
        c("amiodaroneIV",   0,  15, "mg/min"),
        c("amiodaroneIV",  10,   1, "mg/min"),
        c("amiodaroneIV", 370, 0.5, "mg/min")
      ),
      timeUnits = "hours", maximum = 1440
    ),

    # --- Opioids -------------------------------------------------------------
    helpScenario(
      "context-sensitive-opioids",
      "Context-sensitive decrement: four opioids",
      "Opioids",
      "Four opioids infused for three hours and stopped: how differently they wear off.",
      doses = helpDoses(
        c("fentanyl", 0, 2, "mcg/kg/hr"),
        c("fentanyl", 180, 0, "mcg/kg/hr"),
        c("alfentanil", 0, 30, "mcg/kg/hr"),
        c("alfentanil", 180, 0, "mcg/kg/hr"),
        c("sufentanil", 0, 0.3, "mcg/kg/hr"),
        c("sufentanil", 180, 0, "mcg/kg/hr"),
        c("remifentanil", 0, 0.2, "mcg/kg/min"),
        c("remifentanil", 180, 0, "mcg/kg/min")
      ),
      maximum = 360, normalization = "Peak effect site", typical = "none"
    ),
    helpScenario(
      "opioid-meac",
      "Opioid potency on one axis: the MEAC panel",
      "Opioids",
      "Morphine, hydromorphone and fentanyl boluses compared as multiples of their analgesic concentrations.",
      doses = helpDoses(
        c("morphine", 0, 10, "mg"),
        c("hydromorphone", 0, 1.5, "mg"),
        c("fentanyl", 0, 100, "mcg")
      ),
      maximum = 240, addedPlots = PLOT_ID_MEAC
    ),
    helpScenario(
      "tci-propofol",
      "Target-controlled infusion of propofol",
      "Intravenous basics",
      "An effect-site target of 3 mcg/mL, stepped down to 2 and then off: the pump finds the doses, and the rate panel shows them.",
      doses = helpDoses(
        c("propofol", 0, 3, "Effect site target"),
        c("propofol", 30, 2, "Effect site target"),
        c("propofol", 60, 0, "Effect site target")
      ),
      maximum = 120, plasmaLinetype = "dashed", showThreshold = TRUE
    ),

    # --- Oral analgesics -----------------------------------------------------
    helpScenario(
      "oral-oxycodone",
      "Oral oxycodone: absorption sets the pace",
      "Oral analgesics",
      "Two oral doses six hours apart, with the rise governed by absorption rather than distribution, and the oxymorphone formed from them.",
      doses = helpDoses(
        c("oxycodone", 0, 10, "mg PO"),
        c("oxycodone", 360, 10, "mg PO")
      ),
      maximum = 720, plasmaLinetype = "dashed"
    ),
    helpScenario(
      "codeine-cyp2d6",
      "Codeine: a prodrug, and the CYP2D6 phenotype",
      "Oral analgesics",
      "Sixty milligrams of oral codeine produces a morphine curve; change the CYP 2D6 field and watch it change.",
      doses = helpDoses(
        c("codeine", 0, 60, "mg PO")
      ),
      maximum = 360, plasmaLinetype = "dashed", addedPlots = PLOT_ID_MEAC
    ),
    helpScenario(
      "tramadol-oral",
      "Tramadol and its metabolite desmetramadol",
      "Oral analgesics",
      "An oral dose of tramadol, with the opioid effect carried by the desmetramadol formed from it.",
      doses = helpDoses(
        c("tramadol", 0, 100, "mg PO")
      ),
      maximum = 360, plasmaLinetype = "dashed", addedPlots = PLOT_ID_MEAC
    ),
    helpScenario(
      "gabapentin-saturable-absorption",
      "Gabapentin: why 1200 mg is not twice 600 mg",
      "Oral analgesics",
      "Gabapentin's absorption saturates: 600 mg and then, after a washout, 1200 mg by mouth, and the larger dose peaks less than half again as high.",
      doses = helpDoses(
        c("gabapentin", 0, 600, "mg PO"),
        c("gabapentin", 2160, 1200, "mg PO")
      ),
      # Gabapentin has no effect site, so the plasma line carries the curve.
      # Two days is longer than minutes and hours go (24 hours), so in days:
      # the second dose reads 1.5.
      timeUnits = "days", maximum = 2880, plasmaLinetype = "solid"
    ),
    helpScenario(
      "pregabalin-linear-absorption",
      "Pregabalin: twice the dose, twice the concentration",
      "Oral analgesics",
      "Pregabalin 150 mg and then, after a washout, 300 mg by mouth: unlike gabapentin, the larger dose peaks twice as high, and the effect site peaks hours after the plasma.",
      doses = helpDoses(
        c("pregabalin", 0, 150, "mg PO"),
        c("pregabalin", 2160, 300, "mg PO")
      ),
      # Two days, so in days, as for gabapentin: the second dose reads 1.5.
      timeUnits = "days", maximum = 2880, plasmaLinetype = "dashed"
    ),
    helpScenario(
      "acetaminophen-headache",
      "Acetaminophen 500 mg for a headache",
      "Oral analgesics",
      "One 500 mg tablet: the plasma peaks in half an hour, but the effect site peaks nearly two hours after the dose, and only just reaches the lower edge of the band.",
      doses = helpDoses(
        c("acetaminophen", 0, 500, "mg PO")
      ),
      timeUnits = "hours", maximum = 480, plasmaLinetype = "dashed"
    ),
    helpScenario(
      "acetaminophen-arthritis-qid",
      "Acetaminophen 1 g four times a day for arthritis",
      "Oral analgesics",
      "The labelled maximum of 4 g a day: a short half-life against a six-hour interval, so the plasma swings six-fold and the effect site rises and falls with every dose.",
      doses = helpDoses(
        c("acetaminophen", 0, 1000, "mg PO qid")
      ),
      timeUnits = "hours", maximum = 1440, plasmaLinetype = "dashed"
    ),
    helpScenario(
      "ibuprofen-with-acetaminophen",
      "Ibuprofen 400 mg with acetaminophen 1 g",
      "Oral analgesics",
      "Two common tablets taken together and cleared at much the same rate, yet ibuprofen's effect site stays above its threshold for six and a half hours and acetaminophen's for two and a half.",
      doses = helpDoses(
        c("ibuprofen", 0, 400, "mg PO"),
        c("acetaminophen", 0, 1000, "mg PO")
      ),
      timeUnits = "hours", maximum = 720, plasmaLinetype = "dashed",
      showThreshold = TRUE
    ),

    # --- Interactions --------------------------------------------------------
    helpScenario(
      "tiva-remifentanil-propofol",
      "TIVA: propofol with remifentanil and the interaction panel",
      "Interactions",
      "How a modest opioid concentration lets a much lower propofol concentration block the response to laryngoscopy.",
      doses = helpDoses(
        c("propofol", 0, 1.5, "mg/kg"),
        c("propofol", 0, 120, "mcg/kg/min"),
        c("propofol", 60, 0, "mcg/kg/min"),
        c("remifentanil", 0, 1, "mcg/kg"),
        c("remifentanil", 0, 0.15, "mcg/kg/min"),
        c("remifentanil", 60, 0, "mcg/kg/min")
      ),
      events = helpEvents(c(2, "Intubation"), c(70, "Emergence")),
      maximum = 120, addedPlots = c(PLOT_ID_INTERACTION, PLOT_ID_EVENTS)
    ),
    helpScenario(
      "opioid-mac-interaction",
      "Opioids lower MAC",
      "Interactions",
      "The same sevoflurane concentration counts for more MAC equivalents once remifentanil is on board.",
      doses = helpDoses(
        c("oxygen", 0, 4, "L/min"),
        c("sevoflurane", 0, 1.5, "%"),
        c("ventilation", 0, 6, "L/min"),
        c("remifentanil", 0, 1, "mcg/kg"),
        c("remifentanil", 0, 0.1, "mcg/kg/min")
      ),
      maximum = 60, addedPlots = PLOT_ID_MEAC, opioidMacInteraction = TRUE
    ),

    # --- Special populations -------------------------------------------------
    helpScenario(
      "age-and-propofol",
      "Age and propofol: the same dose in a 25-year-old and an 85-year-old",
      "Special populations",
      "Change the age and apply: the Eleveld model shows why the elderly need less.",
      age = 25, weight = 70, height = 175, sex = SEX_MALE,
      doses = helpDoses(
        c("propofol", 0, 2, "mg/kg"),
        c("propofol", 0, 100, "mcg/kg/min"),
        c("propofol", 60, 0, "mcg/kg/min")
      ),
      maximum = 120
    ),
    helpScenario(
      "child-propofol",
      "Propofol in a five-year-old",
      "Special populations",
      "Children need more propofol per kilogram, and the model says how much more.",
      age = 5, weight = 20, height = 110, sex = SEX_MALE,
      doses = helpDoses(
        c("propofol", 0, 3, "mg/kg"),
        c("propofol", 0, 200, "mcg/kg/min"),
        c("propofol", 60, 0, "mcg/kg/min")
      ),
      maximum = 120
    ),
    helpScenario(
      "dexmedetomidine-loading",
      "Dexmedetomidine: loading dose, then infusion",
      "Special populations",
      "A slowly equilibrating drug given to a 70-year-old: the loading dose, the plateau and the long tail.",
      age = 70, weight = 75, height = 172, sex = SEX_FEMALE,
      doses = helpDoses(
        c("dexmedetomidine", 0, 1, "mcg/kg"),
        c("dexmedetomidine", 0, 0.5, "mcg/kg/hr"),
        c("dexmedetomidine", 120, 0, "mcg/kg/hr")
      ),
      maximum = 240, plasmaLinetype = "dashed", showThreshold = TRUE
    ),

    # --- Recovery and emergence ----------------------------------------------
    helpScenario(
      "rocuronium-recovery",
      "Rocuronium: intubating dose and time until threshold",
      "Recovery and emergence",
      "Reading the time-until-threshold line as the time to return of neuromuscular function.",
      doses = helpDoses(c("rocuronium", 0, 42, "mg")),
      maximum = 120, showThreshold = TRUE
    ),
    helpScenario(
      "naloxone-morphine",
      "Naloxone after morphine: mismatched durations",
      "Recovery and emergence",
      "Naloxone wears off long before the morphine it reversed: the pharmacokinetics of renarcotization.",
      doses = helpDoses(
        c("morphine", 0, 10, "mg"),
        c("naloxone", 60, 400, "mcg")
      ),
      maximum = 240, normalization = "Peak effect site", typical = "none"
    ),
    helpScenario(
      "emergence-sevoflurane",
      "Emergence from sevoflurane",
      "Recovery and emergence",
      "Two hours of sevoflurane, then the vaporizer off and the flow up: how long until 0.1 MAC?",
      doses = helpDoses(
        c("oxygen", 0, 2, "L/min"),
        c("sevoflurane", 0, 2, "%"),
        c("ventilation", 0, 6, "L/min"),
        c("sevoflurane", 120, 0, "%"),
        c("oxygen", 120, 8, "L/min")
      ),
      maximum = 240, showThreshold = TRUE
    ),
    helpScenario(
      "emergence-desflurane",
      "Emergence from desflurane",
      "Recovery and emergence",
      "The same two-hour anesthetic with desflurane, for comparison with sevoflurane.",
      doses = helpDoses(
        c("oxygen", 0, 2, "L/min"),
        c("desflurane", 0, 6, "%"),
        c("ventilation", 0, 6, "L/min"),
        c("desflurane", 120, 0, "%"),
        c("oxygen", 120, 8, "L/min")
      ),
      maximum = 240, showThreshold = TRUE
    ),

    # --- Inhaled anesthetics -------------------------------------------------
    helpScenario(
      "sevoflurane-washin",
      "Sevoflurane wash-in at high, then low, fresh gas flow",
      "Inhaled anesthetics",
      "Alveolar concentration chases inspired concentration, and rebreathing slows it once the flow drops.",
      doses = helpDoses(
        c("oxygen", 0, 6, "L/min"),
        c("sevoflurane", 0, 2, "%"),
        c("ventilation", 0, 6, "L/min"),
        c("oxygen", 15, 1, "L/min")
      ),
      maximum = 60, plasmaLinetype = "dashed"
    ),
    helpScenario(
      "second-gas-effect",
      "The second gas effect: sevoflurane with nitrous oxide",
      "Inhaled anesthetics",
      "Nitrous oxide taken up in bulk concentrates the sevoflurane left behind in the alveolus.",
      doses = helpDoses(
        c("oxygen", 0, 2, "L/min"),
        c("nitrousOxide", 0, 4, "L/min"),
        c("sevoflurane", 0, 2, "%"),
        c("ventilation", 0, 6, "L/min")
      ),
      maximum = 60, plasmaLinetype = "dashed"
    ),
    helpScenario(
      "obesity-fat-free-mass",
      "Obesity: dosing by weight against fat-free mass",
      "Special populations",
      "Per-kilogram boluses of fentanyl and sufentanil in a 120 kg man, with the models scaled to his fat-free mass; untick the box to see total-weight scaling.",
      age = 50, weight = 120, height = 170, sex = SEX_MALE,
      doses = helpDoses(
        c("fentanyl", 0, 2, "mcg/kg"),
        c("sufentanil", 0, 0.2, "mcg/kg")
      ),
      maximum = 240, plasmaLinetype = "dashed"
    ),
    helpScenario(
      "mac-and-age",
      "MAC and age",
      "Inhaled anesthetics",
      "Two per cent sevoflurane is more than one MAC in an 80-year-old; change the age and watch the MAC panel.",
      age = 80, weight = 70, height = 170, sex = SEX_MALE,
      doses = helpDoses(
        c("oxygen", 0, 6, "L/min"),
        c("sevoflurane", 0, 2, "%"),
        c("ventilation", 0, 6, "L/min")
      ),
      maximum = 60
    ),

    # --- Regional anesthesia -------------------------------------------------
    helpScenario(
      "regional-anesthesia-absorption",
      "Local anesthetics after a nerve block",
      "Regional anesthesia",
      "Lidocaine, bupivacaine and ropivacaine injected into tissue (RA) are absorbed into the circulation over hours, so the systemic peak comes late and its height and timing differ from drug to drug.",
      # 65 years: the ropivacaine disposition is that of patients 61 and over.
      age = 65,
      doses = helpDoses(
        c("lidocaine", 0, 400, "mg RA"),
        c("bupivacaine", 0, 150, "mg RA"),
        c("ropivacaine", 0, 175, "mg RA")
      ),
      # Bupivacaine and ropivacaine have no effect site, so the plasma line
      # carries every curve; lidocaine's effect site is its systemic one.
      timeUnits = "hours", maximum = 720,
      plasmaLinetype = "solid", effectsiteLinetype = "blank"
    ),
    helpScenario(
      "mepivacaine-two-depot-absorption",
      "Mepivacaine: fast and slow absorption from one block",
      "Regional anesthesia",
      "Mepivacaine injected for an axillary block is absorbed through a fast and a slow depot in parallel, so the plasma rises within minutes, holds for an hour and then falls slowly.",
      doses = helpDoses(
        c("mepivacaine", 0, 600, "mg RA")
      ),
      # Mepivacaine has no effect site, so the plasma line carries the curve.
      timeUnits = "hours", maximum = 480,
      plasmaLinetype = "solid", effectsiteLinetype = "blank"
    ),
    helpScenario(
      "lidocaine-intravascular-injection",
      "Lidocaine: a block dose injected into a vein",
      "Regional anesthesia",
      "The 400 mg of lidocaine meant for a nerve block goes into a vein instead: the plasma starts far above anything the block produces, and the effect site passes 5 mcg/mL within 2 minutes.",
      doses = helpDoses(
        # The block dose, given intravenously ("mg") rather than into the
        # tissue ("mg RA"); the narrative switches it back.
        c("lidocaine", 0, 400, "mg")
      ),
      # A log axis keeps the first-minute plasma (65 mcg/mL) from flattening
      # the effect site, which carries the point.
      maximum = 60, plasmaLinetype = "dashed", effectsiteLinetype = "solid",
      logY = TRUE
    ),
    helpScenario(
      "bupivacaine-perineural-infusion",
      "Bupivacaine: a continuous perineural infusion",
      "Regional anesthesia",
      "A 100 mg block followed by a 48-hour catheter infusion of 10 mg/hr: the block sets the peak, the infusion settles at rate / clearance, and the concentration falls within hours of stopping.",
      doses = helpDoses(
        c("bupivacaine", 0, 100, "mg RA"),
        c("bupivacaine", 0, 10, "mg/hr RA"),
        c("bupivacaine", 2880, 0, "mg/hr RA")
      ),
      # Bupivacaine has no effect site, so the plasma line carries the curve.
      # Three days, in days: the infusion stops at 2.
      timeUnits = "days", maximum = 4320,
      plasmaLinetype = "solid", effectsiteLinetype = "blank"
    ),
    # --- Long-term therapy ---------------------------------------------------
    helpScenario(
      "amiodarone-pollak-regimen",
      "Amiodarone: Pollak's stepped loading regimen",
      "Long-term therapy",
      "Loading a drug with a huge peripheral volume: a year of amiodarone held inside its window.",
      age = 60, weight = 70, height = 170, sex = SEX_MALE,
      doses = helpDoses(
        c("amiodarone",      0, 1600, "mg/day PO"),
        c("amiodarone",   2880, 1200, "mg/day PO"),
        c("amiodarone",  10080, 1000, "mg/day PO"),
        c("amiodarone",  20160,  800, "mg/day PO"),
        c("amiodarone",  30240,  600, "mg/day PO"),
        c("amiodarone",  40320,  400, "mg/day PO"),
        c("amiodarone", 129600,  343, "mg/day PO")
      ),
      timeUnits = "days", maximum = 525600
    ),
    helpScenario(
      "amiodarone-label-loading",
      "Amiodarone: the label's high-dose loading regimen",
      "Long-term therapy",
      "A loading dose that is right in the first week overshoots by the third, and a maintenance dose above clearance x target never settles inside the window.",
      age = 60, weight = 70, height = 170, sex = SEX_MALE,
      doses = helpDoses(
        c("amiodarone",     0, 1600, "mg/day PO"),
        c("amiodarone", 30240,  800, "mg/day PO"),
        c("amiodarone", 73440,  600, "mg/day PO")
      ),
      timeUnits = "days", maximum = 525600
    ),
    helpScenario(
      "amiodarone-washout",
      "Amiodarone: stopping after six months",
      "Long-term therapy",
      "A fast early fall and then months of washout: the context-sensitive decrement of amiodarone.",
      age = 60, weight = 70, height = 170, sex = SEX_MALE,
      doses = helpDoses(
        c("amiodarone",      0, 1600, "mg/day PO"),
        c("amiodarone",   2880, 1200, "mg/day PO"),
        c("amiodarone",  10080, 1000, "mg/day PO"),
        c("amiodarone",  20160,  800, "mg/day PO"),
        c("amiodarone",  30240,  600, "mg/day PO"),
        c("amiodarone",  40320,  400, "mg/day PO"),
        c("amiodarone", 129600,  343, "mg/day PO"),
        c("amiodarone", 262080,    0, "mg/day PO")
      ),
      timeUnits = "days", maximum = 525600,
      showThreshold = TRUE
    )
  )
}

helpScenarioIds <- function() {
  vapply(helpScenarios(), `[[`, character(1), "id")
}

helpScenarioById <- function(id) {
  if (!is.character(id) || length(id) != 1 || is.na(id)) return(NULL)
  for (s in helpScenarios()) if (identical(s$id, id)) return(s)
  NULL
}

#' Problems with a scenario definition, as a character vector (empty = fine)
#'
#' Used by the tests, and by applyHelpScenario() as a guard.
#' @noRd
helpScenarioCheck <- function(s, drugDefaults = getDrugDefaultsGlobal(),
                              eventDefaults = getEventDefaults()) {
  problems <- character(0)
  say <- function(...) problems <<- c(problems, paste0(...))

  if (!grepl("^[a-z0-9]+(-[a-z0-9]+)*$", s$id)) say("id is not a lower-case slug: ", s$id)
  if (!nzchar(s$title)) say("title is empty")
  if (!s$group %in% HELP_SCENARIO_GROUPS) say("unknown group: ", s$group)
  if (!nzchar(s$summary)) say("summary is empty")

  p <- s$patient
  if (!is_valid_number(p$age, MIN_AGE, MAX_AGE)) say("age out of range: ", p$age)
  if (!is_valid_number(p$weight, MIN_WEIGHT, MAX_WEIGHT)) say("weight out of range: ", p$weight)
  if (!is_valid_number(p$height, MIN_HEIGHT, MAX_HEIGHT)) say("height out of range: ", p$height)
  if (!p$sex %in% SEX_VALUES) say("invalid sex: ", p$sex)
  if (!p$cyp2d6 %in% CYP2D6_VALUES) say("invalid cyp2d6: ", p$cyp2d6)
  if (!p$cyp2c19 %in% CYP2C19_VALUES) say("invalid cyp2c19: ", p$cyp2c19)
  if (!is.logical(p$adjustToFFM) || length(p$adjustToFFM) != 1) say("adjustToFFM is not a single logical")

  o <- s$options
  if (!identical(validTimeUnit(o$timeUnits), o$timeUnits)) say("invalid timeUnits: ", o$timeUnits)
  if (!isTRUE(o$maximum %in% MAX_TIMES[[validTimeUnit(o$timeUnits)]]$times)) {
    say("maximum is not one of the Max time choices for ", o$timeUnits, ": ", o$maximum)
  }
  if (!o$typical %in% c("none", "Mid", "Range")) say("invalid typical: ", o$typical)
  if (!o$normalization %in% c(NORMALIZE_NONE, "Peak plasma", "Peak effect site")) say("invalid normalization: ", o$normalization)
  lineTypes <- c("blank", "solid", "dashed", "dotted", "dotdash")
  if (!o$plasmaLinetype %in% lineTypes) say("invalid plasmaLinetype: ", o$plasmaLinetype)
  if (!o$effectsiteLinetype %in% lineTypes) say("invalid effectsiteLinetype: ", o$effectsiteLinetype)
  if (o$plasmaLinetype == "blank" && o$effectsiteLinetype == "blank") say("both lines are blank")
  if (!all(o$addedPlots %in% c(PLOT_ID_MEAC, PLOT_ID_INTERACTION, PLOT_ID_EVENTS))) say("invalid addedPlots")
  if (o$showThreshold && o$normalization != NORMALIZE_NONE) say("showThreshold needs normalization none")
  if (o$logY && (o$showThreshold || any(c(PLOT_ID_EVENTS, PLOT_ID_INTERACTION) %in% o$addedPlots))) {
    say("logY is unavailable with the threshold, events or interaction panels")
  }
  for (name in c("showThreshold", "logY", "opioidMacInteraction")) {
    if (!is.logical(o[[name]]) || length(o[[name]]) != 1) say(name, " is not a single logical")
  }

  d <- s$doses
  if (!is.data.frame(d) || nrow(d) == 0) {
    say("no doses")
  } else {
    if (!all(c("Drug", "Time", "Dose", "Units") %in% names(d))) say("dose table lacks columns")
    for (i in seq_len(nrow(d))) {
      drug <- d$Drug[i]
      if (!drug %in% drugDefaults$Drug) {
        say("unknown drug: ", drug)
        next
      }
      units <- helpDrugUnits(drugDefaults[drugDefaults$Drug == drug, ][1, ])
      if (!d$Units[i] %in% units) say(drug, ": unit not offered: ", d$Units[i])
      if (!is_valid_number(d$Dose[i], 0, MAX_DOSE_VALUE)) say(drug, ": invalid dose: ", d$Dose[i])
      if (!is_valid_number(d$Time[i], 0, Inf)) say(drug, ": invalid time: ", d$Time[i])
      else if (d$Time[i] >= o$maximum) say(drug, ": dose at ", d$Time[i], " is beyond Max time ", o$maximum)
    }
    gases <- d$Drug[isGasDrug(d$Drug)]
    if (length(gases) > 0 && !"ventilation" %in% d$Drug) say("a gas scenario should set ventilation explicitly")
    # The app's time-units rule: no TCI target rows and no inhaled agents on
    # a plot of more than a week (timeUnitViolation() in R/app_server.R)
    if ((length(gases) > 0 || any(d$Units %in% tciUnits)) &&
        isTRUE(o$maximum > ACUTE_MAX_PLOT_MINUTES)) {
      say("TCI targets and inhaled agents need a Max time of 7 days or less")
    }
    if (PLOT_ID_INTERACTION %in% o$addedPlots) {
      opioids <- drugDefaults$Drug[!is.na(drugDefaults$MEAC) & drugDefaults$MEAC > 0]
      if (!"propofol" %in% d$Drug || !any(d$Drug %in% opioids)) say("the interaction panel needs propofol and an opioid")
    }
  }

  e <- s$events
  if (!is.null(e)) {
    if (!all(c("Time", "Event") %in% names(e))) say("event table lacks columns")
    for (t in e$Time) {
      if (!is_valid_number(t, 0, Inf)) say("invalid event time: ", t)
      else if (t >= o$maximum) say("event at ", t, " is beyond Max time ", o$maximum)
    }
    bad <- setdiff(e$Event, eventDefaults$Event)
    if (length(bad)) say("unknown events: ", paste(bad, collapse = ", "))
    if (!PLOT_ID_EVENTS %in% o$addedPlots) say("events are given but the Events panel is not shown")
  }

  problems
}

#' The dose table a scenario puts into the app
#'
#' Character columns, as the dose table holds them, with blank rows after the
#' doses so that there is room to type, as doseTableInit has.  The times are
#' written in the scenario's time unit, elapsed (the format applyHelpScenario()
#' records for them).
#' @noRd
helpScenarioDoseTable <- function(s, blankRows = 3) {
  d <- s$doses
  d <- d[order(d$Time, d$Drug), ]
  dt <- data.frame(
    Drug = d$Drug,
    Time = minutesToDisplayTime(d$Time, s$options$timeUnits),
    Dose = as.character(d$Dose),
    Units = d$Units,
    stringsAsFactors = FALSE
  )
  if (blankRows > 0) {
    blanks <- doseTableNewRow[rep(1, blankRows), ]
    dt <- rbind(dt, blanks)
  }
  rownames(dt) <- NULL
  dt
}

helpScenarioEventTable <- function(s) {
  if (is.null(s$events) || nrow(s$events) == 0) return(eventTableInit)
  data.frame(Time = s$events$Time, Event = s$events$Event, stringsAsFactors = FALSE)
}

#' Put a scenario into the running app
#'
#' Sets the patient, the graph options, the event table and the dose table.
#' The dose table goes in through doseTable(), so it is applied at once (no
#' draft to confirm) and the undo history starts afresh, exactly as a URL
#' restore does.
#'
#' With `timeApi` (app_server()'s), the time unit, elapsed time and Max time
#' are set through it, and the dose table is written together with the format
#' its times are in, so that the app has nothing to convert when the browser
#' reports the new settings.  The order does not matter: it all reaches the
#' browser in one message.
#'
#' @param session the Shiny session
#' @param s a scenario from helpScenarios()
#' @param doseTable,eventTable the app's reactiveVal()s
#' @param timeApi list(setDoseTable, showTimeSettings), or NULL to write the
#'   tables directly (tests)
#' @noRd
applyHelpScenario <- function(session, s, doseTable, eventTable, timeApi = NULL) {
  p <- s$patient
  o <- s$options

  updateNumericInput(session, "age", value = p$age)
  updateRadioButtons(session, "ageUnit", selected = as.character(UNIT_YEAR))
  updateNumericInput(session, "weight", value = p$weight)
  updateRadioButtons(session, "weightUnit", selected = as.character(UNIT_KG))
  updateNumericInput(session, "height", value = p$height)
  updateRadioButtons(session, "heightUnit", selected = as.character(UNIT_CM))
  shinyWidgets::updateRadioGroupButtons(session, "sex", selected = p$sex)
  updateSelectInput(session, "cyp2d6", selected = p$cyp2d6)
  updateSelectInput(session, "cyp2c19", selected = p$cyp2c19)
  updateCheckboxInput(session, "adjustToFFM", value = p$adjustToFFM)

  if (is.null(timeApi)) {
    updateSelectInput(session, "maximum", selected = maxTimeValue(o$maximum))
  }
  updateSelectInput(session, "typical", selected = o$typical)
  updateSelectInput(session, "normalization", selected = o$normalization)
  shinyWidgets::updateRadioGroupButtons(session, "plasmaLinetype", selected = o$plasmaLinetype)
  shinyWidgets::updateRadioGroupButtons(session, "effectsiteLinetype", selected = o$effectsiteLinetype)
  updateCheckboxGroupInput(session, "addedPlots", selected = o$addedPlots)
  updateCheckboxInput(session, "showThreshold", value = o$showThreshold)
  updateCheckboxInput(session, "logY", value = o$logY)
  updateCheckboxInput(session, "opioidMacInteraction", value = o$opioidMacInteraction)

  eventTable(helpScenarioEventTable(s))
  if (is.null(timeApi)) {
    updateSelectizeInput(session, "timeMode", selected = "relative")
    doseTable(helpScenarioDoseTable(s))
  } else {
    timeApi$showTimeSettings(o$timeUnits, "relative", o$maximum)
    timeApi$setDoseTable(helpScenarioDoseTable(s), timeFormat(o$timeUnits, "relative"))
  }
  invisible(s)
}

# --- Pages -------------------------------------------------------------------

helpScenarioLoadButton <- function(s) {
  sprintf(
    '<p><a href="#" class="btn btn-primary help-scenario-btn" data-help-scenario="%s">%s Load into the simulator</a>
     <span class="small text-muted help-scenario-hint">Replaces the current patient, doses, events and graph options.</span></p>',
    s$id, as.character(icon("play"))
  )
}

#' The generated page for one scenario
#' @noRd
helpScenarioPageHTML <- function(id) {
  s <- helpScenarioById(id)
  if (is.null(s)) {
    return(sprintf("<p>There is no scenario called <code>%s</code>.</p>", htmltools::htmlEscape(id)))
  }
  p <- s$patient
  o <- s$options

  patient <- data.frame(
    Age = sprintf("%s years", helpFormatNumber(p$age)),
    Weight = sprintf("%s kg", helpFormatNumber(p$weight)),
    Height = sprintf("%s cm", helpFormatNumber(p$height)),
    Sex = p$sex,
    `CYP 2D6` = tools::toTitleCase(p$cyp2d6),
    `CYP 2C19` = tools::toTitleCase(p$cyp2c19),
    `Adjust weight to fat-free mass` = if (isTRUE(p$adjustToFFM)) "on" else "off",
    check.names = FALSE, stringsAsFactors = FALSE
  )
  # Times as the dose table will show them: in the scenario's time unit, in
  # full (helpFormatNumber()'s three significant figures would print 30240
  # minutes as 30,200)
  timeHeader <- paste0("Time (", o$timeUnits, ")")
  dosesShown <- s$doses
  dosesShown <- dosesShown[order(dosesShown$Time, dosesShown$Drug), ]
  dosesShown <- data.frame(
    Drug = helpDrugTitle(dosesShown$Drug),
    Time = minutesToDisplayTime(dosesShown$Time, o$timeUnits),
    Dose = helpFormatNumber(dosesShown$Dose, 4),
    Units = dosesShown$Units,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  names(dosesShown)[2] <- timeHeader
  optionRows <- c(
    "Time units" = o$timeUnits,
    "Max time" = maxTimeLabel(o$maximum, o$timeUnits),
    "Show typical" = if (o$typical == "none") "<none>" else o$typical,
    "Normalize to" = if (o$normalization == NORMALIZE_NONE) "<none>" else o$normalization,
    "Plasma line" = o$plasmaLinetype,
    "Effect site line" = o$effectsiteLinetype,
    "Additional plots" = if (length(o$addedPlots)) paste(o$addedPlots, collapse = ", ") else "none",
    "Time until threshold" = if (o$showThreshold) "on" else "off",
    "Log Y axis" = if (o$logY) "on" else "off",
    "Opioid - MAC interaction" = if (o$opioidMacInteraction) "on" else "off"
  )
  optionsTable <- data.frame(Option = names(optionRows), Setting = unname(optionRows),
                             stringsAsFactors = FALSE)
  eventsHTML <- ""
  if (!is.null(s$events) && nrow(s$events) > 0) {
    ev <- data.frame(Time = minutesToDisplayTime(s$events$Time, o$timeUnits), Event = s$events$Event,
                     check.names = FALSE, stringsAsFactors = FALSE)
    names(ev)[1] <- timeHeader
    eventsHTML <- helpTableHTML(ev, "Events")
  }

  narrative <- helpMarkdownToHTML(helpReadMarkdown(paste0("scenarios/", id)))

  paste0(
    "<p class='lead'>", htmltools::htmlEscape(s$summary), "</p>",
    helpScenarioLoadButton(s),
    helpH2("The simulation"),
    helpTableHTML(patient, "Patient"),
    helpTableHTML(dosesShown, "Doses"),
    eventsHTML,
    helpTableHTML(optionsTable, "Graph options"),
    narrative,
    helpH2("See also"),
    "<ul><li>", helpPageLink("scenarios/index", "All scenarios"), "</li>",
    paste0("<li>", vapply(unique(s$doses$Drug), function(d) {
      helpPageLink(paste0("drugs/", d), helpDrugTitle(d))
    }, character(1)), "</li>", collapse = ""),
    "</ul>"
  )
}

#' The scenario index page, grouped
#' @noRd
helpScenarioIndexHTML <- function() {
  scen <- helpScenarios()
  groups <- lapply(HELP_SCENARIO_GROUPS, function(g) {
    inGroup <- Filter(function(s) s$group == g, scen)
    if (length(inGroup) == 0) return("")
    items <- vapply(inGroup, function(s) {
      sprintf(
        '<li class="help-scenario-item"><a href="#" data-help-page="scenarios/%s"><strong>%s</strong></a>
         <div class="help-scenario-summary">%s</div>
         <a href="#" class="btn btn-outline-primary btn-sm help-scenario-btn" data-help-scenario="%s">Load</a></li>',
        s$id, htmltools::htmlEscape(s$title), htmltools::htmlEscape(s$summary), s$id)
    }, character(1))
    paste0(helpH2(g), "<ul class='help-scenario-list'>", paste(items, collapse = ""), "</ul>")
  })
  paste0(
    "<p>Each scenario is a complete simulation: a patient, a dose table and the graph options that ",
    "make its point. Open one to read what to look for, or press <em>Load</em> to put it straight ",
    "into the simulator and explore it yourself. Loading replaces whatever is in the simulator; ",
    "the browser's Back button will not undo it, so copy the URL first if you want to keep what ",
    "you have (see ", helpPageLink("sharing"), ").</p>",
    "<p>The scenarios are meant for teaching, not as dosing recommendations. The doses are ",
    "ordinary ones chosen to show a pharmacokinetic point clearly; see ", helpPageLink("cautions"), ".</p>",
    paste(groups, collapse = "")
  )
}
