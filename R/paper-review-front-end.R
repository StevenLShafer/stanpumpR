# Single Paper Review: front-end (recirculatory) models.
#
# A paper that models intravascular mixing (Krejcie, Avram and Henthorn's
# recirculatory models; Henthorn 2018's front-end mixing compartment; Upton's
# two-compartment recirculatory model) cannot be put into the library's
# mammillary engine.  The extraction carries such a model in its `front_end`
# block (paperReviewFrontEndSchema()), which this file turns into a model of
# the front-end engine (R/frontEndKinetics.R), checks, and simulates against
# the paper's arterial and venous figures.  The ordinary review of the same
# paper goes on as before on the mammillary part where there is one; this adds
# the section "Front-end kinetics" beside it.  Nothing here is evaluated from
# the reply: the block is numbers and closed choices, converted in R.
#
# Claude Code, 2026-11-19, at the request of Steven L. Shafer
# (docs/front-end-kinetics.md).

PAPER_REVIEW_FRONT_END_SECTION <- "Front-end kinetics"

# The sampling sites a front-end figure series may plot, and the engine's
# observation each is compared with.  "unstated" takes the model's own site.
PAPER_REVIEW_SAMPLING_SITES <- c("arterial", "venous", "mixed_venous", "unstated")
PAPER_REVIEW_FRONT_END_OBSERVATION <- c(arterial = "arterial", venous = "venous",
                                        mixed_venous = "mixed_venous")

# A quantity of the front-end block: printed value, as printed, and its units.
schemaFrontEndQuantity <- function(description)
  schemaObject(list(
    value      = schemaNullable(schemaNumber(description)),
    value_text = schemaString("As printed; empty when not printed."),
    units      = schemaChoice(PAPER_REVIEW_PARAMETER_UNITS)
  ))

#' The schema of the extraction's front-end block
#' @keywords internal
paperReviewFrontEndSchema <- function()
  schemaObject(list(
    model_type = schemaEnum(c("none", "recirculatory", "mixing", "other"), paste(
      "'none' unless the paper models intravascular mixing (front-end kinetics).",
      "'recirculatory': cardiac output through central (heart-lung) delay elements and",
      "parallel peripheral circuits (Krejcie, Avram, Henthorn; Upton's lung-body model).",
      "'mixing': an unmixed first-pass compartment cleared by total flow into the central",
      "compartment of a mammillary model whose V1, V2, V3, CL, Q2, Q3 are the ordinary",
      "parameters (Henthorn 2018). 'other' for any other structure, described in words.")),
    description = schemaString("The front-end structure in words, as the paper describes it."),
    sampling_site = schemaEnum(PAPER_REVIEW_SAMPLING_SITES,
                               "The blood the model's concentrations are, as the paper says."),
    total_flow = schemaFrontEndQuantity(paste(
      "Cardiac output, or the total pharmacokinetic flow (the sum of all clearances from",
      "arterial blood), as the model uses it.")),
    unmixed_volume = schemaFrontEndQuantity("'mixing' only: the unmixed (first-pass) volume, fixed or estimated."),
    elimination_clearance = schemaFrontEndQuantity(
      "'recirculatory' only: elimination clearance from arterial blood."),
    elimination_in_flow = schemaEnum(c("yes", "no", "unstated"),
      "'recirculatory' only: is the elimination clearance one of the clearances summed to cardiac output?"),
    central = schemaArray(schemaObject(list(
      flow_fraction = schemaNullable(schemaNumber("The share of cardiac output through this path (1 when one).")),
      volume        = schemaFrontEndQuantity("The path's volume (its mean transit time times its flow)."),
      compartments  = schemaNullable(schemaNumber("The number of compartments in the delay element (an Erlang order).")),
      location      = schemaLocation()
    )), "'recirculatory' only: the central (heart-lung) circulation, one entry per parallel delay element."),
    pathways = schemaArray(schemaObject(list(
      name          = schemaString("As the paper names it: nondistributive, fast tissue, slow tissue, ..."),
      kind          = schemaEnum(c("delay", "tissue"),
                                 "'delay' for a nondistributive circuit (a delay element); 'tissue' for a tissue volume."),
      flow          = schemaFrontEndQuantity("Its blood flow or distribution clearance."),
      volume        = schemaFrontEndQuantity("Its volume."),
      compartments  = schemaNullable(schemaNumber("For a delay: the number of compartments.")),
      tissue_clearance = schemaFrontEndQuantity("An elimination clearance from this tissue, when the model has one."),
      location      = schemaLocation()
    )), "'recirculatory' only: the peripheral circuits, in parallel."),
    arm = schemaObject(list(
      present             = schemaEnum(c("yes", "no"), "Is a peripheral (arm) vein sampling site modelled?"),
      rate_constant       = schemaFrontEndQuantity("The arm compartment's link rate constant."),
      exchanging_fraction = schemaNullable(schemaNumber("The fraction of arm blood flow that exchanges with tissue.")),
      partition           = schemaNullable(schemaNumber("The arm tissue:blood partition coefficient, if printed as a number.")),
      partition_rule      = schemaEnum(c("printed", "v2_over_v1", "unstated"),
                                       "How the partition coefficient is set: a printed number, or V2/V1."),
      driver              = schemaEnum(c("central", "arterial", "unstated"),
                                       "Which concentration feeds the arm compartment, as the paper's equations say."),
      location            = schemaLocation()
    )),
    location = schemaLocation()
  ))

# The rule added to the instructions.
PAPER_REVIEW_FRONT_END_RULE <- paste(
  "- front_end: if the paper models intravascular mixing or recirculation (a",
  "  recirculatory model with cardiac output and delay elements, or an unmixed",
  "  first-pass compartment ahead of a mammillary model), fill front_end with",
  "  every number it prints for that structure, each with its units; otherwise",
  "  set model_type to 'none'. The mammillary part of a 'mixing' model goes in",
  "  parameters as usual. Give each concentration figure series its",
  "  sampling_site (arterial or venous) when the paper says.",
  sep = "\n")

#' A front-end quantity in the engine's units (L, L/min, 1/min)
#' @returns a list: value (NA when absent or not convertible), problem
#' @keywords internal
paperReviewFrontEndValue <- function(q, kind, weight, what)
{
  if (is.null(q)) return(list(value = NA_real_, problem = NULL))
  v <- paperReviewNum(q$value)
  if (!is.finite(v)) return(list(value = NA_real_, problem = NULL))
  unit <- paperReviewUnit(q$units %or% "other")
  if (!identical(unit$kind, kind))
    return(list(value = NA_real_, problem = paste0(what, " is printed in '", q$units %or% "",
                                                   "', which is not a ", kind, " unit the review can convert.")))
  list(value = v * unit$factor * if (isTRUE(unit$perKg)) weight else 1, problem = NULL)
}

#' The paper's front-end model as an engine model
#'
#' At the reference patient: the front-end block's numbers are typical
#' values, and its covariates (if any) are not evaluated.
#'
#' @param extraction the extraction (one model's part of it)
#' @param patient the reference patient
#' @returns NULL when the paper has no front-end model, else a list: spec (the
#'   engine model, or NULL), problems, assumptions
#' @keywords internal
paperReviewFrontEndSpec <- function(extraction, patient)
{
  fe <- extraction$front_end
  type <- fe$model_type %or% "none"
  if (!type %in% c("recirculatory", "mixing", "other")) return(NULL)
  problems <- character(0); assumptions <- character(0)
  val <- function(q, kind, what) {
    r <- paperReviewFrontEndValue(q, kind, patient$weight, what)
    if (!is.null(r$problem)) problems <<- c(problems, r$problem)
    r$value
  }
  if (type == "other")
    return(list(spec = NULL, problems = paste0(
      "The paper's front-end structure (", fe$description %or% "not described",
      ") is not one the engine builds: a recirculatory model or a mixing compartment."),
      assumptions = character(0)))
  flow <- val(fe$total_flow, "clearance", "The total flow")
  arm <- NULL
  if (identical(fe$arm$present, "yes")) {
    k <- val(fe$arm$rate_constant, "rate", "The arm rate constant")
    f <- paperReviewNum(fe$arm$exchanging_fraction)
    driver <- fe$arm$driver %or% "unstated"
    if (!driver %in% c("central", "arterial")) {
      driver <- "central"
      assumptions <- c(assumptions, paste(
        "The arm compartment is taken to be fed by the mixed central concentration,",
        "the paper not saying (Henthorn 2018's Figure 6 is reproduced only so)."))
    }
    arm <- list(k = k, fraction = f, driver = driver, rule = fe$arm$partition_rule %or% "unstated",
                partition = paperReviewNum(fe$arm$partition))
  }
  if (type == "mixing") {
    d <- paperReviewDisposition(extraction, patient)
    problems <- c(problems, d$problems)
    Vu <- val(fe$unmixed_volume, "volume", "The unmixed volume")
    if (!is.finite(Vu)) {
      Vu <- 1
      assumptions <- c(assumptions, "The unmixed volume is taken as 1 L, the paper printing none (it is not identifiable from infrequent samples; Henthorn 2018 fixed it at 1 L).")
    }
    n <- paperReviewNum(extraction$structure$compartments)
    spec <- list(type = "mixing", unmixed_volume = Vu, flow = flow, v1 = d$v1, cl1 = d$cl1)
    if (isTRUE(n >= 2)) { spec$v2 <- d$v2; spec$cl2 <- d$cl2 }
    if (isTRUE(n >= 3)) { spec$v3 <- d$v3; spec$cl3 <- d$cl3 }
    if (!is.null(arm)) {
      pc <- if (identical(arm$rule, "v2_over_v1") && isTRUE(n >= 2)) d$v2 / d$v1 else arm$partition
      if (!is.finite(pc)) {
        pc <- 1
        assumptions <- c(assumptions, "The arm partition coefficient is taken as 1, the paper printing none.")
      }
      spec$arm <- list(k = arm$k, fraction = arm$fraction, partition = pc, driver = arm$driver)
    }
  } else {
    central <- lapply(fe$central %or% list(), function(ch) list(
      fraction = paperReviewNum(ch$flow_fraction) %or% NA_real_,
      volume = val(ch$volume, "volume", "A central delay element's volume"),
      n = paperReviewNum(ch$compartments)))
    if (length(central) == 1 && !is.finite(central[[1]]$fraction)) central[[1]]$fraction <- 1
    for (i in seq_along(central)) if (!is.finite(central[[i]]$n)) {
      central[[i]]$n <- 1
      assumptions <- c(assumptions, paste0("Central delay element ", i, " is taken as one well-stirred ",
                                           "compartment, the paper printing no number of compartments."))
    }
    pathways <- lapply(fe$pathways %or% list(), function(p) {
      out <- list(name = p$name %or% "pathway", type = p$kind %or% "tissue",
                  flow = val(p$flow, "clearance", paste0("The flow of '", p$name %or% "", "'")),
                  volume = val(p$volume, "volume", paste0("The volume of '", p$name %or% "", "'")),
                  n = paperReviewNum(p$compartments))
      cl <- val(p$tissue_clearance, "clearance", paste0("The tissue clearance of '", p$name %or% "", "'"))
      if (is.finite(cl)) out$clearance <- cl
      if (identical(out$type, "delay") && !is.finite(out$n)) {
        out$n <- 1
        assumptions <<- c(assumptions, paste0("The circuit '", out$name, "' is taken as one compartment, ",
                                              "the paper printing no number of compartments."))
      }
      out
    })
    names <- make.unique(vapply(pathways, function(p) p$name, ""))
    for (i in seq_along(pathways)) pathways[[i]]$name <- names[i]
    spec <- list(type = "recirculatory", cardiac_output = flow, central = central, pathways = pathways,
                 elimination_clearance = val(fe$elimination_clearance, "clearance", "The elimination clearance"),
                 elimination_in_flow = !identical(fe$elimination_in_flow, "no"))
    if (!is.finite(spec$elimination_clearance)) {
      cl <- paperReviewParameterValue(extraction, "CL", patient$weight)$value
      if (is.finite(cl)) spec$elimination_clearance <- cl
    }
  }
  built <- tryCatch(frontEndSystem(spec), error = function(e) e)
  if (inherits(built, "error")) {
    problems <- c(problems, paste0("The front-end model cannot be built: ", conditionMessage(built)))
    spec <- NULL
  }
  list(spec = spec, problems = unique(problems), assumptions = unique(assumptions))
}

#' A regimen's intravenous doses as front-end engine doses (mg, mg/min)
#' @returns a list: doses (frontEndDoses()), problems
#' @keywords internal
paperReviewFrontEndDoses <- function(regimen, patient)
{
  bolus <- data.frame(time = numeric(0), amount = numeric(0))
  inf <- data.frame(start = numeric(0), end = numeric(0), rate = numeric(0))
  problems <- character(0)
  mg <- c(g = 1000, mg = 1, mcg = 1e-3, ng = 1e-6)
  for (d in regimen$doses %or% list()) {
    route <- d$route %or% "other"
    if (!route %in% c("iv_bolus", "iv_infusion")) {
      problems <- c(problems, "A dose that is not intravenous is not given to the front-end model.")
      next
    }
    minutes <- paperReviewMinutes(d$time_units %or% "min")
    mass <- mg[d$mass_unit %or% "other"]
    t0 <- paperReviewNum(d$time) * minutes
    amount <- paperReviewNum(d$amount)
    if (is.na(mass) || !is.finite(t0) || !is.finite(amount)) {
      problems <- c(problems, "A dose's amount, time or units could not be read.")
      next
    }
    analyte <- paperReviewNum(d$analyte_fraction)
    if (!is.finite(analyte)) analyte <- 1
    scale <- mass * (if (identical(d$per_kg, "yes")) patient$weight else 1) * analyte
    duration <- paperReviewNum(d$duration) * minutes
    if (paperReviewIsRateDose(d)) {
      perMin <- switch(d$per_time %or% "none", per_min = 1, per_h = 1 / 60, per_day = 1 / 1440, NA_real_)
      rate <- if (is.finite(perMin)) amount * perMin else if (is.finite(duration) && duration > 0) amount / duration else NA_real_
      if (!is.finite(rate) || !is.finite(duration) || duration <= 0) {
        problems <- c(problems, "An infusion has no readable rate or duration.")
        next
      }
      inf <- rbind(inf, data.frame(start = t0, end = t0 + duration, rate = rate * scale))
    } else {
      bolus <- rbind(bolus, data.frame(time = t0, amount = amount * scale))
    }
  }
  list(doses = if (nrow(bolus) + nrow(inf)) frontEndDoses(bolus, inf) else NULL, problems = unique(problems))
}

#' The front-end findings and figures of one model
#'
#' @param extraction the extraction (one model's part of it)
#' @param reference the reference patient
#' @returns list(findings, figures, spec); empty when the paper has no
#'   front-end model
#' @keywords internal
paperReviewFrontEndChecks <- function(extraction, reference)
{
  built <- paperReviewFrontEndSpec(extraction, reference)
  if (is.null(built)) return(list(findings = list(), figures = list(), spec = NULL))
  section <- PAPER_REVIEW_FRONT_END_SECTION
  findings <- list()
  figures <- list()
  loc <- extraction$front_end$location %or% NA_character_
  add <- function(...) findings[[length(findings) + 1]] <<- paperReviewFinding(section, ...)
  for (p in built$problems) add("concern", "The front-end model is incomplete", p, loc)
  for (a in built$assumptions) add("info", "An assumption the review made", a, loc, computed = FALSE)
  spec <- built$spec
  if (is.null(spec)) return(list(findings = findings, figures = figures, spec = NULL))

  # What was built, in the terms a reader can check against the paper.
  if (spec$type == "recirculatory") {
    vc <- sum(vapply(spec$central, function(ch) ch$volume, 0))
    nd <- Filter(function(p) identical(p$type, "delay"), spec$pathways)
    vnd <- sum(vapply(nd, function(p) p$volume, 0))
    add("ok", "The recirculatory model is built and its flows balance",
        paste0("Cardiac output ", prFmt(spec$cardiac_output), " L/min through ", length(spec$central),
               " central delay element(s) of ", prFmt(vc), " L in all (mean transit time ",
               prFmt(60 * vc / spec$cardiac_output), " s), then ", length(spec$pathways),
               " peripheral circuit(s); elimination clearance ", prFmt(spec$elimination_clearance),
               " L/min from arterial blood. The central circulation and the nondistributive ",
               "circuits together, ", prFmt(vc + vnd), " L, are the central volume a ",
               "mammillary model fitted to the same drug should have (Avram and Krejcie 2003); a ",
               "larger V1 fitted to the paper's data after a bolus means the early samples missed ",
               "the mixing."), loc)
    v1 <- paperReviewParameterValue(extraction, "V1", reference$weight)
    if (is.finite(v1$value))
      add(if (v1$value > 1.5 * (vc + vnd)) "info" else "ok",
          "The paper's mammillary V1 against the recirculatory central volume",
          paste0("V1 ", prFmt(v1$value), " L against VC + VND ", prFmt(vc + vnd), " L. A target-controlled ",
                 "infusion designed with the larger volume overshoots its target in the first minutes ",
                 "(Avram and Krejcie 2003, Table 1)."), c(loc, v1$location))
  } else {
    add("ok", "The front-end mixing model is built",
        paste0("An unmixed volume of ", prFmt(spec$unmixed_volume), " L cleared by ", prFmt(spec$flow),
               " L/min into the central compartment (V1 ", prFmt(spec$v1), " L). During an infusion at ",
               "rate R arterial blood carries R / ", prFmt(spec$flow), " of unmixed drug on top of the ",
               "mixed concentration (Henthorn 2018 eq. 2); it is reached within about ",
               prFmt(3 * spec$unmixed_volume / spec$flow * 60), " s and vanishes as fast when the ",
               "infusion stops, so the arterial-venous difference during an infusion is a mixing ",
               "artifact, not distribution.",
               if (!is.null(spec$arm)) paste0(" Arm vein blood is ", prFmt(1 - spec$arm$fraction),
                 " mixed central blood and ", prFmt(spec$arm$fraction), " blood that has exchanged with arm tissue ",
                 "(partition ", prFmt(spec$arm$partition), ", link ", prFmt(spec$arm$k), " /min), which ",
                 "is why venous concentrations exceed arterial ones after the infusion.") else ""), loc)
  }

  # The paper's concentration figures, by sampling site.
  sys <- frontEndSystem(spec)
  for (f in extraction$figures %or% list()) {
    if (!identical(f$y_quantity, "concentration") || identical(f$one_regimen, "no")) next
    if ((f$y_normalization %or% "none") != "none") next
    factor <- paperReviewConcentrationFactor(f$y_units %or% "other", "mcg", extraction$drug$molecular_weight)
    minutes <- paperReviewMinutes(f$time_units %or% "min")
    if (!is.finite(factor) || is.na(minutes)) next
    id <- f$figure_id %or% "Figure"
    rows <- list(); curves <- list(); setAside <- character(0)
    for (s in f$series %or% list()) {
      if (!(s$analyte %or% "parent") %in% "parent" || !length(s$points)) next
      site <- s$sampling_site %or% "unstated"
      if (!site %in% names(PAPER_REVIEW_FRONT_END_OBSERVATION))
        site <- extraction$front_end$sampling_site %or% "arterial"
      if (!site %in% names(PAPER_REVIEW_FRONT_END_OBSERVATION)) site <- "arterial"
      obs <- PAPER_REVIEW_FRONT_END_OBSERVATION[[site]]
      if (!obs %in% rownames(sys$O)) {
        setAside <- c(setAside, paste0(s$label %or% "a series", ": the model has no ", site, " site"))
        next
      }
      rid <- s$regimen_id %or% f$regimen_id
      regimen <- paperReviewRegimen(extraction, rid)
      if (is.null(regimen)) { setAside <- c(setAside, paste0(s$label %or% "a series", ": names no regimen")); next }
      doses <- paperReviewFrontEndDoses(regimen, reference)
      if (is.null(doses$doses)) { setAside <- c(setAside, paste0(s$label %or% "a series", ": no intravenous dose")); next }
      t <- vapply(s$points, function(p) paperReviewNum(p$time), 0) * minutes
      y <- vapply(s$points, function(p) paperReviewNum(p$value), 0) * factor
      keep <- is.finite(t) & t >= 0 & is.finite(y) & y > 0
      if (!any(keep)) next
      maximum <- max(t[keep]) * 1.05 + 1
      grid <- sort(unique(c(seq(0, maximum, length.out = 400), t[keep])))
      sim <- tryCatch(frontEndSimulate(sys, doses$doses, grid), error = function(e) e)
      if (inherits(sim, "error")) { setAside <- c(setAside, paste0(s$label %or% "a series", ": ", conditionMessage(sim))); next }
      label <- paste0(s$label %or% "series", " (", site, ")")
      rows[[length(rows) + 1]] <- data.frame(series = label, regimen = rid %or% NA_character_, time = t[keep],
                                             observed = y[keep], lower = NA_real_, upper = NA_real_,
                                             predicted = sim[[obs]][match(t[keep], sim$time)],
                                             stringsAsFactors = FALSE)
      curves[[length(curves) + 1]] <- data.frame(Time = sim$time, Cp = sim[[obs]], regimen = label,
                                                 stringsAsFactors = FALSE)
    }
    if (!length(rows)) next
    points <- do.call(rbind, rows)
    use <- points$predicted > 0
    pe <- (points$observed[use] - points$predicted[use]) / points$predicted[use]
    mdpe <- stats::median(pe); mdape <- stats::median(abs(pe))
    severity <- if (!is.finite(mdape)) "info" else if (mdape <= 0.2) "ok" else if (mdape <= 0.5) "concern" else "error"
    if (paperReviewTenfold(1 + mdpe)) severity <- "concern"
    add(severity, paste0(id, ": the front-end model ", if (severity == "ok") "reproduces" else
                           "does not closely reproduce", " the digitised points"),
        paste0(sum(use), " points compared at their sampling sites, each series against the engine's ",
               "concentration there. Median prediction error ", sprintf("%+.0f%%", 100 * mdpe),
               ", median absolute ", sprintf("%.0f%%", 100 * mdape), ".",
               if (length(setAside)) paste0(" Not compared: ", paste(setAside, collapse = "; "), ".") else ""),
        f$location)
    # Concentrations in mcg/mL, the engine's own; the report's plot labels
    # the axis from the drug, so the figure is converted to its units.
    drugUnits <- (PAPER_REVIEW_CONCENTRATION_MAP[[extraction$drug$concentration_units %or% "mcg/mL"]] %or%
                    list(units = "mcg"))$units
    toDrug <- c(pg = 1e6, ng = 1e3, mcg = 1)[[drugUnits]]
    points$observed <- points$observed * toDrug; points$predicted <- points$predicted * toDrug
    curve <- do.call(rbind, curves); curve$Cp <- curve$Cp * toDrug
    figures[[length(figures) + 1]] <- list(id = paste0(id, " (front-end model)"),
      caption = f$caption %or% "", logY = identical(f$log_y, "yes"),
      curve = curve, points = points, mdpe = mdpe, mdape = mdape)
  }
  list(findings = findings, figures = figures, spec = spec)
}
