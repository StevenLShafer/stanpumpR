# -----------------------------------------------------------------------------
# Generated blocks inside hand-written help pages
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Claude Code, 2026-10-09, at the request of Steven L.
# Shafer, after an audit found the hand-written route table on the absorption
# page listing fewer drugs than the dose table offers by mouth, by injection
# into muscle or by nasal spray.  Verified by tests/testthat/test-help-content.R.
#
# A hand-written page (inst/help/<id>.md) may hold a line that is nothing but
#
#     <!-- generated: NAME -->
#
# and helpPageHTML() replaces it, before rendering, with the Markdown that
# HELP_GENERATED_BLOCKS[[NAME]] builds from the code.  A fact the code owns --
# which drugs offer which routes -- is then stated from the code and cannot
# drift from it, while the prose around it stays hand-written.  Viewed as raw
# Markdown (on GitHub, say) the marker is an invisible comment.
#
# Adding a block: write a function of `drugDefaults` that returns Markdown, add
# it to HELP_GENERATED_BLOCKS, and put the marker in the page.  An unknown name
# is an error, so the "every page renders" test catches a misspelt marker.
# -----------------------------------------------------------------------------

HELP_GENERATED_MARKER <-
  "^[[:space:]]*<!--[[:space:]]*generated:[[:space:]]*([A-Za-z0-9_-]+)[[:space:]]*-->[[:space:]]*$"

#' Replace each generated-block marker in a page's Markdown with its block
#'
#' @param text the page's Markdown, one string
#' @param drugDefaults the drug library, passed to each block's builder
#' @returns `text` with every marker line replaced
#' @noRd
helpExpandGenerated <- function(text, drugDefaults = getDrugDefaultsGlobal()) {
  if (is.null(text) || !nzchar(text)) return(text)
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  hit <- grepl(HELP_GENERATED_MARKER, lines)
  if (!any(hit)) return(text)
  for (i in which(hit)) {
    name <- sub(HELP_GENERATED_MARKER, "\\1", lines[i])
    build <- HELP_GENERATED_BLOCKS[[name]]
    if (is.null(build)) stop("Unknown generated help block: ", name)
    lines[i] <- build(drugDefaults)
  }
  paste(lines, collapse = "\n")
}

#' The extravascular routes each drug offers, from its units
#'
#' One entry per drug with a PO, SL, IM, IN or RA unit in `drugDefaults_global.csv`,
#' the same Units the dose table's selector offers.  Each route lists that
#' route's units without their repeating forms (`mg PO bid` is `mg PO` given
#' on a schedule), with a note when a unit is a rate (`mg/day PO`, a constant
#' daily input with no depot) or when the drug's model declares saturable oral
#' absorption (an `oralSaturation` or `sublingualSaturation` block).
#'
#' @param drugDefaults the drug library
#' @returns a list with one element per such drug, in library order: a list of
#'   `drug`, `iv` (TRUE when the drug is also given intravenously) and, named
#'   PO, SL, IM, IN, SC and RA, a list of `units` (empty when the route is not offered) and
#'   `notes`
#' @noRd
helpRouteInventory <- function(drugDefaults = getDrugDefaultsGlobal()) {
  adult <- helpReferencePatients()[1, ]
  extravascular <- c(ROUTE_PO, ROUTE_SL, ROUTE_IM, ROUTE_IN, ROUTE_RA, ROUTE_SC)
  rows <- lapply(seq_len(nrow(drugDefaults)), function(i) {
    row <- drugDefaults[i, ]
    if (isGasDrug(row$Drug)) return(NULL)
    units <- unique(scheduleBaseUnit(helpDrugUnits(row)))
    route <- doseRoute(units)
    if (!any(route %in% extravascular)) return(NULL)
    model <- helpDrugModelOutput(row$Drug, adult)
    satNote <- function(sat) if (!is.null(sat)) switch(oralSaturationForm(sat),
      saturable = "saturable absorption",
      rising    = "bioavailability rises with dose",
      power     = "exposure more than dose-proportional",
      reduction = "bioavailability falls with dose",
      exponential = "bioavailability falls with dose")
    notes <- list(PO = satNote(model$oralSaturation), SL = satNote(model$sublingualSaturation))
    byRoute <- lapply(stats::setNames(extravascular, extravascular), function(r) {
      u <- units[route == r]
      list(units = u,
           notes = c(if (any(isRateUnit(u)))
                       if (r == ROUTE_RA) "continuous infusion into the depot"
                       else if (r == ROUTE_SC) "insulin pump: continuous infusion into the depot"
                       else "constant daily rate, no depot",
                     if (length(u) > 0) notes[[r]]))
    })
    c(list(drug = row$Drug, iv = any(route == ROUTE_IV)), byRoute)
  })
  rows[!vapply(rows, is.null, logical(1))]
}

#' The route table on the absorption page, as Markdown
#' @noRd
helpRouteTableMarkdown <- function(drugDefaults = getDrugDefaultsGlobal()) {
  cell <- function(x) {
    if (length(x$units) == 0) return("—")
    paste0(paste0("`", x$units, "`", collapse = ", "),
           if (length(x$notes) > 0) paste0(" (", paste(x$notes, collapse = "; "), ")") else "")
  }
  body <- vapply(helpRouteInventory(drugDefaults), function(d) {
    sprintf("| [%s](help:drugs/%s) | %s | %s | %s | %s | %s | %s | %s |", helpDrugTitle(d$drug), d$drug,
            cell(d[[ROUTE_PO]]), cell(d[[ROUTE_SL]]), cell(d[[ROUTE_IM]]), cell(d[[ROUTE_IN]]),
            cell(d[[ROUTE_RA]]), cell(d[[ROUTE_SC]]), if (d$iv) "yes" else "no")
  }, character(1))
  paste(c(paste("| Drug | Oral (PO) | Sublingual (SL) | Intramuscular (IM) | Intranasal (IN) |",
                "Regional anesthesia (RA) | Subcutaneous (SC) | Also intravenous |"),
          "|---|---|---|---|---|---|---|---|",
          body), collapse = "\n")
}

#' The Schnider and Minto models at the reference patients, as Markdown
#'
#' One table per alternative model (R/alternativeModels.R): the volumes,
#' clearances, ke0 and time to peak effect getDrugPK() computes from the model
#' at each of the help's reference patients, beside the library's own values
#' for the same patient, so the page states what the code computes.  A
#' patient for whom the model is not defined (a negative clearance) is
#' listed with the reason instead of numbers.
#' @noRd
helpAlternativeModelTableMarkdown <- function(drugDefaults = getDrugDefaultsGlobal()) {
  patients <- helpReferencePatients()
  num <- function(x, digits = 3) if (is.null(x) || is.na(x)) "—" else helpFormatNumber(x, digits)
  # This block is the Minto / Schnider help page's table, so it covers that
  # group's drugs only; the Hamberg companion has its own hand-written page.
  tables <- vapply(companionGroupDrugs("mintoSchnider"), function(drug) {
    spec <- ALTERNATIVE_MODELS[[drug]]
    row <- drugDefaults[drugDefaults$Drug == drug, ][1, ]
    lines <- vapply(seq_len(nrow(patients)), function(i) {
      p <- patients[i, ]
      patient <- sprintf("%s (%s y, %s kg, %s cm, %s)", p$label,
                         helpFormatNumber(p$age), p$weight, p$height, p$sex)
      problem <- alternativeModelProblem(drug, p$weight, p$height, p$age, p$sex)
      own <- helpDrugPK(drug, p, drugDefaults)
      ownSet <- if (is.null(own)) NULL else own$PK[[PK_EVENT_DEFAULT]]
      ownText <- if (is.null(ownSet)) "—" else
        sprintf("CL1 %s L/min, V1 %s L, ke0 %s/min", num(ownSet$cl1), num(ownSet$v1), num(ownSet$ke0))
      if (!is.null(problem)) {
        return(sprintf("| %s | not defined: %s | | | | | | | | %s |", patient,
                       sub(" Only the library's model is shown\\.$", "", sub("^.*patient: ", "", problem)),
                       ownText))
      }
      entry <- alternativeModelEntry(drug, p$weight, p$height, p$age, p$sex, row)
      k <- entry$PK[[PK_EVENT_DEFAULT]]
      sprintf("| %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |", patient,
              num(k$v1), num(k$v2), num(k$v3), num(k$cl1), num(k$cl2), num(k$cl3),
              num(k$ke0), num(entry$tPeak), ownText)
    }, character(1))
    paste(c(
      sprintf("**%s: the %s model**", helpDrugTitle(drug), spec$label),
      "",
      "| Patient | V1 (L) | V2 (L) | V3 (L) | CL1 (L/min) | CL2 (L/min) | CL3 (L/min) | ke0 (1/min) | tPeak (min) | Library model, same patient |",
      "|---|---|---|---|---|---|---|---|---|---|",
      lines,
      ""
    ), collapse = "\n")
  }, character(1))
  paste(tables, collapse = "\n")
}

#' The reference parturient at which the pregnancy factors are measured
#' @noRd
helpPregnancyPatient <- function() {
  list(age = 30, weight = 70, height = 165, sex = SEX_FEMALE)
}

#' The pregnancy review's drugs, as Markdown
#'
#' One table per group of pregnancyFindings() (R/pregnancy.R): the drugs the
#' review quantifies, with what each model applies and the ratio of its
#' elimination clearance and central volume with the Pregnant field on to
#' off, measured from the model itself at helpPregnancyPatient(); then the
#' drugs the review describes without a magnitude, reports as unchanged, or
#' reports as unknown, with its finding for each.  The page states what the
#' code computes, and test-pregnancy.R checks the groups against the models.
#' @noRd
helpPregnancyTableMarkdown <- function(drugDefaults = getDrugDefaultsGlobal()) {
  findings <- pregnancyFindings()
  findings <- findings[findings$drug %in% drugDefaults$Drug, ]
  p <- helpPregnancyPatient()
  ratios <- function(drug) {
    row <- drugDefaults[drugDefaults$Drug == drug, ][1, ]
    set <- function(pregnant) getDrugPK(drug, p$weight, p$height, p$age, p$sex, row,
                                        pregnant = pregnant)$PK[[PK_EVENT_DEFAULT]]
    on <- set(TRUE)
    off <- set(FALSE)
    sprintf("%s | %s", helpFormatNumber(on$cl1 / off$cl1), helpFormatNumber(on$v1 / off$v1))
  }
  link <- function(drug) sprintf("[%s](help:drugs/%s)", helpDrugTitle(drug), drug)
  section <- function(group, heading, intro, columns, cells) {
    rows <- findings[findings$group == group, ]
    if (nrow(rows) == 0) return(character(0))
    c(sprintf("**%s**", heading), "", intro, "",
      sprintf("| %s |", paste(columns, collapse = " | ")),
      sprintf("|%s", strrep("---|", length(columns))),
      vapply(seq_len(nrow(rows)), function(i) cells(rows[i, ]), character(1)),
      "")
  }
  patient <- sprintf("%s years, %s kg, %s cm", p$age, p$weight, p$height)
  c(
    section("applied", "Adjusted when Pregnant is ticked",
            sprintf(paste("The last two columns are measured from the model itself: its elimination clearance",
                          "and its central volume with the field on, divided by the same with it off, for a",
                          "woman of %s with a normal creatinine."), patient),
            c("Drug", "What the review found", "What the model applies", "CL1 ratio", "V1 ratio"),
            function(r) sprintf("| %s | %s | %s | %s |", link(r$drug), r$finding, r$applied, ratios(r$drug))),
    section("unquantified", "Described by the review, but without a magnitude: not adjusted",
            "The review reports a direction for these and no figure, so the models are left as published and the field has no effect on them.",
            c("Drug", "What the review found"),
            function(r) sprintf("| %s | %s |", link(r$drug), r$finding)),
    section("unchanged", "Reported as unchanged: not adjusted",
            "The review found no pharmacokinetic change to apply.",
            c("Drug", "What the review found"),
            function(r) sprintf("| %s | %s |", link(r$drug), r$finding)),
    section("unknown", "Unknown: not adjusted",
            "The review's tables give the pharmacokinetic change in pregnancy as unknown.",
            c("Drug", "What the review found"),
            function(r) sprintf("| %s | %s |", link(r$drug), r$finding))
  ) |> paste(collapse = "\n")
}

#' The pharmacokinetics of every illicit-drug research model, at one patient
#'
#' These models differ from one another more than the rest of the library does
#' — one compartment or two, linear elimination or saturable, a parent or a
#' metabolite plotted, apparent parameters or absolute — and the thing a reader
#' most often wants is to compare them. Each drug's own page carries its
#' parameters at six reference patients; this is the one-screen summary, built
#' from the models so that it cannot drift from them.
#'
#' Everything is evaluated at the reference adult with the fat-free-mass switch
#' as the app ships it, which is why a value here may differ slightly from the
#' published number its page quotes.
#'
#' @param drugDefaults the drug library
#' @returns one Markdown table, as a single string
#' @noRd
helpIllicitPKTableMarkdown <- function(drugDefaults = getDrugDefaultsGlobal()) {
  drugs <- illicitDrugNames(drugDefaults)
  p <- helpReferencePatients()[1, ]
  columns <- c("Drug", "Plotted", "Structure", "V1 (L)", "Elimination",
               "Absorption", "Half-life")
  cells <- function(drug) {
    row <- drugDefaults[drugDefaults$Drug == drug, ][1, ]
    X <- getDrugPK(drug, p$weight, p$height, p$age, p$sex, row)
    set <- X$PK[[PK_EVENT_DEFAULT]]
    mm <- X$michaelisMenten
    units <- helpDrugUnits(row)
    # Two compartments can come from v2 or, on the saturable engine, from the
    # pair of distribution rate constants.
    peripheral <- isTRUE(set$v2 > 1) ||
      (!is.null(mm) && !is.null(mm$k23) && !is.null(mm$k32))
    structure <- paste0(if (peripheral) "Two compartments" else "One compartment",
                        if (!is.null(mm)) ", saturable" else "")
    elimination <- if (!is.null(mm)) {
      sprintf("Vmax %s mg/min, Km %s %s", helpFormatNumber(mm$vmax),
              helpFormatNumber(mm$km), helpConcentrationUnits(row$Concentration.Units))
    } else {
      sprintf("CL %s L/min", helpFormatNumber(set$cl1))
    }
    # The route actually offered, and the rate constant that drives it.
    route <- unique(doseRoute(units))
    # One absorption constant per extravascular route the drug offers.  Keep
    # this in step with DOSE_ROUTES: a route missing here reads as no
    # absorption at all rather than as an error.
    kaOf <- function(r) set[[routeKaField(r)]]
    kas <- unlist(lapply(route, kaOf))
    kas <- kas[!is.na(kas) & kas > 0]
    routes <- paste(route, collapse = ", ")
    absorption <- if (length(kas) == 0) {
      "—"
    } else if (length(unique(round(kas, 9))) > 1) {
      # Each route absorbs at its own rate; one number here would be a lie.
      sprintf("%s, each at its own rate", routes)
    } else {
      sprintf("%s, ka %s /h (t&frac12; %s min)", routes,
              helpFormatNumber(kas[1] * 60), helpFormatNumber(log(2) / kas[1]))
    }
    # For a saturable drug the terminal half-life is not a constant; say so
    # rather than printing the limit as though it were one.
    halfLife <- if (!is.null(mm)) "Not fixed: rises with the dose" else {
      k <- set$cl1 / set$v1
      sprintf("%s min", helpFormatNumber(log(2) / k))
    }
    sprintf("| [%s](help:drugs/%s) | %s | %s | %s | %s | %s | %s |",
            drug, drug, helpIllicitAnalyte(drug),
            structure, helpFormatNumber(set$v1), elimination, absorption, halfLife)
  }
  paste(c(
    sprintf("| %s |", paste(columns, collapse = " | ")),
    sprintf("|%s", strrep("---|", length(columns))),
    vapply(drugs, cells, character(1))
  ), collapse = "\n")
}

#' What each illicit-drug model plots, where the drug and the analyte differ
#' @noRd
helpIllicitAnalyte <- function(drug) {
  registry <- illicitRouteRegistry()
  rows <- registry[registry$drug == drug & registry$offered == "yes", ]
  if (nrow(rows) == 0) return("—")
  paste(unique(rows$analyte), collapse = ", ")
}

#' The James lean body mass against the Al-Sallami fat-free mass, as a figure
#'
#' An inline SVG line chart for the Open TCI page (inst/help/opentci.md): both
#' measures of body size for one woman across weight, drawn from lbmJames()
#' and ffmAlSallami() themselves, with the James peak and zero found from the
#' function rather than quoted, and the pumps' BMI limit for women shaded.  The
#' block is one HTML block with no blank line, so commonmark passes it through
#' whole.
#'
#' @param drugDefaults unused; every block takes it
#' @returns the figure, as one HTML string
#' @noRd
helpJamesLBMFigure <- function(drugDefaults = NULL) {
  height <- 160; age <- 40; sex <- SEX_FEMALE
  bmiLimit <- 35    # the pumps' limit for the Schnider and Minto models in women
  weights <- seq(40, 200, by = 1)
  james <- vapply(weights, lbmJames, numeric(1), height = height, sex = sex)
  ffm <- vapply(weights, ffmAlSallami, numeric(1), height = height, age = age, sex = sex)
  lbm <- function(w) lbmJames(w, height, sex)
  peak <- stats::optimize(lbm, c(40, 200), maximum = TRUE, tol = 1e-8)$maximum
  zero <- stats::uniroot(lbm, c(peak, 400), tol = 1e-8)$root
  bmi <- function(w) w / (height / 100)^2
  limitWeight <- bmiLimit * (height / 100)^2

  # Plot area, in SVG units
  W <- 640; H <- 360; left <- 56; right <- 30; top <- 16; bottom <- 64
  xr <- c(40, 200); yr <- c(-20, 70)
  sx <- function(x) left + (x - xr[1]) / diff(xr) * (W - left - right)
  sy <- function(y) H - bottom - (y - yr[1]) / diff(yr) * (H - top - bottom)
  f <- function(v) sprintf("%.1f", v)
  path <- function(x, y) paste0("M", paste(f(sx(x)), f(sy(y)), sep = ",", collapse = " L"))

  xTicks <- seq(40, 200, by = 20)
  yTicks <- seq(-20, 70, by = 10)
  grid <- c(
    sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="#e3e8ec"/>',
            f(sx(xr[1])), f(sy(yTicks)), f(sx(xr[2])), f(sy(yTicks))),
    sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="#4a5764"/>',
            f(sx(xr[1])), f(sy(0)), f(sx(xr[2])), f(sy(0))),
    sprintf('<text x="%s" y="%s" text-anchor="end" dominant-baseline="middle">%d</text>',
            f(sx(xr[1]) - 6), f(sy(yTicks)), as.integer(yTicks)),
    sprintf('<text x="%s" y="%s" text-anchor="middle">%d</text>',
            f(sx(xTicks)), f(sy(yr[1]) + 16), as.integer(xTicks)),
    sprintf('<text x="%s" y="%s" text-anchor="middle" fill="#7b8a8b">BMI %d</text>',
            f(sx(xTicks)), f(sy(yr[1]) + 30), as.integer(round(bmi(xTicks))))
  )
  band <- sprintf(paste0(
    '<rect x="%s" y="%s" width="%s" height="%s" fill="#f4f6f8"/>',
    '<text x="%s" y="%s" fill="#7b8a8b">Above BMI %d: past the pumps\' limit for women</text>'),
    f(sx(limitWeight)), f(top), f(sx(xr[2]) - sx(limitWeight)), f(sy(yr[1]) - top),
    f(sx(limitWeight) + 6), f(sy(yr[1]) - 8), bmiLimit)
  marks <- c(
    sprintf('<circle cx="%s" cy="%s" r="4" fill="#c0392b"/>', f(sx(peak)), f(sy(lbm(peak)))),
    sprintf('<text x="%s" y="%s" text-anchor="middle" fill="#c0392b">Peak, %s kg at BMI %s</text>',
            f(sx(peak) + 8), f(sy(lbm(peak)) + 36), helpFormatNumber(lbm(peak)),
            helpFormatNumber(bmi(peak))),
    sprintf('<circle cx="%s" cy="%s" r="4" fill="#c0392b"/>', f(sx(zero)), f(sy(0))),
    sprintf('<text x="%s" y="%s" text-anchor="end" fill="#c0392b">Zero at BMI %s</text>',
            f(sx(zero) - 8), f(sy(0) + 16), helpFormatNumber(bmi(zero)))
  )
  lines <- c(
    sprintf('<path d="%s" fill="none" stroke="#2c3e50" stroke-width="2.5"/>', path(weights, ffm)),
    sprintf('<path d="%s" fill="none" stroke="#c0392b" stroke-width="2.5"/>', path(weights, james)),
    # The legend sits above both lines, left of the shaded band
    sprintf(paste0('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="%s" stroke-width="2.5"/>',
                   '<text x="%s" y="%s" dominant-baseline="middle" fill="%s">%s</text>'),
            f(sx(xr[1]) + 10), c(30, 48), f(sx(xr[1]) + 34), c(30, 48),
            c("#c0392b", "#2c3e50"), f(sx(xr[1]) + 40), c(30, 48), c("#c0392b", "#2c3e50"),
            c("James lean body mass", "Al-Sallami fat-free mass"))
  )
  axes <- c(
    sprintf('<text x="%s" y="%s" text-anchor="middle">Weight (kg)</text>',
            f((sx(xr[1]) + sx(xr[2])) / 2), f(H - 8)),
    sprintf('<text transform="translate(14,%s) rotate(-90)" text-anchor="middle">Body size measure (kg)</text>',
            f((sy(yr[1]) + sy(yr[2])) / 2))
  )
  label <- sprintf(paste(
    "James lean body mass and Al-Sallami fat-free mass of a %d-year-old woman, %d cm tall,",
    "from 40 to 200 kg. The James value peaks at %s kg at a BMI of %s and is zero at a BMI of %s;",
    "the fat-free mass rises at every weight."), age, height, helpFormatNumber(lbm(peak)),
    helpFormatNumber(bmi(peak)), helpFormatNumber(bmi(zero)))
  svg <- c(
    sprintf('<svg viewBox="0 0 %d %d" role="img" aria-label="%s" font-size="12" font-family="inherit" style="width:100%%;max-width:640px;height:auto">',
            W, H, label),
    band, grid, lines, marks, axes, "</svg>")
  paste(c(
    '<figure class="help-figure">',
    svg,
    sprintf(paste(
      "<figcaption>Lean body mass of a %d-year-old woman, %d cm tall, by the James equation",
      "(used by the Schnider and Minto models) and the Al-Sallami fat-free mass (used by",
      "Eleveld's). Drawn by stanpumpR from the same functions its models call.</figcaption>"),
      age, height),
    "</figure>"
  ), collapse = "\n")
}

#' The vasoactive registry as a Markdown table
#'
#' One row per drug and candidate source in
#' \code{inst/extdata/vasoactiveRegistry.csv}: what it is, whether it is in
#' the library, and what stands in the way if it is not.  Built from the
#' registry so the help cannot drift from it.
#' @noRd
helpVasoactiveRegistryMarkdown <- function(registry = vasoactiveRegistry()) {
  status <- c(implemented = "In the library",
              needs_full_text = "Needs the paper",
              no_suitable_model = "No usable model")
  read <- c(full_text = "full text", abstract = "abstract", none = "citation only")
  registry <- registry[order(tolower(registry$drug),
                             match(registry$readiness, names(status))), ]
  drugCell <- ifelse(
    is.na(registry$stanpumpr_drug), registry$drug,
    sprintf("[%s](help:drugs/%s)", registry$drug, registry$stanpumpr_drug))
  # "Abboud 2009": the first author's surname and the year of publication
  short <- paste(sub("^(.+?) [A-Z][A-Za-z]{0,2}[,. ].*$", "\\1", registry$source),
                 sub(";$", "", regmatches(registry$source,
                                          regexpr("(19|20)[0-9]{2};", registry$source))))
  rows <- sprintf("| %s | %s | %s (%s) | [%s](%s) | %s |",
                  drugCell, status[registry$readiness], registry$population,
                  read[registry$evidence_read],
                  short, registry$source_url,
                  registry$blocking_issue)
  paste(c("| Drug | Status | Population (what was read) | Source | Notes |",
          "|---|---|---|---|---|", rows), collapse = "\n")
}

#' The papers that would unblock a vasoactive model, as a Markdown list
#' @noRd
helpVasoactiveToObtainMarkdown <- function(registry = vasoactiveRegistry()) {
  want <- registry[!is.na(registry$to_obtain) & nzchar(registry$to_obtain), ]
  want <- want[order(want$readiness != "needs_full_text", tolower(want$drug)), ]
  paste(sprintf("- **%s**%s: %s", want$drug,
                ifelse(want$readiness == "implemented",
                       " (in the library, to confirm)", ""),
                want$to_obtain), collapse = "\n")
}

HELP_GENERATED_BLOCKS <- list(
  "route-table" = helpRouteTableMarkdown,
  "minto-schnider-table" = helpAlternativeModelTableMarkdown,
  "pregnancy-table" = helpPregnancyTableMarkdown,
  "illicit-pk-table" = helpIllicitPKTableMarkdown,
  "james-lbm-figure" = helpJamesLBMFigure,
  "vasoactive-registry-table" = helpVasoactiveRegistryMarkdown,
  "vasoactive-to-obtain" = helpVasoactiveToObtainMarkdown
)
