# Single Paper Review: the checks, computed in R from the extraction.
#
# Every number in the review report that is not a quotation from the paper
# comes from here, through stanpumpR's own code: getDrugPK() and cube() for
# the eigenvalues and coefficients, effectSitePeakTime() for the time to peak
# effect, twoCompartmentFromCentralVolume(), twoCompartmentFromHalfLives() and
# threeCompartmentFromTriexponential() for the derived routes to a model, and
# simCpCe() for the figures.  Each finding records the places in the paper it
# rests on, and whether it was computed here or is Claude's reading.
#
# The checks follow the library's rules (AGENTS.md): identifiability as
# docs/antihypertensives.md section 4 settles it, only clearance is
# physiologically real, a difference of two measured numbers is a fraught
# measurement, and bioavailability is read, never fitted to another study's
# exposure.  (Claude Code, 2026-10-30, at the request of Steven L. Shafer.)

# The most printed inputs whose rounding the consistency check propagates
# (2^k corners, each a getDrugPK() run).
PAPER_REVIEW_ROUNDING_MAX_INPUTS <- 7

# Agreement thresholds for a recomputed quantity, as a relative difference.
PAPER_REVIEW_CLOSE  <- 0.05
PAPER_REVIEW_FAR    <- 0.20
# A derived model whose parameters spread more than this factor across the
# printed uncertainty is ill-conditioned (the esmolol case).
PAPER_REVIEW_ILL_CONDITIONED <- 2
# Perturbation used when the paper prints no standard error.
PAPER_REVIEW_DEFAULT_PERTURBATION <- 0.05
# The legal patients a covariate equation must stay finite and positive for.
PAPER_REVIEW_SWEEP <- list(age = c(0, 0.1, 1, 5, 18, 40, 65, 90),
                           weight = c(3, 10, 40, 70, 120, 200),
                           height = c(50, 100, 150, 175, 200),
                           sex = c("male", "female"))

#' One finding of the review
#'
#' @param section the report section
#' @param severity "ok", "info", "concern" or "error"
#' @param title,detail text
#' @param locations places in the paper it rests on
#' @param computed TRUE when computed in R, FALSE when it is Claude's reading
#' @keywords internal
paperReviewFinding <- function(section, severity, title, detail = "",
                               locations = character(0), computed = TRUE)
{
  locations <- unique(locations[!is.na(locations) & nzchar(locations)])
  list(section = section, severity = severity, title = title, detail = detail,
       locations = locations, computed = computed)
}

# Format a number for the report.
prFmt <- function(x, digits = 3)
  ifelse(is.finite(x), trimws(formatC(signif(x, digits), format = "fg", digits = digits)), "NA")

#' Half a unit of the last printed digit
#'
#' @param valueText the value as printed
#' @returns the rounding half-width, or 0 when it cannot be read
#' @keywords internal
paperReviewRounding <- function(valueText)
{
  m <- regmatches(valueText %or% "", regexpr("[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?",
                                               valueText %or% ""))
  if (length(m) == 0) return(0)
  mantissa <- sub("[eE].*$", "", m)
  exponent <- if (grepl("[eE]", m)) as.numeric(sub("^.*[eE]", "", m)) else 0
  decimals <- if (grepl("\\.", mantissa)) nchar(sub("^.*\\.", "", mantissa)) else 0
  0.5 * 10^(-decimals + exponent)
}

#' The SE of a printed value, and where it came from
#'
#' @param value the value, in its printed units
#' @param uncertainty the extraction's uncertainty object
#' @param n the number of subjects, to turn an SD into an SE
#' @returns a list: \code{se} (printed units) and \code{basis}
#' @keywords internal
paperReviewSE <- function(value, uncertainty, n = NA_real_)
{
  u <- uncertainty %or% list()
  v <- paperReviewNum(u$value)
  lo <- paperReviewNum(u$lower); hi <- paperReviewNum(u$upper)
  type <- u$type %or% "not_reported"
  if (type == "SE" && !is.na(v)) return(list(se = v, basis = "printed SE"))
  if (type == "RSE_percent" && !is.na(v)) return(list(se = abs(value) * v / 100, basis = "printed RSE"))
  if (type == "CI95" && !is.na(lo) && !is.na(hi))
    return(list(se = (hi - lo) / 3.92, basis = "95% CI / 3.92"))
  if (type == "SD" && !is.na(v) && is.finite(n) && n > 1)
    return(list(se = v / sqrt(n), basis = "SD / sqrt(n)"))
  # No uncertainty printed: the only imprecision the paper does state is its
  # rounding, so that is the perturbation (an arbitrary +/-5% called the
  # Duzitol editorial's exact table ill-conditioned); 5% only when even the
  # printed digits are unknown.
  r <- paperReviewRounding(uncertainty$valueText %or% "")
  if (r > 0) return(list(se = r, basis = "no SE printed; the printed rounding (half the last digit)"))
  list(se = abs(value) * PAPER_REVIEW_DEFAULT_PERTURBATION,
       basis = paste0("none printed; assumed +/-", PAPER_REVIEW_DEFAULT_PERTURBATION * 100, "%"))
}

# --- Derived quantities in internal units ------------------------------------

#' A printed derived quantity in internal units
#'
#' @param dq one entry of \code{extraction$derived_quantities}
#' @param weight the reference weight, for per-kilogram units
#' @param concUnits the model's Concentration.Units
#' @param molecularWeight for molar units
#' @returns a list: \code{value} (NA when not convertible), \code{kind}
#' @keywords internal
paperReviewDerivedInternal <- function(dq, weight, concUnits, molecularWeight = NULL)
{
  value <- paperReviewNum(dq$value)
  units <- dq$units %or% "other"
  unit <- paperReviewUnit(units)
  if (!is.na(unit$kind))
    return(list(value = value * unit$factor * if (unit$perKg) weight else 1,
                kind = unit$kind))
  if (units %in% PAPER_REVIEW_CONCENTRATION_UNITS)
    return(list(value = value * paperReviewConcentrationFactor(units, concUnits, molecularWeight),
                kind = "concentration"))
  if (units == "1/L") return(list(value = value, kind = "coefficient"))     # per unit dose
  if (units %in% PAPER_REVIEW_AUC_UNITS) {
    mass <- sub("\\*.*$", "", units)
    time <- if (grepl("\\*h/", units)) 60 else 1
    perL <- grepl("/L$", units)
    # Printed as mass * time / volume; the model's AUC is concUnits/mL * min.
    massPerMl <- c(pg = 1e-3, ng = 1, mcg = 1e3, mg = 1e6)[[mass]] / if (perL) 1000 else 1
    modelPerMl <- c(pg = 1e-3, ng = 1, mcg = 1e3)[[concUnits]]
    return(list(value = value * time * massPerMl / modelPerMl, kind = "auc"))
  }
  list(value = NA_real_, kind = NA_character_)
}

# The kind each derived quantity must be.
PAPER_REVIEW_DERIVED_KIND <- c(
  t_half_1 = "time", t_half_2 = "time", t_half_3 = "time", t_half_terminal = "time",
  t_half_absorption = "time", lambda_1 = "rate", lambda_2 = "rate", lambda_3 = "rate",
  coefficient_1 = "coefficient", coefficient_2 = "coefficient", coefficient_3 = "coefficient",
  k10 = "rate", k12 = "rate", k21 = "rate", k13 = "rate", k31 = "rate",
  V1 = "volume", Vss = "volume", Varea = "volume", CL = "clearance", MRT = "time",
  MAT = "time", F = "fraction",
  decrement_25 = "time", decrement_50 = "time", decrement_75 = "time", decrement_80 = "time",
  AUC_inf = "auc", AUC_0_t = "auc", Cmax = "concentration", tmax = "time",
  tPeak = "time", ke0 = "rate")

#' The patient a derived quantity was printed for, when not the reference
#'
#' A paper may tabulate CL/F, volumes and half-lives at several weights or
#' ages; each such value is the model at that patient, not at the reference.
#' Compared at the reference, 18 of 19 "errors" in a paediatric paper were
#' false (Devin's stress test, 2026-11-08: 0.319 x (10/70)^0.75 is the
#' printed 0.074 L/min at 10 kg).
#'
#' @param dq one derived quantity
#' @param reference the reference patient
#' @returns the reference patient with the quantity's own covariates in place
#'   and \code{changed}, their names; NULL when none differs from the
#'   reference
#' @keywords internal
paperReviewQuantityPatient <- function(dq, reference)
{
  at <- dq$at
  if (!is.list(at) || length(at) == 0) return(NULL)
  patient <- reference
  changed <- character(0)
  for (k in c("weight", "height", "age")) {
    v <- paperReviewNum(at[[k]])
    if (is.finite(v) && v > 0 && !isTRUE(all.equal(v, reference[[k]]))) {
      patient[[k]] <- v
      changed <- c(changed, k)
    }
  }
  sex <- at[["sex"]]
  if (is.character(sex) && length(sex) == 1 && sex %in% c("male", "female") && sex != reference$sex) {
    patient$sex <- sex
    changed <- c(changed, "sex")
  }
  if (!length(changed)) return(NULL)
  patient$changed <- changed
  patient
}

# How a quantity's own patient is described in the report.
paperReviewPatientText <- function(patient)
  paste(vapply(patient$changed, function(k) switch(k,
    weight = paste(prFmt(patient$weight), "kg"), height = paste(prFmt(patient$height), "cm"),
    age = paste(prFmt(patient$age), "years"), sex = patient$sex), ""), collapse = ", ")

#' The derived quantities of one kind, in internal units
#'
#' Only those printed for the reference patient: a value the paper gives for
#' another patient (\code{at}) describes the model there, and closing or
#' fitting the model at the reference with it would be wrong.
#' @keywords internal
paperReviewDerived <- function(extraction, quantity, weight, concUnits = "ng")
{
  out <- list()
  reference <- NULL
  for (dq in extraction$derived_quantities %or% list())
    if (identical(dq$quantity, quantity)) {
      if (is.list(dq$at) && length(dq$at)) {
        if (is.null(reference)) reference <- paperReviewReferencePatient(extraction)
        if (!is.null(paperReviewQuantityPatient(dq, reference))) next
      }
      v <- paperReviewDerivedInternal(dq, weight, concUnits, extraction$drug$molecular_weight)
      out[[length(out) + 1]] <- c(dq, list(internal = v$value, kind = v$kind))
    }
  out
}

# The first printed derived quantity of a kind, in internal units, or NA.
paperReviewDerivedOne <- function(extraction, quantity, weight)
{
  d <- paperReviewDerived(extraction, quantity, weight)
  if (length(d) == 0) return(list(value = NA_real_, location = NA_character_, entry = NULL))
  kind <- PAPER_REVIEW_DERIVED_KIND[[quantity]]
  # A coefficient may be a fraction of their sum (a percent), which the
  # caller tells apart by its units.
  ok <- !is.na(d[[1]]$kind) && (identical(d[[1]]$kind, kind) ||
    (kind == "coefficient" && identical(d[[1]]$kind, "fraction")))
  list(value = if (ok) d[[1]]$internal else NA_real_, location = d[[1]]$location,
       entry = d[[1]], perKg = isTRUE(paperReviewUnit(d[[1]]$units %or% "other")$perKg))
}

# --- Identifiability ----------------------------------------------------------

# threeCompartmentFromHalfLives(), Shafer's spreadsheet algebra, is
# R/duzitol.R's: the review uses the library's one copy.

#' Two-compartment parameters from a biexponential disposition function
#'
#' The two-exponential counterpart of threeCompartmentFromTriexponential():
#' V1 = 1 / (A + B), k21 = (A lambda2 + B lambda1) / (A + B),
#' k10 = lambda1 lambda2 / k21, k12 = lambda1 + lambda2 - k10 - k21.
#'
#' @param coefficients the two coefficients per unit dose (1/volume)
#' @param exponents the two exponents (1/time)
#' @returns a list of \code{v1}, \code{v2}, \code{cl1}, \code{cl2}
#' @keywords internal
twoCompartmentFromBiexponential <- function(coefficients, exponents)
{
  A <- coefficients[1]; B <- coefficients[2]
  l1 <- exponents[1]; l2 <- exponents[2]
  V1 <- 1 / (A + B)
  k21 <- (A * l2 + B * l1) / (A + B)
  k10 <- l1 * l2 / k21
  k12 <- l1 + l2 - k10 - k21
  if (!all(is.finite(c(V1, k10, k12, k21))) || !all(c(V1, k10, k12, k21) > 0))
    stop("This biexponential has no physically admissible two-compartment model")
  list(v1 = V1, v2 = V1 * k12 / k21, cl1 = V1 * k10, cl2 = V1 * k12)
}

# The printed half-lives (or exponents) of the model, fastest first, in
# minutes, with their locations and SEs in minutes.
paperReviewHalfLives <- function(extraction, n, weight)
{
  # One slot per exponential, NULL where none is printed (assigning NULL to
  # out[[i]] would not create it, and the list came back short).
  out <- vector("list", n)
  for (i in seq_len(n)) {
    q <- if (i == n) c(paste0("t_half_", i), "t_half_terminal") else paste0("t_half_", i)
    found <- NULL
    for (name in q) {
      d <- paperReviewDerivedOne(extraction, name, weight)
      if (!is.na(d$value)) { found <- d; break }
    }
    if (is.null(found)) {
      d <- paperReviewDerivedOne(extraction, paste0("lambda_", i), weight)
      if (!is.na(d$value)) found <- list(value = log(2) / d$value, location = d$location,
                                         entry = d$entry, fromRate = TRUE)
    }
    if (!is.null(found)) out[[i]] <- found
  }
  out
}

#' Does the printed paper close the model?
#'
#' Counts the independent quantities the paper prints against the model's
#' free parameters, as docs/antihypertensives.md section 4 does: a printed
#' parameter set closes the disposition directly; otherwise a derived route
#' is tried (for two compartments, CL, V1 or Vss, and BOTH half-lives; or the
#' biexponential; for three, the triexponential).  A closed route is then
#' perturbed across every corner of its inputs' printed uncertainty: a model
#' that leaves the admissible region, or whose parameters spread by more than
#' PAPER_REVIEW_ILL_CONDITIONED, is formally closed but ill-conditioned.
#'
#' @param extraction the extraction
#' @returns a list: \code{verdict} ("printed", "derived", "ill_conditioned",
#'   "underdetermined"), \code{route}, \code{closed} (v1.. at the reference
#'   patient, or NULL), \code{findings}
#' @keywords internal
paperReviewIdentifiability <- function(extraction)
{
  section <- "Identifiability"
  findings <- list()
  add <- function(...) findings[[length(findings) + 1]] <<- paperReviewFinding(section, ...)
  reference <- paperReviewReferencePatient(extraction)
  w <- reference$weight
  n <- paperReviewNum(extraction$structure$compartments)
  structureLoc <- extraction$structure$location
  if (is.na(n) || !n %in% 1:3) {
    add("error", "The number of compartments is not 1, 2 or 3",
        paste0("The extraction reports ", prFmt(n), " compartments; stanpumpR's closed forms take one to three."),
        structureLoc)
    return(list(verdict = "underdetermined", route = NA_character_, closed = NULL, findings = findings))
  }
  param <- function(role) paperReviewParameterValue(extraction, role, w)
  have <- function(roles) all(vapply(roles, function(r) !is.na(param(r)$value), logical(1)))
  locs <- function(roles) vapply(roles, function(r) param(r)$location, character(1))
  vc <- list(c("CL", "V1"), c("CL", "V1", "Q2", "V2"), c("CL", "V1", "Q2", "V2", "Q3", "V3"))[[n]]
  micro <- list(c("V1", "k10"), c("V1", "k10", "k12", "k21"),
                c("V1", "k10", "k12", "k21", "k13", "k31"))[[n]]
  nFree <- 2 * n

  verdict <- "underdetermined"; route <- NA_character_; closed <- NULL
  # How to rebuild the model with each printed input moved to either end of
  # its rounding (paperReviewConsistency() uses it): k inputs, and a function
  # of their k signs returning the model's drug at the reference patient.
  rounding <- NULL
  if (have(vc) || have(micro)) {
    roles <- if (have(vc)) vc else micro
    verdict <- "printed"; route <- paste(roles, collapse = ", ")
    rounding <- list(n = length(roles), variant = function(signs) {
      x <- extraction
      for (j in seq_along(roles)) {
        k <- which(vapply(x$parameters, function(p) identical(p$role, roles[j]), logical(1)))[1]
        p <- x$parameters[[k]]
        p$value <- paperReviewNum(p$value) + signs[j] * paperReviewRounding(p$value_text)
        x$parameters[[k]] <- p
      }
      paperReviewDrug(x, reference)
    })
    add("ok", paste0("The printed parameters close the ", n, "-compartment disposition"),
        paste0(nFree, " free parameters, ", nFree, " printed: ", route, "."), locs(roles))
  } else {
    hl <- paperReviewHalfLives(extraction, n, w)
    haveHl <- all(vapply(hl, function(x) !is.null(x), logical(1)))
    CL <- param("CL"); if (is.na(CL$value)) CL <- paperReviewDerivedOne(extraction, "CL", w)
    V1 <- param("V1"); if (is.na(V1$value)) V1 <- paperReviewDerivedOne(extraction, "V1", w)
    Vss <- paperReviewDerivedOne(extraction, "Vss", w)
    coefs <- lapply(seq_len(n), function(i) paperReviewDerivedOne(extraction, paste0("coefficient_", i), w))
    lambdas <- lapply(seq_len(n), function(i) paperReviewDerivedOne(extraction, paste0("lambda_", i), w))
    perL <- all(vapply(coefs, function(x) identical(x$entry$units, "1/L"), logical(1)))
    attempt <- NULL
    if (n == 1) {
      if (!is.na(CL$value) && haveHl) {
        attempt <- list(name = "CL and the half-life",
                        inputs = list(CL = CL, thalf = hl[[1]]),
                        solve = function(x) list(v1 = x$CL * x$thalf / log(2), cl1 = x$CL))
      } else if (!is.na(V1$value) && haveHl) {
        attempt <- list(name = "V and the half-life",
                        inputs = list(V1 = V1, thalf = hl[[1]]),
                        solve = function(x) list(v1 = x$V1, cl1 = x$V1 * log(2) / x$thalf))
      }
    } else if (n == 2 && haveHl && !is.na(CL$value) && !is.na(V1$value)) {
      attempt <- list(name = "CL, V1 and both half-lives",
                      inputs = list(CL = CL, V1 = V1, t1 = hl[[1]], t2 = hl[[2]]),
                      solve = function(x) twoCompartmentFromCentralVolume(x$CL, x$V1, x$t1, x$t2))
    } else if (n == 2 && haveHl && !is.na(CL$value) && !is.na(Vss$value)) {
      attempt <- list(name = "CL, Vss and both half-lives",
                      inputs = list(CL = CL, Vss = Vss, t1 = hl[[1]], t2 = hl[[2]]),
                      solve = function(x) twoCompartmentFromHalfLives(x$CL, x$Vss, x$t1, x$t2))
    } else if (n == 3 && haveHl && !is.na(CL$value) && !is.na(V1$value) && !is.na(Vss$value)) {
      attempt <- list(name = "CL, V1, Vss and the three half-lives",
                      inputs = list(CL = CL, V1 = V1, Vss = Vss, t1 = hl[[1]], t2 = hl[[2]], t3 = hl[[3]]),
                      solve = function(x) threeCompartmentFromHalfLives(x$CL, x$V1, x$Vss, x$t1, x$t2, x$t3))
    } else if (n >= 2 && haveHl && have(c("V1", "k21", if (n == 3) "k31"))) {
      # Convert.xls tab 5: V1, the exponents and the return rate constants.
      inputs <- c(list(V1 = param("V1"), k21 = param("k21")), if (n == 3) list(k31 = param("k31")),
                  stats::setNames(hl, paste0("t", seq_len(n))))
      attempt <- list(
        name = if (n == 2) "V1, the half-lives and k21" else "V1, the half-lives, k21 and k31",
        inputs = inputs,
        solve = function(x) compartmentsFromExponentsAndRates(
          x$V1, log(2) / unname(unlist(x[paste0("t", seq_len(n))])), x$k21, x$k31))
    } else if (perL && all(vapply(lambdas, function(x) !is.na(x$value), logical(1)))) {
      inputs <- c(stats::setNames(coefs, paste0("A", seq_len(n))),
                  stats::setNames(lambdas, paste0("l", seq_len(n))))
      attempt <- list(
        name = if (n == 2) "the biexponential (coefficients per unit dose and exponents)"
               else "the triexponential (coefficients per unit dose and exponents)",
        inputs = inputs,
        solve = function(x) {
          A <- unname(unlist(x[paste0("A", seq_len(n))])); l <- unname(unlist(x[paste0("l", seq_len(n))]))
          if (n == 2) twoCompartmentFromBiexponential(A, l)
          else threeCompartmentFromTriexponential(A, l)
        })
    }
    # The fractional coefficients, A(i) / sum(A), never close a model: they
    # are always derived, never primary (Shafer, 2026-11-01), and are only
    # compared with the model in the consistency table.

    if (is.null(attempt)) {
      printed <- c(
        if (!is.na(CL$value)) "CL", if (!is.na(V1$value)) "V1", if (!is.na(Vss$value)) "Vss",
        vapply(seq_len(n), function(i) if (!is.null(hl[[i]])) paste0("half-life ", i) else NA_character_,
               character(1)))
      printed <- printed[!is.na(printed)]
      add("error", paste0("The paper does not close the ", n, "-compartment model"),
          paste0(nFree, " free parameters; independent quantities printed: ",
                 if (length(printed)) paste(printed, collapse = ", ") else "none",
                 ". ", if (n == 2) "For two compartments, CL with V1 or Vss and BOTH half-lives closes it; CL, Vss and the terminal half-life alone do not." else ""),
          c(structureLoc, CL$location, V1$location, Vss$location))
      # The displayed curve can close what the print does not: fit the
      # model's number of exponentials to a digitised intravenous series and
      # turn them into volumes and clearances with the existing helpers.
      fromCurve <- paperReviewCloseFromCurve(extraction, reference, n)
      if (!is.null(fromCurve)) {
        closed <- fromCurve$closed
        verdict <- "derived_from_curve"
        route <- fromCurve$route
        add("concern", paste("Closed instead from", fromCurve$route),
            paste0("A sum of ", n, " exponentials fitted to the digitised points, scaled by the ",
                   "dose, gives V1 ", prFmt(closed$v1), " L and CL ", prFmt(closed$cl1 * 60),
                   " L/h. Only as good as Claude's reading of the figure: an approximation to ",
                   "check, not a measurement."), fromCurve$locations)
      }
    } else {
      x0 <- lapply(attempt$inputs, function(i) i$value)
      base <- tryCatch(attempt$solve(x0), error = function(e) NULL)
      inputLocs <- vapply(attempt$inputs, function(i) i$location %or% NA_character_, character(1))
      if (is.null(base)) {
        add("error", paste("The printed", attempt$name, "admit no physical model"),
            "The decomposition gives a negative or non-finite rate constant or volume.",
            inputLocs)
      } else {
        conditioning <- paperReviewConditioning(attempt, extraction, reference)
        closed <- base
        closed$perKg <- isTRUE(CL$perKg) || isTRUE(V1$perKg)
        half <- vapply(attempt$inputs, function(i) {
          printed <- paperReviewNum(i$entry$value)
          r <- paperReviewRounding(i$entry$value_text %or% "")
          if (is.finite(printed) && printed != 0) abs(r * i$value / printed) else 0
        }, numeric(1))
        perKg <- closed$perKg
        rounding <- list(n = length(half), variant = function(signs) {
          x <- as.list(stats::setNames(x0, names(attempt$inputs)))
          for (j in seq_along(x)) x[[j]] <- x[[j]] + signs[j] * half[j]
          c2 <- tryCatch(attempt$solve(x), error = function(e) NULL)
          if (is.null(c2)) return(NULL)
          c2$perKg <- perKg
          paperReviewDrug(extraction, reference, c2)
        })
        route <- attempt$name
        if (conditioning$illConditioned) {
          verdict <- "ill_conditioned"
          add("concern", paste("Formally closed by", attempt$name, "but ill-conditioned"),
              conditioning$text, inputLocs)
        } else {
          verdict <- "derived"
          add("ok", paste("Closed by", attempt$name, "and stable"), conditioning$text, inputLocs)
        }
      }
    }
  }

  # Effect site.  (Absorption is derived per regimen in paperReviewRun().)
  effect <- extraction$structure$effect_site %or% "none"
  if (effect == "ke0" && is.na(param("ke0")$value))
    add("error", "The paper describes an effect site but prints no ke0", "", structureLoc)
  if (effect == "tpeak" && is.na(param("tPeak")$value))
    add("error", "The paper describes an effect site but prints no time to peak effect", "", structureLoc)
  if (effect %in% c("pd_without_effect_site", "other"))
    add("info", "The pharmacodynamic model is not an effect-site model",
        paste0("The simulator plots concentrations only. ", extraction$structure$pd_model %or% ""),
        structureLoc)

  list(verdict = verdict, route = route, closed = closed, findings = findings, rounding = rounding)
}

#' Perturb a derived route across its inputs' printed uncertainty
#'
#' Every corner of the box (each input at -1 SE, 0, +1 SE: 3^k corners) is
#' solved again.  The SE comes from the paper where it prints one, otherwise
#' an assumed PAPER_REVIEW_DEFAULT_PERTURBATION, which the text says.
#'
#' @returns a list: \code{illConditioned}, \code{text}
#' @keywords internal
paperReviewConditioning <- function(attempt, extraction, reference)
{
  n <- paperReviewNum(extraction$population$n)
  ses <- lapply(attempt$inputs, function(i) {
    entry <- i$entry
    printed <- paperReviewNum(entry$value)
    scale <- if (!is.na(printed) && printed != 0) i$value / printed else 1
    s <- paperReviewSE(printed, c(entry$uncertainty %or% list(), list(valueText = entry$value_text)), n)
    list(se = abs(s$se * scale), basis = s$basis)
  })
  k <- length(attempt$inputs)
  corners <- as.matrix(expand.grid(rep(list(c(-1, 0, 1)), k)))
  x0 <- vapply(attempt$inputs, function(i) i$value, numeric(1))
  se <- vapply(ses, function(s) s$se, numeric(1))
  fits <- lapply(seq_len(nrow(corners)), function(r) {
    x <- as.list(stats::setNames(pmax(x0 + corners[r, ] * se, 1e-12), names(attempt$inputs)))
    tryCatch(unlist(attempt$solve(x)), error = function(e) NULL)
  })
  ok <- !vapply(fits, is.null, logical(1))
  admissible <- sum(ok)
  spread <- if (admissible > 1) {
    m <- do.call(rbind, fits[ok])
    apply(m, 2, function(col) if (all(col > 0)) max(col) / min(col) else Inf)
  } else NA
  worst <- if (all(is.na(spread))) NA else max(spread, na.rm = TRUE)
  ill <- admissible < length(fits) || (is.finite(worst) && worst > PAPER_REVIEW_ILL_CONDITIONED) ||
    !is.finite(worst)
  bases <- unique(vapply(ses, function(s) s$basis, character(1)))
  text <- paste0(
    "Each of the ", k, " inputs was moved by -1, 0 and +1 standard error (",
    paste(bases, collapse = "; "), "): ", admissible, " of ", length(fits),
    " corners give an admissible model",
    if (is.finite(worst)) paste0(", and the most sensitive parameter spreads ", prFmt(worst, 2), "-fold") else "",
    ". ", if (ill) paste0("A spread beyond ", PAPER_REVIEW_ILL_CONDITIONED,
                          "-fold, or a corner with no admissible model, means the printed numbers do not pin the model down (as for esmolol).")
          else "The printed numbers pin the model down.")
  list(illConditioned = ill, text = text, admissible = admissible, corners = length(fits),
       spread = worst)
}

# --- Internal consistency ----------------------------------------------------

#' Recompute the derived quantities from the model and compare
#'
#' @param extraction the extraction
#' @param drug paperReviewDrug() at the reference patient
#' @param reference the reference patient
#' @returns a list: \code{table} (a data frame) and \code{findings}
#' @keywords internal
paperReviewConsistency <- function(extraction, drug, reference, ident = NULL)
{
  section <- "Internal consistency"
  findings <- list()
  rows <- list()
  if (!drug$ok)
    return(list(table = NULL, findings = list(paperReviewFinding(section, "info",
      "Not checked: there is no complete model to recompute from"))))
  w <- reference$weight
  concUnits <- drug$defaults$Concentration.Units
  run <- tryCatch(paperReviewQuietly(function()
    getDrugPK(drug$defaults$Drug, reference$weight, reference$height, reference$age,
              reference$sex, drug$defaults, model = drug$model)),
    error = function(e) e)
  # The reason goes to the report, which only the user sees.
  if (inherits(run, "error"))
    return(list(table = NULL, findings = list(paperReviewFinding(section, "error",
      "stanpumpR could not build the model from the printed parameters",
      conditionMessage(run)))))
  pk <- run$PK$default
  n <- paperReviewNum(extraction$structure$compartments)
  # paperReviewDisposition() builds one compartment when the extraction's
  # count is missing or outside 1-3; that stand-in says nothing about what
  # the paper prints, so no row is checked against it (the identifiability
  # section says why).
  unknownN <- is.na(n) || !n %in% 1:3
  if (unknownN) n <- 1
  # What a PK set gives the rows: its exponents, coefficients, Vss, ke0, tPeak.
  quantitiesOf <- function(set, weight, patient) {
    l <- c(set$lambda_1, set$lambda_2, set$lambda_3)[seq_len(n)]
    a <- c(set$p_coef_bolus_l1, set$p_coef_bolus_l2, set$p_coef_bolus_l3)[seq_len(n)]
    k <- if (isTRUE(set$ke0 > 0)) set$ke0 else NA_real_
    # The placeholder volume of an absent compartment is not part of Vss.
    list(pk = set, lambda = l, coef = a,
         vss = set$v1 + (if (n >= 2) set$v2 else 0) + (if (n >= 3) set$v3 else 0),
         ke0 = k, tPeak = if (is.finite(k)) effectSitePeakTime(a, l, k) else NA_real_,
         w = weight, patient = patient)
  }
  atReference <- quantitiesOf(pk, w, reference)
  # The model at a patient a derived quantity was printed for (its `at`),
  # built as the reference's is, or an error message.
  quantitiesAt <- function(patient) {
    built <- tryCatch(paperReviewQuietly(function() {
      d <- paperReviewDrug(extraction, patient, drug$closed, absorption = drug$absorption,
                           absorptions = drug$absorptions)
      getDrugPK(d$defaults$Drug, patient$weight, patient$height, patient$age, patient$sex,
                d$defaults, model = d$model)$PK$default
    }), error = function(e) conditionMessage(e))
    if (is.character(built)) built else quantitiesOf(built, patient$weight, patient)
  }
  lambda <- atReference$lambda; coef <- atReference$coef; vss <- atReference$vss
  ke0 <- atReference$ke0; tPeak <- atReference$tPeak
  # Derived quantities an absorption was built from (R/paper-review-absorption.R).
  usedKeys <- unlist(lapply(Filter(function(a) isTRUE(a$ok), drug$absorptions %or% list()),
                            function(a) a$used))

  # The model at every corner of its printed inputs' rounding, built only if
  # a printed value does not already match.  A derived quantity can move far
  # when the inputs are printed to two figures: the Duzitol editorial's own
  # steady-state decrement times (41, 10, 20 min) lie inside the ranges its
  # Table 1 allows (39-44, 9.6-10.8, 17-27) though not all at the centre.
  corners <- NULL
  cornerSets <- function() {
    if (!is.null(corners)) return(corners)
    corners <<- list()
    r <- ident$rounding
    if (is.null(r) || r$n == 0 || r$n > PAPER_REVIEW_ROUNDING_MAX_INPUTS) return(corners)
    signs <- as.matrix(expand.grid(rep(list(c(-1, 1)), r$n)))
    for (s in seq_len(nrow(signs))) {
      d <- tryCatch(r$variant(unname(signs[s, ])), error = function(e) NULL)
      if (is.null(d) || !isTRUE(d$ok)) next
      p2 <- tryCatch(paperReviewQuietly(function()
        getDrugPK(d$defaults$Drug, reference$weight, reference$height, reference$age,
                  reference$sex, d$defaults, model = d$model)$PK$default), error = function(e) NULL)
      if (!is.null(p2)) corners[[length(corners) + 1]] <<- p2
    }
    corners
  }
  # One analytic quantity of a PK set: what recomputed() gives for the centre.
  analytic <- function(dq, set) {
    l <- c(set$lambda_1, set$lambda_2, set$lambda_3)[seq_len(n)]
    c0 <- c(set$p_coef_bolus_l1, set$p_coef_bolus_l2, set$p_coef_bolus_l3)[seq_len(n)]
    v <- set$v1 + (if (n >= 2) set$v2 else 0) + (if (n >= 3) set$v3 else 0)
    q <- dq$quantity
    i <- suppressWarnings(as.integer(sub("^.*_", "", q)))
    switch(q,
      t_half_1 = , t_half_2 = , t_half_3 = if (isTRUE(i <= n)) log(2) / l[i] else NA_real_,
      t_half_terminal = log(2) / l[n],
      lambda_1 = , lambda_2 = , lambda_3 = if (isTRUE(i <= n)) l[i] else NA_real_,
      coefficient_1 = , coefficient_2 = , coefficient_3 =
        if (!isTRUE(i <= n)) NA_real_
        else if (identical(dq$units, "1/L")) c0[i]
        else if (identical(paperReviewUnit(dq$units %or% "other")$kind, "fraction")) c0[i] / sum(c0)
        else NA_real_,
      k10 = set$k10, k12 = set$k12, k21 = set$k21, k13 = set$k13, k31 = set$k31,
      V1 = set$v1, Vss = v, Varea = set$cl1 / l[n], CL = set$cl1,
      MRT = if (is.null(dq$regimen_id)) v / set$cl1 else NA_real_,
      decrement_25 = , decrement_50 = , decrement_75 = , decrement_80 =
        heldInfusionDecrementTime(set, Inf, as.numeric(sub("^decrement_", "", q)) / 100),
      NA_real_)
  }

  recomputed <- function(dq) {
    q <- dq$quantity
    i <- suppressWarnings(as.integer(sub("^.*_", "", q)))
    switch(q,
      t_half_1 = , t_half_2 = , t_half_3 = if (i <= n) log(2) / lambda[i] else NA_real_,
      t_half_terminal = log(2) / lambda[n],
      lambda_1 = , lambda_2 = , lambda_3 = if (i <= n) lambda[i] else NA_real_,
      k10 = pk$k10, k12 = pk$k12, k21 = pk$k21, k13 = pk$k13, k31 = pk$k31,
      V1 = pk$v1, Vss = vss, Varea = pk$cl1 / lambda[n], CL = pk$cl1, MRT = vss / pk$cl1,
      ke0 = ke0, tPeak = tPeak,
      decrement_25 = , decrement_50 = , decrement_75 = , decrement_80 =
        heldInfusionDecrementTime(pk, Inf, as.numeric(sub("^decrement_", "", q)) / 100),
      NA_real_)
  }

  for (dq in extraction$derived_quantities %or% list())
  {
    q <- dq$quantity %or% "other"
    if (q %in% c("other", "AUC_0_t", "t_half_absorption")) next
    # A quantity outside the schema's choices (named in the extraction's
    # limitations by paperReviewValidate()), or one with no readable value
    # (set aside by paperReviewConform()), cannot be recomputed.
    if (!q %in% names(PAPER_REVIEW_DERIVED_KIND) || !is.finite(paperReviewNum(dq$value))) next
    if (unknownN) {
      rows[[length(rows) + 1]] <- data.frame(
        quantity = q, printed = dq$value_text %or% prFmt(paperReviewNum(dq$value)),
        units = dq$units %or% "", model = "", relative = "", status = "not comparable",
        location = dq$location %or% "",
        note = "The number of compartments is not known; the model is a one-compartment stand-in.",
        stringsAsFactors = FALSE)
      next
    }
    # The patient the value was printed for: the reference, or the quantity's
    # own (its `at`), at which the whole model is rebuilt and compared.
    own <- paperReviewQuantityPatient(dq, reference)
    ctx <- if (is.null(own)) atReference else quantitiesAt(own)
    if (is.character(ctx)) {
      rows[[length(rows) + 1]] <- data.frame(
        quantity = q, printed = dq$value_text %or% prFmt(paperReviewNum(dq$value)),
        units = dq$units %or% "", model = "", relative = "", status = "not recomputed",
        location = dq$location %or% "",
        note = paste0("Printed for ", paperReviewPatientText(own), "; the model could not be built ",
                      "for that patient (", ctx, ")."),
        stringsAsFactors = FALSE)
      next
    }
    pk <- ctx$pk; lambda <- ctx$lambda; coef <- ctx$coef; vss <- ctx$vss
    ke0 <- ctx$ke0; tPeak <- ctx$tPeak
    patientQ <- ctx$patient
    printedValue <- paperReviewNum(dq$value)
    conv <- paperReviewDerivedInternal(dq, ctx$w, concUnits, extraction$drug$molecular_weight)
    expected <- PAPER_REVIEW_DERIVED_KIND[[q]]
    model <- NA_real_
    note <- if (is.null(own)) "" else
      paste0("Printed for ", paperReviewPatientText(own), ", where the model is evaluated, not at the ",
             "reference patient.")
    incomparable <- FALSE
    if (expected == "coefficient") {
      i <- as.integer(sub("^.*_", "", q))
      if (i > n) next
      if (identical(dq$units, "1/L")) {
        model <- coef[i]; conv$kind <- "coefficient"
      } else if (identical(conv$kind, "fraction")) {
        # A fractional coefficient, A(i) / sum(A): the weight of each exponent
        # in the fall after a bolus, often printed as a percent.
        model <- coef[i] / sum(coef); conv$kind <- "coefficient"
        note <- trimws(paste(note, "A fraction of the sum of the coefficients."))
      } else if (identical(conv$kind, "concentration")) {
        bolus <- paperReviewBolusAmount(extraction, dq$regimen_id, concUnits, patientQ,
                                        log(2) / lambda[1])
        if (!is.na(bolus$amount)) {
          model <- coef[i] * bolus$amount; conv$kind <- "coefficient"
          note <- trimws(paste(note, bolus$note))
        } else {
          note <- trimws(paste(note, "No single intravenous dose to scale the coefficient by."))
        }
      }
    } else if (q %in% c("AUC_inf", "Cmax", "tmax", "MRT") && !is.null(dq$regimen_id)) {
      # A quantity of one regimen is compared only if the model can give that
      # regimen: an oral AUC against a model with no oral route is not a
      # disagreement (the first real paper reported both).
      regimen <- paperReviewRegimen(extraction, dq$regimen_id)
      routable <- if (is.null(regimen)) list(ok = FALSE, why = "The regimen is not identified.")
                  else paperReviewRegimenRoutable(regimen, drug)
      key <- paste(q, dq$regimen_id, sep = "|")
      if (key %in% usedKeys) {
        incomparable <- TRUE
        routable$ok <- FALSE
        note <- "This regimen's absorption was derived from it, so it cannot also check it."
      } else if (!routable$ok) {
        incomparable <- TRUE
        note <- routable$why
      } else if (q == "MRT") {
        model <- paperReviewRegimenMRT(regimen, paperReviewDrugFor(drug, extraction, regimen, patientQ),
                                       vss / pk$cl1)
        if (!is.finite(model))
          note <- trimws(paste(note, "The regimen's input time is not known, so neither is its mean residence time."))
      } else {
        res <- paperReviewRegimenExposure(extraction, paperReviewDrugFor(drug, extraction, regimen, patientQ),
                                          patientQ, dq$regimen_id)
        model <- res[[q]]
        note <- trimws(paste(note, res$note))
      }
      if (q %in% c("Cmax", "tmax") && !identical(dq$statistic, "typical_value") && routable$ok)
        note <- paste0(note, " The paper's value is a ",
                       gsub("_", " ", dq$statistic %or% "unstated statistic"),
                       "; the model's is the typical subject's curve, which need not equal it.")
    } else {
      model <- recomputed(dq)
      if (grepl("^decrement_", q) && is.finite(model))
        note <- trimws(paste(note, paperReviewSteadyStateNote(pk, as.numeric(sub("^decrement_", "", q)) / 100, model)))
    }
    kindOk <- identical(conv$kind, expected)
    printedInternal <- if (kindOk) conv$value else NA_real_
    relative <- (model - printedInternal) / printedInternal
    roundingInternal <- paperReviewRounding(dq$value_text) * if (is.finite(printedValue) && printedValue != 0)
      printedInternal / printedValue else NA_real_
    withinRounding <- is.finite(model) && is.finite(printedInternal) &&
      isTRUE(abs(model - printedInternal) <= roundingInternal * 1.0001)
    loose <- q %in% c("Cmax", "tmax") && !identical(dq$statistic, "typical_value")
    # Not a match at the centre: is it inside what the inputs' rounding allows?
    withinInputs <- FALSE
    if (kindOk && is.finite(model) && is.finite(printedInternal) && !incomparable && is.null(own) &&
        !(withinRounding || abs(relative) <= 0.01)) {
      range <- range(c(model, vapply(cornerSets(), function(set) analytic(dq, set), numeric(1))),
                     na.rm = TRUE)
      own <- if (is.finite(roundingInternal)) roundingInternal else 0
      if (all(is.finite(range)) && diff(range) > 0 &&
          printedInternal >= range[1] - own && printedInternal <= range[2] + own) {
        withinInputs <- TRUE
        f <- if (is.finite(printedValue) && printedValue != 0) printedValue / printedInternal else 1
        note <- trimws(paste(note, paste0("Within the ", prFmt(range[1] * f, 3), " to ",
                                          prFmt(range[2] * f, 3), " that the rounding of the printed ",
                                          "inputs allows.")))
      }
    }
    status <- if (incomparable && grepl("derived from it", note)) "used to build the model"
      else if (withinInputs) "matches within rounding"
      else if (incomparable) "not comparable"
      else if (!kindOk && nzchar(note)) "not recomputed"
      else if (!kindOk) "units not convertible"
      else if (!is.finite(model)) "not recomputed"
      else if (withinRounding || abs(relative) <= 0.01) "matches"
      else if (abs(relative) <= PAPER_REVIEW_CLOSE) "close"
      else if (abs(relative) <= PAPER_REVIEW_FAR || loose) "differs"
      else "disagrees"
    rows[[length(rows) + 1]] <- data.frame(
      quantity = q, printed = dq$value_text %or% prFmt(printedValue),
      units = dq$units %or% "", model = if (kindOk && is.finite(model) && is.finite(printedValue) && printedValue != 0)
        prFmt(model * printedValue / printedInternal, 4) else "",
      relative = if (is.finite(relative)) sprintf("%+.1f%%", 100 * relative) else "",
      status = status, location = dq$location %or% "", note = trimws(note),
      stringsAsFactors = FALSE)
    if (status %in% c("differs", "disagrees"))
      findings[[length(findings) + 1]] <- paperReviewFinding(section,
        if (status == "disagrees") "error" else "concern",
        paste0(q, " as printed (", dq$value_text, " ", dq$units, ") ", status,
               " with the model's own parameters"),
        paste0("Recomputed from the printed parameters: ",
               rows[[length(rows)]]$model, " ", dq$units, " (", rows[[length(rows)]]$relative, "). ", note),
        dq$location)
  }
  # The rest is about the reference patient.
  pk <- atReference$pk; lambda <- atReference$lambda; coef <- atReference$coef
  vss <- atReference$vss; ke0 <- atReference$ke0; tPeak <- atReference$tPeak
  table <- if (length(rows)) do.call(rbind, rows) else NULL
  if (!is.null(table)) {
    compared <- table$status %in% c("matches", "close", "matches within rounding", "differs", "disagrees")
    nMatch <- sum(table$status %in% c("matches", "close", "matches within rounding"))
    findings <- c(list(paperReviewFinding(section,
      if (nMatch == sum(compared)) "ok" else "info",
      paste0(nMatch, " of ", sum(compared), " printed derived quantities compared agree with the model ",
             "(within 5%, or within what the rounding of the printed inputs allows)"),
      paste0("Each was recomputed from the printed parameters through stanpumpR (getDrugPK(), cube(), ",
             "effectSitePeakTime(), simCpCe()).",
             if (any(!compared)) paste0(" ", sum(!compared), " more could not be compared; the table says why.")
             else ""))),
      findings)
  } else {
    findings <- list(paperReviewFinding(section, "info",
      "The paper prints no derived quantities to check against the model"))
  }
  list(table = table, findings = findings, pkTable = paperReviewPKTable(pk))
}

#' How long an infusion takes to reach a steady-state decrement time
#'
#' A printed decrement time with no duration is the steady-state one, the
#' limit of the curve after an infusion of unlimited length.  The limit can
#' be far off: the Duzitol editorial's hepatic-failure model has a 50%
#' decrement time of 9.9 min at steady state but 8.2 min after 10 h of TCI,
#' and comes within 5% of the limit only after about a day.
#'
#' @param set a PK set
#' @param fall the fraction the concentration falls (0.5 for 50%)
#' @param steady the steady-state decrement time (min)
#' @returns a sentence for the consistency table's note
#' @keywords internal
paperReviewSteadyStateNote <- function(set, fall, steady)
{
  dec <- function(T) heldInfusionDecrementTime(set, T, fall)
  gap <- function(logT) dec(exp(logT)) - 0.95 * steady
  T95 <- tryCatch(if (gap(log(1e6)) > 0 && gap(log(0.01)) < 0)
    exp(stats::uniroot(gap, log(c(0.01, 1e6)), tol = 1e-6)$root) else NA_real_,
    error = function(e) NA_real_)
  paste0("At steady state, after an infusion of unlimited length.",
         if (is.finite(T95)) paste0(" With the plasma concentration held constant (TCI), the model comes ",
                                    "within 5% of it after ", paperReviewDuration(T95), ".") else "")
}

#' A duration in minutes, written in the unit that reads naturally
#' @keywords internal
paperReviewDuration <- function(minutes)
{
  if (minutes < 120) paste(prFmt(minutes, 2), "min")
  else if (minutes < 2 * 1440) paste(prFmt(minutes / 60, 2), "h")
  else paste(prFmt(minutes / 1440, 2), "days")
}

#' Check a figure of decrement time against infusion duration
#'
#' The context-sensitive decrement time (the time for the concentration to
#' fall by decrement_percent after a constant infusion of the plotted
#' duration stops) is computed from the model at every digitised duration
#' and compared with the plotted value.  The Duzitol editorial's Figure 1 is
#' such a figure: its whole point is that the curves cannot be read off the
#' table of half-lives.
#'
#' @returns a list: \code{finding}, and \code{figure} for the report's plot
#' @keywords internal
paperReviewDecrementFigure <- function(f, drug, reference, family = list(f))
{
  id <- f$figure_id %or% "Figure"
  fall <- paperReviewNum(f$decrement_percent) / 100
  minutes <- paperReviewMinutes(f$time_units %or% "min")
  if (!is.finite(fall)) fall <- 0.5
  modes <- if (isTRUE(drug$ok)) paperReviewDispositionModes(drug, reference) else NULL
  if (is.null(modes) || is.na(minutes))
    return(list(finding = paperReviewFinding("Figures", "info", paste(id, "not compared"),
                                             "There is no complete model, or its time units are unknown.",
                                             f$location)))
  points <- do.call(rbind, lapply(f$series %or% list(), function(s) {
    p <- s$points %or% list()
    if (length(p) == 0) return(NULL)
    data.frame(series = s$label %or% "",
               time = vapply(p, function(x) paperReviewNum(x$time), numeric(1)) * minutes,
               observed = vapply(p, function(x) paperReviewNum(x$value), numeric(1)) * minutes,
               lower = NA_real_, upper = NA_real_, stringsAsFactors = FALSE)
  }))
  if (is.null(points) || nrow(points) == 0)
    return(list(finding = paperReviewFinding("Figures", "info", paste(id, "has no digitised points"),
                                             f$digitization_note %or% "", f$location)))
  site <- f$decrement_site %or% "plasma"
  dec <- if (identical(site, "effect_site"))
    function(T) paperReviewEffectSiteDecrementTime(drug, reference, fall, T)
  else
    function(T) heldInfusionDecrementTime(drug$model$PK$default, T, fall)
  points$predicted <- vapply(points$time, function(T) if (T > 0) dec(T) else NA_real_, numeric(1))
  use <- is.finite(points$observed) & points$observed > 0 & is.finite(points$predicted) &
    points$predicted > 0
  pe <- (points$observed[use] - points$predicted[use]) / points$predicted[use]
  mdpe <- stats::median(pe); mdape <- stats::median(abs(pe))
  # A curve read by eye is off by about the same number of minutes wherever
  # it is read, so a short decrement time carries a large relative error
  # that says nothing about the model: on the Duzitol editorial's 30% curve,
  # 2.5 to 12.5 min, a reading good to +/-3 min was -31% off.  Each point is
  # also judged against the precision of the reading.
  # Where the curve is steep (the 80% and 90% curves leave the axis almost
  # at once) a small misreading of the duration is a large one of the
  # decrement time, so a point also agrees when the model reaches its value
  # within the precision of the duration read.
  precision <- paperReviewReadingPrecision(f, family, "y")
  xPrecision <- paperReviewReadingPrecision(f, family, "x")
  tolY <- precision$value * minutes; tolX <- xPrecision$value * minutes
  difference <- abs(points$observed[use] - points$predicted[use])
  mdad <- stats::median(difference)
  xDifference <- if (identical(site, "effect_site")) rep(Inf, sum(use)) else
    vapply(which(use), function(i) {
      target <- points$observed[i]
      top <- 20 * max(points$time, na.rm = TRUE)
      gap <- function(T) dec(T) - target
      lo <- max(points$time, na.rm = TRUE) / 1e4
      if (!(gap(lo) < 0 && gap(top) > 0)) return(Inf)
      abs(stats::uniroot(gap, c(lo, top), tol = 1e-6)$root - points$time[i])
    }, numeric(1))
  distance <- pmin(difference / max(tolY, 1e-12), xDifference / max(tolX, 1e-12))
  within <- sum(distance <= 1.0001)
  # Once the precision of the reading is known it decides: a median error of
  # 19% passed the Duzitol editorial's propofol curve, every point of which
  # lay 1-3 min above the model on a 0-50 min axis.
  severity <- if (!is.finite(mdape)) "info"
    else if (tolY > 0) {
      if (stats::median(distance) <= 1) "ok" else if (stats::median(distance) <= 3) "concern" else "error"
    }
    else if (mdape <= 0.2) "ok" else if (mdape <= 0.5) "concern" else "error"
  grid <- seq(max(points$time, na.rm = TRUE) / 200, max(points$time, na.rm = TRUE) * 1.02,
              length.out = if (identical(site, "effect_site")) 30 else 200)
  finding <- paperReviewFinding("Figures", severity,
    paste0(paperReviewFigureTitle(id, points$series), ": the model ",
           if (severity == "ok") "reproduces" else "does not closely reproduce",
           " the ", round(100 * fall), "% decrement times"),
    paste0(sum(use), " points compared. Each is the time for the ",
           if (identical(site, "effect_site")) "effect-site" else "plasma",
           " concentration to fall ", round(100 * fall), "% after it has been held constant by TCI ",
           "for the plotted duration, computed from the model (",
           if (identical(site, "effect_site")) "stanpumpR's effect-site TCI, simCpCe()"
           else "heldInfusionDecrementTime()",
           "). Median prediction error ", sprintf("%+.0f%%", 100 * mdpe),
           ", median absolute ", sprintf("%.0f%%", 100 * mdape), "; median difference ",
           prFmt(mdad / minutes, 2), " ", f$time_units %or% "min", ". ", within, " of ", sum(use),
           " points are within the reading's precision: +/-", prFmt(precision$value, 2), " ",
           f$time_units %or% "min", " in the decrement time (", precision$basis, ")",
           if (!identical(site, "effect_site")) paste0(", or the model's curve reaches the value within +/-",
             prFmt(xPrecision$value, 2), " ", f$time_units %or% "min", " of the duration read (",
             xPrecision$basis, ")") else "",
           ". The points were digitised ",
           "approximately by Claude (", f$digitization_note %or% "", ")."),
    f$location)
  list(finding = finding,
       figure = list(id = id, caption = f$caption %or% "", logY = FALSE, kind = "decrement",
                     yLabel = paste0("Time for a ", round(100 * fall), "% fall"),
                     xLabel = "Infusion duration",
                     curve = data.frame(Time = grid, Cp = vapply(grid, dec, numeric(1)), regimen = "model"),
                     points = points, mdpe = mdpe, mdape = mdape))
}

#' The figures that share a figure's axes
#'
#' Claude may extract one figure as several entries, one per curve
#' ("Figure 3 (10%)", "Figure 3 (20%)", ...): they share the figure's axes,
#' so they share the precision of a reading.  So do the curves of the
#' paper's other models (the Duzitol editorial's Figure 1 plots three), which
#' one model's part of the extraction keeps as paperFigures.
#' @keywords internal
paperReviewFigureFamily <- function(f, extraction)
{
  base <- function(x) trimws(sub("\\s*\\(.*$", "", x$figure_id %or% ""))
  family <- Filter(function(g) identical(base(g), base(f)),
                   extraction$paperFigures %or% extraction$figures %or% list())
  if (length(family) == 0) list(f) else family
}

#' How precisely a figure's points were read
#'
#' A curve read by eye is off by about the same amount anywhere on its axis,
#' so the precision is a fraction of the axis, not of the curve: the
#' Duzitol editorial's 10% decrement curve, near zero on a 0-120 min axis,
#' was judged against 2% of its own values (+/-0.04 min) and failed.  In
#' order: Claude's own estimate (reading_precision, or a "+/-" in its
#' digitisation note, both for the y axis), else 2% of the printed axis
#' (y_axis_max, x_axis_max), else 2% of the largest value read anywhere on
#' the figure.
#'
#' @param f the figure
#' @param family the figures sharing its axes (paperReviewFigureFamily())
#' @param axis "y" or "x"
#' @returns a list: \code{value} (in the figure's units) and \code{basis}
#' @keywords internal
paperReviewReadingPrecision <- function(f, family = list(f), axis = c("y", "x"))
{
  axis <- match.arg(axis)
  if (axis == "y") {
    stated <- paperReviewNum(f$reading_precision)
    if (is.finite(stated) && stated > 0) return(list(value = stated, basis = "Claude's estimate"))
    note <- f$digitization_note %or% ""
    m <- regmatches(note, regexpr("(\u00b1|\\+/-)\\s*[0-9.]+", note))
    if (length(m)) {
      v <- suppressWarnings(as.numeric(sub("^(\u00b1|\\+/-)\\s*", "", m)))
      if (is.finite(v) && v > 0) return(list(value = v, basis = "from Claude's digitisation note"))
    }
  }
  printed <- paperReviewNum(if (axis == "y") f$y_axis_max else f$x_axis_max)
  if (is.finite(printed) && printed > 0)
    return(list(value = 0.02 * printed, basis = "2% of the axis"))
  if (axis == "y") {
    # The axis Claude names in its note ("read by eye from a linear axis
    # (0-50 min)"), unless it is the duration's.
    tops <- unlist(lapply(family, function(g) {
      note <- g$digitization_note %or% ""
      m <- regmatches(note, regexpr("(?<![xX][ -])axis\\s*\\(\\s*0\\s*(-|–|to)\\s*[0-9.]+", note, perl = TRUE))
      if (length(m)) suppressWarnings(as.numeric(sub("^.*(-|–|to)\\s*", "", m))) else NA_real_
    }))
    tops <- tops[is.finite(tops) & tops > 0]
    if (length(tops))
      return(list(value = 0.02 * max(tops),
                  basis = paste0("2% of the axis, 0 to ", prFmt(max(tops)), " in Claude's digitisation note")))
  }
  values <- unlist(lapply(family, function(g) lapply(g$series %or% list(), function(s)
    vapply(s$points %or% list(), function(p) paperReviewNum(if (axis == "y") p$value else p$time), numeric(1)))))
  top <- suppressWarnings(max(values[is.finite(values)]))
  list(value = if (is.finite(top)) 0.02 * top else 0,
       basis = "2% of the largest value read on the figure")
}

#' Time for the effect-site concentration to fall after TCI holds it constant
#'
#' Simulated with stanpumpR's own effect-site TCI (an "Effect site target"
#' row, R/tci.R) for `duration`, then a target of zero; the fall is read on
#' the simulated effect-site curve.
#'
#' @returns minutes, or NA when the drug has no effect site
#' @keywords internal
paperReviewEffectSiteDecrementTime <- function(drug, reference, fall, duration)
{
  if (!isTRUE(drug$model$ke0 > 0) && !isTRUE(drug$model$tPeak > 0)) return(NA_real_)
  doses <- data.frame(Time = c(0, duration), Dose = c(1, 0),
                      Units = c(TCI_UNIT_EFFECT, TCI_UNIT_EFFECT), stringsAsFactors = FALSE)
  sim <- tryCatch(paperReviewSimulate(drug, reference, doses, duration * 4 + 600),
                  error = function(e) NULL)
  if (is.null(sim)) return(NA_real_)
  after <- sim$curve[sim$curve$Time >= duration, ]
  start <- after$Ce[1]
  below <- which(after$Ce <= start * (1 - fall))
  if (!length(below)) return(NA_real_)
  j <- below[1]
  if (j == 1) return(0)
  stats::approx(after$Ce[(j - 1):j], after$Time[(j - 1):j], start * (1 - fall))$y - duration
}

# The amount of a regimen's single intravenous dose, in the model's amount
# units (so that coef * amount is a concentration in its units), with a note;
# NA when there is none.  A single infusion shorter than a tenth of the
# fastest half-life counts as a bolus, as a paper that fits a sum of
# exponentials to it does (the first real paper: 3 min against 33 min).
paperReviewBolusAmount <- function(extraction, regimenId, concUnits, reference,
                                   fastestHalfLife = Inf)
{
  none <- list(amount = NA_real_, note = "")
  regimen <- paperReviewRegimen(extraction, regimenId)
  if (is.null(regimen) || length(regimen$doses) != 1) return(none)
  d <- regimen$doses[[1]]
  mg <- c(g = 1000, mg = 1, mcg = 1e-3, ng = 1e-6)[d$mass_unit %or% "other"]
  if (is.na(mg)) return(none)
  amountMg <- d$amount * mg * if (identical(d$per_kg, "yes")) reference$weight else 1
  note <- ""
  if (identical(d$route, "iv_infusion")) {
    minutes <- paperReviewMinutes(d$time_units %or% "min")
    duration <- paperReviewNum(d$duration) * minutes
    if (!is.finite(duration) || duration > 0.1 * fastestHalfLife) return(none)
    perTime <- d$per_time %or% "none"
    if (perTime == "per_min") amountMg <- amountMg * duration
    if (perTime == "per_h") amountMg <- amountMg * duration / 60
    note <- paste0("The ", prFmt(duration, 2), "-min infusion is treated as a bolus, as a ",
                   "sum-of-exponentials fit to it does.")
  } else if (!identical(d$route, "iv_bolus")) {
    return(none)
  }
  # mg/L is mcg/mL; express in concUnits/mL.
  list(amount = amountMg * c(pg = 1e6, ng = 1e3, mcg = 1)[[concUnits]], note = note)
}

#' Can the model give this regimen?
#'
#' An intravenous regimen always; an extravascular one only through the route
#' the model has an absorption constant for.
#'
#' @returns a list: \code{ok}, \code{why}
#' @keywords internal
paperReviewRegimenRoutable <- function(regimen, drug)
{
  routes <- unique(vapply(regimen$doses %or% list(), function(d) d$route %or% "other", character(1)))
  extravascular <- setdiff(routes, c("iv_bolus", "iv_infusion"))
  if (length(extravascular) == 0) return(list(ok = TRUE, why = ""))
  if (all(extravascular %in% names(PAPER_REVIEW_ROUTE_SUFFIX)) && !paperReviewNeedsAbsorption(regimen))
    return(list(ok = TRUE, why = ""))
  if (!all(extravascular %in% names(PAPER_REVIEW_ROUTE_SUFFIX)))
    return(list(ok = FALSE, why = paste0("The ", paste(gsub("_", " ", extravascular), collapse = ", "),
                                         " route is not one the simulator offers.")))
  a <- drug$absorptions[[regimen$id %or% ""]] %or% drug$absorptions[["route"]]
  if (!is.null(a) && isTRUE(a$ok)) return(list(ok = TRUE, why = ""))
  missing <- c(if (is.null(a) || !is.finite(a$ka)) "an absorption constant",
               if (is.null(a) || !is.finite(a$F)) "a bioavailability")
  list(ok = FALSE, why = paste0("The model has no ", gsub("_", " ", extravascular[1]),
                                " route for this regimen: nothing in the paper gives ",
                                paste(missing, collapse = " or "), "."))
}

# The mean residence time of a regimen: the disposition's Vss/CL, plus the
# mean absorption time 1/ka and the lag for an extravascular dose.
paperReviewRegimenMRT <- function(regimen, drug, mrtIV)
{
  routes <- vapply(regimen$doses %or% list(), function(d) d$route %or% "other", character(1))
  if (all(routes %in% c("iv_bolus", "iv_infusion"))) return(mrtIV)
  if (!paperReviewNeedsAbsorption(regimen)) {
    # A constant extravascular input enters the apparent disposition
    # directly; the printed MRT includes its mean input time, half its
    # duration.  Without one input of known duration it is not known.
    doses <- regimen$doses %or% list()
    if (length(doses) != 1) return(NA_real_)
    d <- doses[[1]]
    duration <- paperReviewNum(d$duration) * paperReviewMinutes(d$time_units %or% "min")
    return(if (is.finite(duration) && duration > 0) mrtIV + duration / 2 else NA_real_)
  }
  # A drug with no absorption has no route to add one for.
  if (is.na(drug$route %or% NA)) return(NA_real_)
  field <- if (drug$route == "sc") "SC" else PAPER_REVIEW_ROUTE_SUFFIX[[drug$route]]
  set <- drug$model$PK$default
  mrtIV + 1 / set[[paste0("ka_", field)]] + (set[[paste0("tlag_", field)]] %or% 0)
}

paperReviewRegimen <- function(extraction, id)
{
  if (is.null(id)) return(NULL)
  for (r in extraction$regimens %or% list()) if (identical(r$id, id)) return(r)
  NULL
}

#' AUC(0-inf), Cmax and tmax of a regimen for the reference patient
#' @keywords internal
paperReviewRegimenExposure <- function(extraction, drug, reference, regimenId)
{
  regimen <- paperReviewRegimen(extraction, regimenId)
  if (is.null(regimen))
    return(list(AUC_inf = NA_real_, Cmax = NA_real_, tmax = NA_real_,
                note = "The regimen is not identified."))
  d <- paperReviewRegimenDoses(regimen)
  if (nrow(d$doses) == 0)
    return(list(AUC_inf = NA_real_, Cmax = NA_real_, tmax = NA_real_, note = "The regimen has no usable doses."))
  last <- max(d$doses$Time)
  pk <- drug$model$PK$default
  lambda <- cube(pk$cl1 / pk$v1, pk$cl2 / pk$v1, pk$cl3 / pk$v1,
                 if (pk$v2 > 0) pk$cl2 / pk$v2 else 0,
                 if (pk$v3 > 0) pk$cl3 / pk$v3 else 0)
  terminal <- log(2) / min(lambda[lambda > 0])
  maximum <- last + min(10 * terminal, 60 * 24 * 28)
  sim <- tryCatch(paperReviewSimulate(drug, reference, d$doses, maximum), error = function(e) NULL)
  if (is.null(sim)) return(list(AUC_inf = NA_real_, Cmax = NA_real_, tmax = NA_real_,
                                note = "The regimen could not be simulated."))
  curve <- sim$curve
  i <- which.max(curve$Cp)
  # AUC(0-inf) is exact for a linear model: the bioavailable amount over CL.
  amount <- paperReviewRegimenAmount(d$doses, drug, reference)
  auc <- amount / pk$cl1 * c(pg = 1e6, ng = 1e3, mcg = 1)[[drug$defaults$Concentration.Units]]
  list(AUC_inf = auc, Cmax = curve$Cp[i], tmax = curve$Time[i] - min(d$doses$Time),
       note = "")
}

# The total bioavailable amount of a dose table, mg.
paperReviewRegimenAmount <- function(doses, drug, reference)
{
  mg <- function(u) c(g = 1000, mg = 1, mcg = 1e-3, ng = 1e-6)[[sub("[/ ].*$", "", u)]]
  pk <- drug$model$PK$default
  total <- 0
  for (r in seq_len(nrow(doses))) {
    u <- doses$Units[r]
    a <- doses$Dose[r] * mg(u) * if (grepl("/kg", u)) reference$weight else 1
    if (grepl("/min|/hr|/day", u)) {
      nextTime <- doses$Time[doses$Units == u & doses$Time > doses$Time[r]]
      if (length(nextTime) == 0) next
      a <- a * (min(nextTime) - doses$Time[r]) /
        (if (grepl("/hr", u)) 60 else if (grepl("/day", u)) 1440 else 1)
    } else if (grepl(" (PO|IM|IN|SL|SC)$", u)) {
      field <- sub("^.* ", "", u)
      F <- pk[[paste0("bioavailability_", field)]]
      a <- a * if (is.null(F)) 1 else F
    }
    total <- total + a
  }
  total
}

# --- Covariates, assumptions and the library's rules -------------------------

#' Check the covariate equations
#'
#' Each effect is evaluated over every legal patient in PAPER_REVIEW_SWEEP:
#' a multiplier that is not finite and positive is an error ("no infinity,
#' no division by zero", AGENTS.md).  A negative power of age is infinite at
#' age 0 (the age-floor rule).  The reference value is checked against the
#' population's range.
#'
#' @returns a list of findings
#' @keywords internal
paperReviewCovariateChecks <- function(extraction)
{
  section <- "Covariates"
  findings <- list()
  add <- function(...) findings[[length(findings) + 1]] <<- paperReviewFinding(section, ...)
  reference <- paperReviewReferencePatient(extraction)
  pop <- extraction$population %or% list()
  grid <- expand.grid(PAPER_REVIEW_SWEEP, stringsAsFactors = FALSE)

  # The reference patient matters only to a covariate the review evaluates.
  evaluated <- unique(vapply(extraction$covariate_effects %or% list(), function(e) e$covariate %or% "", ""))
  needs <- c(weight = any(evaluated %in% c("weight", "bmi", "lean_body_mass", "fat_free_mass")),
             height = any(evaluated %in% c("height", "bmi", "lean_body_mass", "fat_free_mass")),
             age = any(evaluated %in% c("age", "fat_free_mass")))
  perKgPrinted <- any(vapply(extraction$parameters %or% list(), function(p)
    isTRUE(paperReviewUnit(p$units %or% "other")$perKg), logical(1)))
  needs[["weight"]] <- needs[["weight"]] || perKgPrinted
  for (field in c("weight", "height", "age"))
    if (needs[[field]] && grepl("default", reference$source[[field]]))
      add("info", paste0("No reference or typical ", field, " is printed"),
          paste0("Typical values are evaluated at the stanpumpR default ", field, " (",
                 reference[[field]], ")."), pop$location)

  for (e in extraction$covariate_effects %or% list())
  {
    label <- paste0(e$parameter_role, " ~ ", e$covariate, " (", e$form, ")")
    loc <- e$location
    if (identical(e$form, "other")) {
      add("concern", paste("Covariate equation not in a form the review can evaluate:", label),
          paste0("As printed: ", e$equation_as_printed, ". It is held at its reference in the simulator."), loc)
      next
    }
    if (is.na(paperReviewNum(e$coefficient))) {
      add("concern", paste("No coefficient printed for", label), e$equation_as_printed %or% "", loc)
      next
    }
    if (is.na(paperReviewCovariateValue(e$covariate, reference))) {
      add("info", paste("Not applied:", label),
          paste0("The simulator does not take ", e$covariate,
                 "; the parameter is held at its typical value. As printed: ",
                 e$equation_as_printed), loc)
      next
    }
    if (is.na(paperReviewNum(e$reference_value)) && e$form != "category_multiplier")
      add("concern", paste("No reference value printed for", label),
          paste0("Evaluated at the population's central value (",
                 prFmt(paperReviewCovariateValue(e$covariate, reference)), ")."), loc)
    m <- vapply(seq_len(nrow(grid)), function(r)
      suppressWarnings(paperReviewCovariateMultiplier(e, as.list(grid[r, ]), reference)$multiplier),
      numeric(1))
    bad <- !is.finite(m) | m <= 0
    if (any(bad)) {
      g <- grid[which(bad)[1], ]
      add("error", paste("The equation is not finite and positive for every legal patient:", label),
          paste0("It gives ", prFmt(m[which(bad)[1]]), " at age ", g$age, " y, ", g$weight,
                 " kg, ", g$height, " cm, ", g$sex,
                 ". A model must stay finite for ages 0 to 90 and the full weight and height ranges."), loc)
    } else {
      add("ok", paste("Finite and positive for every legal patient:", label),
          paste0("Multiplier ranges from ", prFmt(min(m)), " to ", prFmt(max(m)),
                 " across ages 0-90 y, 3-200 kg, 50-200 cm, both sexes."), loc)
    }
    if (identical(e$covariate, "age") && identical(e$form, "power") && isTRUE(e$coefficient < 0))
      add("concern", paste("A negative power of age is infinite at age 0:", label),
          "stanpumpR's rule: an age term floors at 18 years unless the source fitted it in younger patients, and then at the youngest age studied.",
          loc)
    summary <- switch(e$covariate %or% "other", weight = pop$weight_kg, height = pop$height_cm,
                      age = pop$age_years, NULL)
    ref <- paperReviewNum(e$reference_value)
    if (!is.null(summary) && !is.na(ref)) {
      lo <- paperReviewNum(summary$min); hi <- paperReviewNum(summary$max)
      if ((!is.na(lo) && ref < lo) || (!is.na(hi) && ref > hi))
        add("concern", paste("The reference value lies outside the studied range:", label),
            paste0("Reference ", ref, "; studied ", prFmt(lo), " to ", prFmt(hi), "."),
            c(loc, pop$location))
    }
  }
  for (field in c("age_years", "weight_kg"))
  {
    s <- pop[[field]]
    lo <- paperReviewNum(s$min); hi <- paperReviewNum(s$max)
    if (!is.na(lo) || !is.na(hi))
      add("info", paste0("Studied ", sub("_.*", "", field), ": ", prFmt(lo), " to ", prFmt(hi), " ",
                         sub("^.*_", "", field)),
          "The simulator accepts patients outside this range; its predictions there are extrapolations.",
          pop$location, computed = FALSE)
  }
  findings
}

#' Check the assumptions the paper makes and apply the library's rules
#'
#' @returns a list of findings
#' @keywords internal
paperReviewAssumptionChecks <- function(extraction)
{
  section <- "Assumptions and the library's rules"
  findings <- list()
  add <- function(...) findings[[length(findings) + 1]] <<- paperReviewFinding(section, ...)
  drug <- extraction$drug %or% list()
  s <- extraction$structure %or% list()

  basis <- drug$dose_basis %or% "unclear"
  if (basis == "unclear")
    add("concern", "The dose basis is not stated",
        paste0("Whether doses are the salt or the base changes every volume and clearance by the ",
               "ratio of their molecular weights. ", drug$dose_basis_note %or% ""), drug$location,
        computed = FALSE)
  else if (basis %in% c("salt", "prodrug_equivalent"))
    add("info", paste("Doses are the", gsub("_", " ", basis)),
        paste("Enter doses on the same basis in the simulator.", drug$dose_basis_note %or% ""), drug$location,
        computed = FALSE)

  b <- extraction$bioavailability %or% list()
  if (identical(b$source, "fitted_to_other_study_exposure"))
    add("error", "Bioavailability was fitted to another study's exposure",
        paste("Bioavailability is read from a within-subject crossover, never fitted so that one study's",
              "clearance reproduces another study's concentrations: that absorbs the difference between",
              "the two populations into F.", b$note %or% ""), b$location, computed = FALSE)
  else if (identical(b$source, "fixed_from_literature"))
    add("info", "Bioavailability is fixed from the literature",
        paste("Check that the cited value is a crossover F and that its population matches.", b$note %or% ""),
        b$location, computed = FALSE)
  if (identical(s$apparent_oral, "yes"))
    add("info", "Volumes and clearances are apparent (CL/F, V/F)",
        "They cannot be compared with intravenous values without F.", s$location, computed = FALSE)

  if (!identical(s$elimination %or% "linear", "linear"))
    add("concern", "Elimination is not linear",
        "The in-memory simulator is linear; it uses the printed clearance, which is only the low-concentration limit of saturable elimination.",
        s$location)
  absorption <- s$absorption %or% "none"
  discrete <- any(vapply(extraction$regimens %or% list(), paperReviewNeedsAbsorption, logical(1)))
  if (absorption == "zero_order" && !discrete)
    add("info", "Absorption is zero order",
        "Every extravascular dose is a constant input, which the simulator gives exactly as the paper models it.",
        s$location, computed = FALSE)
  else if (absorption %in% c("zero_order", "transit", "other"))
    add("concern", paste("Absorption is", gsub("_", " ", absorption)),
        "The in-memory simulator absorbs a discrete dose first order, so its early curve will differ from the paper's.",
        s$location)

  # 3. A parameter printed twice with different values, within one model
  # (two models' values of one parameter are not a conflict: the Duzitol
  # editorial's Table 1 has three columns).
  groupOf <- function(p) p$group %or% ""
  for (role in setdiff(PAPER_REVIEW_ROLES, "other"))
  for (g in unique(vapply(extraction$parameters %or% list(), groupOf, ""))) {
    found <- Filter(function(p) identical(p$role, role) && is.finite(paperReviewNum(p$value)) &&
                      is.null(p$regimen_id) && identical(groupOf(p), g), extraction$parameters %or% list())
    if (length(found) < 2) next
    values <- vapply(found, function(p) paperReviewNum(p$value) * paperReviewUnit(p$units %or% "other")$factor,
                     numeric(1))
    if (!all(is.finite(values)) || diff(range(values)) <= 1e-9 * max(abs(values))) next
    add("concern", paste(role, "is printed with different values"),
        paste0(paste(vapply(found, function(p) paste0(p$value_text %or% "", " (", p$location %or% "", ")"), ""),
                     collapse = "; "),
               ". The review uses the first, ", found[[1]]$value_text %or% "", "."),
        vapply(found, function(p) p$location %or% NA_character_, ""))
  }

  for (c in extraction$interpretive_claims %or% list())
  {
    if (identical(c$category, "volume_as_anatomy"))
      add("concern", "A volume is read as an anatomical space",
          paste0("'", c$claim, "'. Only clearance measures a physiological process; volumes and ",
                 "intercompartmental clearances are constructs that relate dose to concentration, ",
                 "and they carry the artefacts of sampling site, first sample time and assay limit."),
          c$location, computed = FALSE)
    else if (identical(c$category, "trend_from_difference"))
      add("info", "A trend is claimed from a difference of means",
          paste0("'", c$claim, "'. See the differences below: a difference of two measured numbers ",
                 "carries both their variances."), c$location, computed = FALSE)
  }

  for (d in extraction$reported_differences %or% list())
    findings[[length(findings) + 1]] <- paperReviewDifferenceFinding(d, section)
  findings
}

#' Test a reported difference of two means
#'
#' The SE of a difference of independent means is sqrt(se1^2 + se2^2).  When
#' the paper prints the SE of the difference too, comparing the two reveals
#' pairing: rho = (se1^2 + se2^2 - sed^2) / (2 se1 se2).
#'
#' @keywords internal
paperReviewDifferenceFinding <- function(d, section)
{
  v1 <- paperReviewNum(d$group1_value); v2 <- paperReviewNum(d$group2_value)
  s1 <- paperReviewNum(d$group1_se); s2 <- paperReviewNum(d$group2_se)
  diff <- paperReviewNum(d$difference); if (is.na(diff)) diff <- v2 - v1
  sed <- paperReviewNum(d$difference_se)
  independent <- sqrt(s1^2 + s2^2)
  se <- if (!is.na(sed)) sed else independent
  t <- diff / se
  rho <- if (!is.na(sed) && is.finite(independent)) (s1^2 + s2^2 - sed^2) / (2 * s1 * s2) else NA_real_
  detail <- paste0(
    "Difference ", prFmt(diff), "; SE if the groups are independent ", prFmt(independent),
    if (!is.na(sed)) paste0("; SE printed ", prFmt(sed),
                            if (is.finite(rho)) paste0(", implying a correlation of ", prFmt(rho, 2),
                                                       if (rho > 0.3) " (paired)" else "") else "") else "",
    ". t = ", prFmt(t, 2), ".")
  severity <- if (!is.finite(t)) "info" else if (abs(t) < 2) "concern" else "ok"
  title <- if (!is.finite(t)) paste("Difference not testable:", d$description)
    else if (abs(t) < 2) paste("The difference is within its own scatter:", d$description)
    else paste("The difference exceeds its scatter:", d$description)
  paperReviewFinding(section, severity, title, detail, d$location)
}

# --- Figures ---------------------------------------------------------------

# A figure's id with the series it compares: one figure of three models'
# curves is checked once per model, and "Figure 1: reproduces" beside
# "Figure 1: does not reproduce" read as a contradiction (Shafer, 2026-11-01).
paperReviewFigureTitle <- function(id, series)
{
  labels <- unique(series[nzchar(series)])
  if (length(labels) == 0) id else paste0(id, ", ", paste(labels, collapse = "; "))
}

#' Check a figure normalized to its own peak
#'
#' Compartmental models are linear in dose, so a curve divided by its own
#' maximum does not depend on the dose, and a paper can plot one without
#' printing any doses.  The model's curve is divided by its maximum the same
#' way: the figure's regimen when it names one that can be simulated,
#' otherwise a unit intravenous bolus, sum A(i) exp(-lambda(i) t).  Dividing
#' by the maximum rather than by the sum of the coefficients matters at the
#' effect site, whose peak after a bolus is later and far lower than the
#' plasma's (Shafer, 2026-11-01).
#'
#' @returns a list: \code{finding}, and \code{figure} for the report's plot
#' @keywords internal
paperReviewNormalizedFigure <- function(f, extraction, drug, reference)
{
  id <- f$figure_id %or% "Figure"
  scale <- if (identical(f$y_normalization, "fraction_of_peak")) 1 else 100
  site <- f$concentration_site %or% "plasma"
  minutes <- paperReviewMinutes(f$time_units %or% "min")
  series <- Filter(function(s) (s$analyte %or% "parent") %in% "parent", f$series %or% list())
  points <- do.call(rbind, lapply(series, function(s) {
    p <- s$points %or% list()
    if (length(p) == 0) return(NULL)
    data.frame(series = s$label %or% "",
               regimen = s$regimen_id %or% f$regimen_id %or% NA_character_,
               time = vapply(p, function(x) paperReviewNum(x$time), numeric(1)) * minutes,
               observed = vapply(p, function(x) paperReviewNum(x$value), numeric(1)) * 100 / scale,
               lower = NA_real_, upper = NA_real_, stringsAsFactors = FALSE)
  }))
  if (is.na(minutes) || is.null(points) || nrow(points) == 0)
    return(list(finding = paperReviewFinding("Figures", "info", paste(id, "has no digitised points"),
                                             f$digitization_note %or% "", f$location)))
  pk <- tryCatch(paperReviewQuietly(function()
    getDrugPK(drug$defaults$Drug, reference$weight, reference$height, reference$age,
              reference$sex, drug$defaults, model = drug$model)$PK$default), error = function(e) NULL)
  if (is.null(pk))
    return(list(finding = paperReviewFinding("Figures", "info", paste(id, "not compared"),
      "stanpumpR could not build the model.", f$location)))
  ke0 <- if (isTRUE(pk$ke0 > 0)) pk$ke0 else NA_real_
  if (identical(site, "effect_site") && !is.finite(ke0))
    return(list(finding = paperReviewFinding("Figures", "info", paste(id, "not compared"),
      "It plots the effect site, and the model has none.", f$location)))
  maximum <- max(points$time, na.rm = TRUE) * 1.05 + 1
  grid <- c(0, exp(seq(log(maximum / 2000), log(maximum), length.out = 400)))
  curves <- list()
  points$predicted <- NA_real_
  source <- character(0)
  for (rid in unique(points$regimen)) {
    these <- which(points$regimen %in% rid)
    regimen <- if (is.na(rid)) NULL else paperReviewRegimen(extraction, rid)
    doses <- if (is.null(regimen)) NULL else paperReviewRegimenDoses(regimen)$doses
    sim <- NULL
    if (!is.null(doses) && nrow(doses) > 0 && any(doses$Dose > 0) &&
        isTRUE(paperReviewRegimenRoutable(regimen, drug)$ok))
      sim <- tryCatch(paperReviewSimulate(paperReviewDrugFor(drug, extraction, regimen, reference),
                                          reference, doses, maximum), error = function(e) NULL)
    if (!is.null(sim)) {
      y <- if (identical(site, "effect_site")) sim$curve$Ce else sim$curve$Cp
      time <- sim$curve$Time
      source <- c(source, paste0("the regimen ", rid))
    } else {
      # A unit intravenous bolus at time 0, at the plasma or the effect site.
      l <- c(pk$lambda_1, pk$lambda_2, pk$lambda_3)
      A <- c(pk$p_coef_bolus_l1, pk$p_coef_bolus_l2, pk$p_coef_bolus_l3)[l > 0]
      l <- l[l > 0]
      time <- grid
      y <- if (identical(site, "effect_site"))
        vapply(time, function(t) sum(A * ke0 / (ke0 - l) * (exp(-l * t) - exp(-ke0 * t))), 0)
      else vapply(time, function(t) sum(A * exp(-l * t)), 0)
      source <- c(source, "a unit intravenous bolus")
    }
    y <- 100 * y / max(y, na.rm = TRUE)
    points$predicted[these] <- stats::approx(time, y, points$time[these], rule = 2)$y
    curves[[length(curves) + 1]] <- data.frame(Time = time, Cp = y,
                                               regimen = if (is.na(rid)) "bolus" else rid,
                                               stringsAsFactors = FALSE)
  }
  use <- is.finite(points$observed) & points$observed > 0 & is.finite(points$predicted) &
    points$predicted > 0
  pe <- (points$observed[use] - points$predicted[use]) / points$predicted[use]
  mdpe <- stats::median(pe); mdape <- stats::median(abs(pe))
  # On a linear axis a reading by eye is off by about the same number of
  # percentage points everywhere, as for the decrement curves.
  linear <- !identical(f$log_y, "yes")
  precision <- paperReviewReadingPrecision(f, paperReviewFigureFamily(f, extraction), "y")
  tol <- precision$value * 100 / scale
  mdad <- stats::median(abs(points$observed[use] - points$predicted[use]))
  severity <- if (!is.finite(mdape)) "info"
    else if (mdape <= 0.2 || (linear && mdad <= tol)) "ok"
    else if (mdape <= 0.5 || (linear && mdad <= 2 * tol)) "concern"
    else "error"
  what <- if (identical(site, "effect_site")) "effect-site" else "plasma"
  finding <- paperReviewFinding("Figures", severity,
    paste0(paperReviewFigureTitle(id, points$series), ": the model ",
           if (severity == "ok") "reproduces" else "does not closely reproduce",
           " the curve normalized to its peak"),
    paste0(sum(use), " points compared. The figure plots the ", what, " concentration as a ",
           if (scale == 100) "percent" else "fraction", " of its own peak, which does not depend on ",
           "the dose; the model's curve (", paste(unique(source), collapse = ", "),
           ") is divided by its own maximum the same way. Median prediction error ",
           sprintf("%+.0f%%", 100 * mdpe), ", median absolute ", sprintf("%.0f%%", 100 * mdape),
           if (linear) paste0("; median difference ", prFmt(mdad, 2), " percentage points against a ",
                              "reading precision of +/-", prFmt(tol, 2), " (", precision$basis, ")") else "",
           ". The points were digitised approximately by Claude (", f$digitization_note %or% "", ")."),
    f$location)
  list(finding = finding,
       figure = list(id = id, caption = f$caption %or% "", logY = identical(f$log_y, "yes"),
                     kind = "normalized", yLabel = paste0("Percent of peak, ", what),
                     curve = do.call(rbind, curves), points = points, mdpe = mdpe, mdape = mdape))
}

#' Simulate each figure's regimen and compare with its digitised points
#'
#' @param extraction the extraction
#' @param drug paperReviewDrug() at the reference patient
#' @param reference the reference patient
#' @returns a list: \code{figures} (each with its curve and points) and
#'   \code{findings}
#' @keywords internal
paperReviewFigureChecks <- function(extraction, drug, reference)
{
  section <- "Figures"
  findings <- list()
  figures <- list()
  add <- function(...) findings[[length(findings) + 1]] <<- paperReviewFinding(section, ...)
  concUnits <- drug$defaults$Concentration.Units
  for (f in extraction$figures %or% list())
  {
    id <- f$figure_id %or% "Figure"
    if (identical(f$y_quantity, "decrement_time")) {
      checked <- paperReviewDecrementFigure(f, drug, reference, paperReviewFigureFamily(f, extraction))
      findings[[length(findings) + 1]] <- checked$finding
      if (!is.null(checked$figure)) figures[[length(figures) + 1]] <- checked$figure
      next
    }
    if (!identical(f$y_quantity, "concentration")) {
      add("info", paste(id, "is not a concentration figure"), "Not compared.", f$location)
      next
    }
    if (!drug$ok) { add("info", paste(id, "not compared"), "There is no complete model.", f$location); next }
    if ((f$y_normalization %or% "none") %in% c("percent_of_peak", "fraction_of_peak")) {
      checked <- paperReviewNormalizedFigure(f, extraction, drug, reference)
      findings[[length(findings) + 1]] <- checked$finding
      if (!is.null(checked$figure)) figures[[length(figures) + 1]] <- checked$figure
      next
    }
    # Points from individually dosed subjects cannot be one simulation (a
    # test paper's cloud of study observations was scored against a typical
    # regimen, +40%).
    if (identical(f$one_regimen, "no")) {
      add("info", paste(id, "not compared"),
          "Its points come from subjects dosed individually, so no single regimen reproduces them.",
          f$location)
      next
    }
    factor <- paperReviewConcentrationFactor(f$y_units %or% "other", concUnits, extraction$drug$molecular_weight)
    minutes <- paperReviewMinutes(f$time_units %or% "min")
    if (!is.finite(factor) || is.na(minutes)) {
      add("concern", paste(id, "could not be compared"), "Its units could not be converted.", f$location)
      next
    }
    # Each series may show its own regimen (a figure of an intravenous and two
    # oral curves); a series without one takes the figure's.  The first real
    # paper's figure had three regimens and only one slot for them.
    # A metabolite's series is set aside: the in-memory model is the parent's
    # (the second test paper's figures paired each parent panel with one of
    # its metabolite, which were scored against the parent's curve).
    otherAnalyte <- Filter(function(s) !(s$analyte %or% "parent") %in% "parent", f$series %or% list())
    analyteNote <- if (length(otherAnalyte))
      paste0(" Not compared: ", paste(vapply(otherAnalyte, function(s) s$label %or% "a series", ""),
                                      collapse = ", "), ", which plots another analyte than the one modelled.")
      else ""
    points <- do.call(rbind, lapply(f$series %or% list(), function(s) {
      p <- s$points %or% list()
      if (length(p) == 0) return(NULL)
      if (!(s$analyte %or% "parent") %in% "parent") return(NULL)
      data.frame(series = s$label %or% "",
                 regimen = s$regimen_id %or% f$regimen_id %or% NA_character_,
                 time = vapply(p, function(x) paperReviewNum(x$time), numeric(1)) * minutes,
                 observed = vapply(p, function(x) paperReviewNum(x$value), numeric(1)) * factor,
                 lower = vapply(p, function(x) paperReviewNum(x$error_lower), numeric(1)) * factor,
                 upper = vapply(p, function(x) paperReviewNum(x$error_upper), numeric(1)) * factor,
                 stringsAsFactors = FALSE)
    }))
    if (is.null(points) || nrow(points) == 0) {
      add("info", paste(id, if (nzchar(analyteNote)) "plots no series of the modelled drug"
                        else "has no digitised points"),
          trimws(paste(analyteNote, f$digitization_note %or% "")), f$location)
      next
    }
    points$predicted <- NA_real_
    curves <- list()
    setAside <- character(0)
    regimenLocations <- character(0)
    for (rid in unique(points$regimen)) {
      these <- which(points$regimen %in% rid)
      labels <- paste(unique(points$series[these]), collapse = ", ")
      regimen <- paperReviewRegimen(extraction, if (is.na(rid)) NULL else rid)
      if (is.null(regimen)) {
        setAside <- c(setAside, paste0(labels, ": names no regimen"))
        next
      }
      regimenLocations <- c(regimenLocations, regimen$location)
      routable <- paperReviewRegimenRoutable(regimen, drug)
      if (!routable$ok) {
        setAside <- c(setAside, paste0(labels, " (", rid, "): ", routable$why))
        next
      }
      a <- drug$absorptions[[rid]]
      if (!is.null(a) && id %in% a$fittedFigures) {
        setAside <- c(setAside, paste0(labels, " (", rid, "): its absorption was fitted to these ",
                                       "points, so they cannot also check it"))
        next
      }
      doses <- paperReviewRegimenDoses(regimen)
      if (nrow(doses$doses) == 0) {
        setAside <- c(setAside, paste0(labels, " (", rid, "): its doses could not be converted"))
        next
      }
      maximum <- max(c(points$time[these], doses$doses$Time), na.rm = TRUE) * 1.05 + 1
      sim <- tryCatch(paperReviewSimulate(paperReviewDrugFor(drug, extraction, regimen, reference),
                                          reference, doses$doses, maximum),
                      error = function(e) e)
      if (inherits(sim, "error")) {
        setAside <- c(setAside, paste0(labels, " (", rid, "): could not be simulated: ",
                                       conditionMessage(sim)))
        next
      }
      points$predicted[these] <- stats::approx(sim$curve$Time, sim$curve$Cp, points$time[these],
                                               rule = 2)$y
      curves[[length(curves) + 1]] <- data.frame(Time = sim$curve$Time, Cp = sim$curve$Cp,
                                                 regimen = rid, stringsAsFactors = FALSE)
    }
    note <- paste0(if (length(setAside))
      paste0(" Not compared: ", paste(setAside, collapse = "; "), ".") else "", analyteNote)
    if (length(curves) == 0) {
      add("concern", paste(id, "could not be compared"), trimws(note), f$location)
      next
    }
    use <- is.finite(points$observed) & points$observed > 0 & is.finite(points$predicted) &
      points$predicted > 0
    pe <- (points$observed[use] - points$predicted[use]) / points$predicted[use]
    mdpe <- stats::median(pe); mdape <- stats::median(abs(pe))
    severity <- if (!is.finite(mdape)) "info" else if (mdape <= 0.2) "ok"
                else if (mdape <= 0.5) "concern" else "error"
    add(severity, paste0(paperReviewFigureTitle(id, points$series), ": the model ",
                         if (severity == "ok") "reproduces" else "does not closely reproduce",
                         " the digitised points"),
        paste0(sum(use), " points compared. Median prediction error (observed - predicted) / predicted ",
               sprintf("%+.0f%%", 100 * mdpe), ", median absolute ", sprintf("%.0f%%", 100 * mdape),
               ".", note, " The points were digitised approximately by Claude (",
               f$digitization_note %or% "",
               "); errors under about 10-20% are within that approximation.",
               if (f$data_kind %in% c("mean", "median"))
                 " The figure shows a mean or median of individuals; the model's curve is the typical subject's, which need not equal it."
               else ""),
        c(f$location, regimenLocations))
    figures[[length(figures) + 1]] <- list(
      id = id, caption = f$caption %or% "", logY = identical(f$log_y, "yes"),
      curve = do.call(rbind, curves), points = points, mdpe = mdpe, mdape = mdape)
  }
  if (length(extraction$figures %or% list()) == 0)
    add("info", "No concentration figures were extracted")
  list(figures = figures, findings = findings)
}

# --- Claude's own notes --------------------------------------------------------

paperReviewClaudeNotes <- function(extraction)
{
  lapply(extraction$reviewer_notes %or% list(), function(n)
    paperReviewFinding("Claude's reading",
                       # a severity outside the choices is read as information
                       if (isTRUE((n$severity %or% "") %in% PAPER_REVIEW_SEVERITY_ORDER)) n$severity else "info",
                       n$topic %or% "Note",
                       n$finding %or% "", n$location, computed = FALSE))
}

#' Run the whole review on an extraction
#'
#' @param extraction the extraction (a list, as parsed from the JSON)
#' @returns the review: \code{extraction}, \code{reference},
#'   \code{identifiability}, \code{drug} (at the reference patient),
#'   \code{consistency}, \code{figures}, \code{findings}
#' @keywords internal
paperReviewRun <- function(extraction)
{
  groups <- paperReviewGroups(extraction)
  if (length(groups) <= 1) return(paperReviewRunOne(extraction))
  # Several models (the Duzitol editorial: one drug in two populations beside
  # propofol, as columns of one table).  Each model is reviewed on its own
  # items, the items every model shares included; what is about the paper
  # (its assumptions, its claims, Claude's notes) is reviewed once.
  models <- lapply(stats::setNames(groups, groups), function(g)
    paperReviewRunOne(paperReviewGroupExtraction(extraction, g), paperLevel = FALSE))
  paperFindings <- c(paperReviewAssumptionChecks(extraction), paperReviewClaudeNotes(extraction))
  out <- models[[1]]
  out$extraction <- extraction
  out$models <- models
  out$paperFindings <- paperFindings
  out$findings <- c(unlist(lapply(groups, function(g)
    lapply(models[[g]]$findings, function(f) { f$group <- g; f })), recursive = FALSE),
    paperFindings)
  out
}

#' The models a paper reports, by the group names its items carry
#' @returns the names, in the order they first appear; empty for one model
#' @keywords internal
paperReviewGroups <- function(extraction)
{
  g <- c(vapply(extraction$parameters %or% list(), function(p) p$group %or% "", ""),
         vapply(extraction$derived_quantities %or% list(), function(p) p$group %or% "", ""),
         vapply(extraction$regimens %or% list(), function(p) p$group %or% "", ""),
         unlist(lapply(extraction$figures %or% list(), function(f)
           vapply(f$series %or% list(), function(s) s$group %or% "", ""))))
  unique(g[nzchar(g)])
}

#' One model's part of an extraction
#'
#' Its own items and the items every model shares (those with no group);
#' a figure keeps only that model's series.  The drug is named for the model.
#' @keywords internal
paperReviewGroupExtraction <- function(extraction, group)
{
  mine <- function(items) Filter(function(i) is.null(i$group) || identical(i$group, group), items %or% list())
  x <- extraction
  x$parameters <- mine(x$parameters)
  x$derived_quantities <- mine(x$derived_quantities)
  x$covariate_effects <- mine(x$covariate_effects)
  x$regimens <- mine(x$regimens)
  # Every model's curves of a figure share its axes, so the precision of a
  # reading is the figure's, whichever model's curve was read.
  x$paperFigures <- extraction$figures
  x$figures <- Filter(function(f) length(f$series) > 0,
                      lapply(x$figures %or% list(), function(f) { f$series <- mine(f$series); f }))
  x$drug$name <- group
  x
}

#' Review one model
#'
#' @param extraction the extraction (one model's part of it, when there are
#'   several)
#' @param paperLevel include the findings about the paper as a whole?
#' @keywords internal
paperReviewRunOne <- function(extraction, paperLevel = TRUE)
{
  reference <- paperReviewReferencePatient(extraction)
  ident <- paperReviewIdentifiability(extraction)
  # Absorption is derived per extravascular regimen against the disposition,
  # then the drug carries the first regimen that has one (the simulator can
  # switch), and every check builds the drug for the regimen it checks.
  base <- paperReviewDrug(extraction, reference, ident$closed)
  absorptions <- if (base$ok) paperReviewAbsorptions(extraction, base, reference) else list()
  usable <- Filter(function(a) isTRUE(a$ok), absorptions)
  drug <- paperReviewDrug(extraction, reference, ident$closed,
                          absorption = if (length(usable)) usable[[1]] else NULL,
                          absorptions = absorptions)
  # Each check on its own: an extraction a check cannot read costs that
  # check's section a finding, not the whole review (Devin's stress test,
  # 2026-11-08; paperReviewConform() sets aside what is unreadable, and this
  # catches what a well-formed but unusable value still breaks).
  guarded <- function(section, empty, f) tryCatch(f(), error = function(e) {
    out <- empty
    out$findings <- list(paperReviewFinding(section, "concern",
      "This check could not be computed from the extraction",
      paste0("The reply could not be used here (", conditionMessage(e), "). Check the extraction ",
             "tables below against the paper.")))
    out
  })
  consistency <- guarded("Internal consistency", list(table = NULL, pkTable = NULL),
                         function() paperReviewConsistency(extraction, drug, reference, ident))
  figures <- guarded("Figures", list(figures = list()),
                     function() paperReviewFigureChecks(extraction, drug, reference))
  findingsOf <- function(section, f) guarded(section, list(), function() list(findings = f()))$findings
  modelFindings <- lapply(drug$problems, function(p)
    paperReviewFinding("Model", "concern", p))
  findings <- c(ident$findings,
                findingsOf("Model", function() paperReviewAbsorptionFindings(extraction, absorptions)),
                findingsOf("Model", function() paperReviewExponentialFindings(extraction, reference, drug)),
                modelFindings, consistency$findings,
                findingsOf("Covariates", function() paperReviewCovariateChecks(extraction)),
                if (paperLevel) findingsOf("Assumptions and the library's rules", function() paperReviewAssumptionChecks(extraction)),
                figures$findings,
                if (paperLevel) paperReviewClaudeNotes(extraction))
  list(extraction = extraction, reference = reference, identifiability = ident,
       drug = drug, absorptions = absorptions, consistency = consistency$table,
       pkTable = consistency$pkTable,
       figures = figures$figures, findings = findings)
}

#' One finding per extravascular regimen: where its absorption came from
#' @keywords internal
paperReviewAbsorptionFindings <- function(extraction, absorptions)
{
  lapply(names(absorptions), function(id) {
    a <- absorptions[[id]]
    regimen <- paperReviewRegimen(extraction, if (id == "route") NULL else id)
    label <- if (is.null(regimen)) gsub("_", " ", a$route)
             else paste0(id, " (", regimen$description %or% gsub("_", " ", a$route), ")")
    if (!isTRUE(a$ok)) {
      missing <- c(if (!is.finite(a$ka)) "an absorption constant", if (!is.finite(a$F)) "a bioavailability")
      return(paperReviewFinding("Identifiability", "concern",
        paste("No absorption can be derived for", label),
        paste0("Nothing printed or plotted gives ", paste(missing, collapse = " or "),
               ". The disposition model is unaffected; this regimen cannot be simulated."),
        c(regimen$location, a$locations)))
    }
    src <- function(what) a$sources[[what]] %or% "printed"
    detail <- paste0(
      "ka ", prFmt(a$ka * 60), " /h (", src("ka"), "); F ", prFmt(a$F),
      " (", if (is.null(a$sources[["F"]])) "printed" else a$sources[["F"]], ")",
      if (a$tlag > 0) paste0("; lag ", prFmt(a$tlag), " min") else "",
      if (a$fraction != 1) paste0("; dose basis ", prFmt(a$fraction), ", from ", a$sources[["dose"]]) else "",
      ". First-order absorption assumed.",
      if (!is.null(a$crossCheck)) paste0(" ", a$crossCheck) else "")
    curve <- grepl("digitised", paste(unlist(a$sources), collapse = " "))
    paperReviewFinding("Identifiability", if (curve) "info" else "ok",
                       paste("Absorption for", label), detail,
                       c(regimen$location, a$locations))
  })
}

#' How many exponentials do the digitised intravenous points support?
#'
#' Sums of one, two and three exponentials are fitted (on log
#' concentrations) to each intravenous series; the lowest AIC is the number
#' the points support, set against the model's compartments.
#'
#' @keywords internal
paperReviewExponentialFindings <- function(extraction, reference, drug)
{
  n <- paperReviewNum(extraction$structure$compartments)
  concUnits <- drug$defaults$Concentration.Units
  lapply(paperReviewIntravenousSeries(extraction, reference, concUnits), function(s) {
    fits <- lapply(1:3, function(k) paperReviewFitExponentials(s$points$time, s$points$observed, k))
    ok <- !vapply(fits, is.null, logical(1))
    if (sum(ok) < 2) return(paperReviewFinding("Identifiability", "info",
      paste0(s$figure, ": too few intravenous points to compare exponentials")))
    aic <- vapply(fits, function(f) if (is.null(f)) NA_real_ else f$aic, numeric(1))
    best <- which.min(aic)
    detail <- paste0(
      nrow(s$points), " digitised points of ", s$label, ". Residual sum of squares (log scale) for ",
      paste(paste0(which(ok), " exponential", ifelse(which(ok) > 1, "s", ""), " ",
                   prFmt(vapply(fits[ok], function(f) f$rss, 0))), collapse = ", "),
      "; AIC prefers ", best, ". The model has ", n, ". Digitised points are approximate, so a ",
      "difference this test finds is a lead to look for in the paper, not a finding by itself.")
    severity <- if (!is.finite(n)) "info" else if (best > n) "concern" else if (best == n) "ok" else "info"
    paperReviewFinding("Identifiability", severity,
      paste0(s$figure, ": the digitised intravenous curve supports ", best, " exponential",
             if (best > 1) "s" else "", if (is.finite(n) && best != n) paste0(", the model ", n) else ""),
      detail, c(s$location, s$regimenLocation))
  })
}

#' Close the disposition from a digitised intravenous curve
#'
#' @returns NULL, or a list: \code{closed}, \code{route}, \code{locations}
#' @keywords internal
paperReviewCloseFromCurve <- function(extraction, reference, n)
{
  mapped <- PAPER_REVIEW_CONCENTRATION_MAP[[extraction$drug$concentration_units %or% "other"]]
  concUnits <- if (is.null(mapped)) "ng" else mapped$units
  for (s in paperReviewIntravenousSeries(extraction, reference, concUnits)) {
    fit <- paperReviewFitExponentials(s$points$time, s$points$observed, n)
    if (is.null(fit)) next
    coef <- fit$A / s$amount            # per unit dose, 1/L
    closed <- tryCatch(switch(n,
      list(v1 = 1 / coef, cl1 = fit$lambda / coef),
      twoCompartmentFromBiexponential(coef, fit$lambda),
      threeCompartmentFromTriexponential(coef, fit$lambda)), error = function(e) NULL)
    if (is.null(closed)) next
    closed$perKg <- FALSE
    return(list(closed = closed,
                route = paste0("the digitised intravenous curve of ", s$figure, " (approximate)"),
                locations = c(s$location, s$regimenLocation)))
  }
  NULL
}
