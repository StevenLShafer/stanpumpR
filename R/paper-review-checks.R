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
  AUC_inf = "auc", AUC_0_t = "auc", Cmax = "concentration", tmax = "time",
  tPeak = "time", ke0 = "rate")

#' The derived quantities of one kind, in internal units
#' @keywords internal
paperReviewDerived <- function(extraction, quantity, weight, concUnits = "ng")
{
  out <- list()
  for (dq in extraction$derived_quantities %or% list())
    if (identical(dq$quantity, quantity)) {
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
  ok <- !is.na(d[[1]]$kind) && identical(d[[1]]$kind, kind)
  list(value = if (ok) d[[1]]$internal else NA_real_, location = d[[1]]$location,
       entry = d[[1]], perKg = isTRUE(paperReviewUnit(d[[1]]$units %or% "other")$perKg))
}

# --- Identifiability ----------------------------------------------------------

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
  out <- list()
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
    out[[i]] <- found
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
  if (have(vc) || have(micro)) {
    roles <- if (have(vc)) vc else micro
    verdict <- "printed"; route <- paste(roles, collapse = ", ")
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

  # Absorption and effect site.
  evRoute <- paperReviewExtravascularRoute(extraction)
  if (!is.na(evRoute)) {
    ka <- param("ka"); F <- param("F"); lag <- param("tlag")
    apparent <- identical(extraction$structure$apparent_oral, "yes")
    if (is.na(ka$value))
      add("concern", "No absorption rate constant is printed",
          "The extravascular route cannot be simulated; the disposition model still can.",
          structureLoc)
    if (is.na(F$value) && !apparent)
      add("concern", "No bioavailability is printed and the parameters are not marked apparent",
          "The simulator takes F = 1, which is right only if the volumes and clearances are CL/F and V/F.",
          structureLoc)
    if (identical(extraction$structure$absorption_lag, "yes") && is.na(lag$value))
      add("concern", "The model has a lag time but none is printed", "", structureLoc)
  }
  effect <- extraction$structure$effect_site %or% "none"
  if (effect == "ke0" && is.na(param("ke0")$value))
    add("error", "The paper describes an effect site but prints no ke0", "", structureLoc)
  if (effect == "tpeak" && is.na(param("tPeak")$value))
    add("error", "The paper describes an effect site but prints no time to peak effect", "", structureLoc)
  if (effect %in% c("pd_without_effect_site", "other"))
    add("info", "The pharmacodynamic model is not an effect-site model",
        paste0("The simulator plots concentrations only. ", extraction$structure$pd_model %or% ""),
        structureLoc)

  list(verdict = verdict, route = route, closed = closed, findings = findings)
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
    s <- paperReviewSE(printed, entry$uncertainty, n)
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
paperReviewConsistency <- function(extraction, drug, reference)
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
  lambda <- c(pk$lambda_1, pk$lambda_2, pk$lambda_3)[seq_len(n)]
  coef <- c(pk$p_coef_bolus_l1, pk$p_coef_bolus_l2, pk$p_coef_bolus_l3)[seq_len(n)]
  # The placeholder volume of an absent compartment is not part of Vss.
  vss <- pk$v1 + (if (n >= 2) pk$v2 else 0) + (if (n >= 3) pk$v3 else 0)
  ke0 <- if (isTRUE(pk$ke0 > 0)) pk$ke0 else NA_real_
  tPeak <- if (is.finite(ke0)) effectSitePeakTime(coef, lambda, ke0) else NA_real_

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
      NA_real_)
  }

  for (dq in extraction$derived_quantities %or% list())
  {
    q <- dq$quantity %or% "other"
    if (q %in% c("other", "AUC_0_t", "t_half_absorption")) next
    printedValue <- paperReviewNum(dq$value)
    conv <- paperReviewDerivedInternal(dq, w, concUnits, extraction$drug$molecular_weight)
    expected <- PAPER_REVIEW_DERIVED_KIND[[q]]
    model <- NA_real_
    note <- ""
    incomparable <- FALSE
    if (expected == "coefficient") {
      i <- as.integer(sub("^.*_", "", q))
      if (i > n) next
      if (identical(dq$units, "1/L")) {
        model <- coef[i]; conv$kind <- "coefficient"
      } else if (identical(conv$kind, "concentration")) {
        bolus <- paperReviewBolusAmount(extraction, dq$regimen_id, concUnits, reference,
                                        log(2) / lambda[1])
        if (!is.na(bolus$amount)) {
          model <- coef[i] * bolus$amount; conv$kind <- "coefficient"; note <- bolus$note
        } else {
          note <- "No single intravenous dose to scale the coefficient by."
        }
      }
    } else if (q %in% c("AUC_inf", "Cmax", "tmax", "MRT") && !is.null(dq$regimen_id)) {
      # A quantity of one regimen is compared only if the model can give that
      # regimen: an oral AUC against a model with no oral route is not a
      # disagreement (the first real paper reported both).
      regimen <- paperReviewRegimen(extraction, dq$regimen_id)
      routable <- if (is.null(regimen)) list(ok = FALSE, why = "The regimen is not identified.")
                  else paperReviewRegimenRoutable(regimen, drug)
      if (!routable$ok) {
        incomparable <- TRUE
        note <- routable$why
      } else if (q == "MRT") {
        model <- paperReviewRegimenMRT(regimen, drug, vss / pk$cl1)
      } else {
        res <- paperReviewRegimenExposure(extraction, drug, reference, dq$regimen_id)
        model <- res[[q]]
        note <- res$note
      }
      if (q %in% c("Cmax", "tmax") && !identical(dq$statistic, "typical_value") && routable$ok)
        note <- paste0(note, " The paper's value is a ",
                       gsub("_", " ", dq$statistic %or% "unstated statistic"),
                       "; the model's is the typical subject's curve, which need not equal it.")
    } else {
      model <- recomputed(dq)
    }
    kindOk <- identical(conv$kind, expected)
    printedInternal <- if (kindOk) conv$value else NA_real_
    relative <- (model - printedInternal) / printedInternal
    roundingInternal <- paperReviewRounding(dq$value_text) * if (is.finite(printedValue) && printedValue != 0)
      printedInternal / printedValue else NA_real_
    withinRounding <- is.finite(model) && is.finite(printedInternal) &&
      isTRUE(abs(model - printedInternal) <= roundingInternal * 1.0001)
    loose <- q %in% c("Cmax", "tmax") && !identical(dq$statistic, "typical_value")
    status <- if (incomparable) "not comparable"
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
  table <- if (length(rows)) do.call(rbind, rows) else NULL
  if (!is.null(table)) {
    compared <- table$status %in% c("matches", "close", "differs", "disagrees")
    nMatch <- sum(table$status %in% c("matches", "close"))
    findings <- c(list(paperReviewFinding(section,
      if (nMatch == sum(compared)) "ok" else "info",
      paste0(nMatch, " of ", sum(compared), " printed derived quantities compared agree with the model within 5%"),
      paste0("Each was recomputed from the printed parameters through stanpumpR (getDrugPK(), cube(), ",
             "effectSitePeakTime(), simCpCe()).",
             if (any(!compared)) paste0(" ", sum(!compared), " more could not be compared; the table says why.")
             else ""))),
      findings)
  } else {
    findings <- list(paperReviewFinding(section, "info",
      "The paper prints no derived quantities to check against the model"))
  }
  list(table = table, findings = findings)
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
  missing <- setdiff(extravascular, if (is.na(drug$route)) character(0) else drug$route)
  if (length(missing) == 0) return(list(ok = TRUE, why = ""))
  list(ok = FALSE, why = paste0("The model has no ", paste(gsub("_", " ", missing), collapse = ", "),
                                " route: the paper prints no absorption constant for it."))
}

# The mean residence time of a regimen: the disposition's Vss/CL, plus the
# mean absorption time 1/ka and the lag for an extravascular dose.
paperReviewRegimenMRT <- function(regimen, drug, mrtIV)
{
  routes <- vapply(regimen$doses %or% list(), function(d) d$route %or% "other", character(1))
  if (all(routes %in% c("iv_bolus", "iv_infusion"))) return(mrtIV)
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
    if (grepl("/min|/hr", u)) {
      nextTime <- doses$Time[doses$Units == u & doses$Time > doses$Time[r]]
      if (length(nextTime) == 0) next
      a <- a * (min(nextTime) - doses$Time[r]) / if (grepl("/hr", u)) 60 else 1
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

  for (field in c("weight", "height", "age"))
    if (grepl("default", reference$source[[field]]))
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
    summary <- switch(e$covariate, weight = pop$weight_kg, height = pop$height_cm, age = pop$age_years, NULL)
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
  if ((s$absorption %or% "none") %in% c("zero_order", "transit", "other"))
    add("concern", paste("Absorption is", gsub("_", " ", s$absorption)),
        "The in-memory simulator absorbs first order, so its early curve will differ from the paper's.",
        s$location)

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
    if (!identical(f$y_quantity, "concentration")) {
      add("info", paste(id, "is not a concentration figure"), "Not compared.", f$location)
      next
    }
    if (!drug$ok) { add("info", paste(id, "not compared"), "There is no complete model.", f$location); next }
    factor <- paperReviewConcentrationFactor(f$y_units %or% "other", concUnits, extraction$drug$molecular_weight)
    minutes <- paperReviewMinutes(f$time_units %or% "min")
    if (!is.finite(factor) || is.na(minutes)) {
      add("concern", paste(id, "could not be compared"), "Its units could not be converted.", f$location)
      next
    }
    # Each series may show its own regimen (a figure of an intravenous and two
    # oral curves); a series without one takes the figure's.  The first real
    # paper's figure had three regimens and only one slot for them.
    points <- do.call(rbind, lapply(f$series %or% list(), function(s) {
      p <- s$points %or% list()
      if (length(p) == 0) return(NULL)
      data.frame(series = s$label %or% "",
                 regimen = s$regimen_id %or% f$regimen_id %or% NA_character_,
                 time = vapply(p, function(x) paperReviewNum(x$time), numeric(1)) * minutes,
                 observed = vapply(p, function(x) paperReviewNum(x$value), numeric(1)) * factor,
                 lower = vapply(p, function(x) paperReviewNum(x$error_lower), numeric(1)) * factor,
                 upper = vapply(p, function(x) paperReviewNum(x$error_upper), numeric(1)) * factor,
                 stringsAsFactors = FALSE)
    }))
    if (is.null(points) || nrow(points) == 0) {
      add("info", paste(id, "has no digitised points"), f$digitization_note %or% "", f$location)
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
      doses <- paperReviewRegimenDoses(regimen)
      if (nrow(doses$doses) == 0) {
        setAside <- c(setAside, paste0(labels, " (", rid, "): its doses could not be converted"))
        next
      }
      maximum <- max(c(points$time[these], doses$doses$Time), na.rm = TRUE) * 1.05 + 1
      sim <- tryCatch(paperReviewSimulate(drug, reference, doses$doses, maximum),
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
    note <- if (length(setAside))
      paste0(" Not compared: ", paste(setAside, collapse = "; "), ".") else ""
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
    add(severity, paste0(id, ": the model ", if (severity == "ok") "reproduces" else "does not closely reproduce",
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
    paperReviewFinding("Claude's reading", n$severity %or% "info", n$topic %or% "Note",
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
  reference <- paperReviewReferencePatient(extraction)
  ident <- paperReviewIdentifiability(extraction)
  drug <- paperReviewDrug(extraction, reference, ident$closed)
  consistency <- paperReviewConsistency(extraction, drug, reference)
  figures <- paperReviewFigureChecks(extraction, drug, reference)
  modelFindings <- lapply(drug$problems, function(p)
    paperReviewFinding("Model", "concern", p))
  findings <- c(ident$findings, modelFindings, consistency$findings,
                paperReviewCovariateChecks(extraction),
                paperReviewAssumptionChecks(extraction),
                figures$findings, paperReviewClaudeNotes(extraction))
  list(extraction = extraction, reference = reference, identifiability = ident,
       drug = drug, consistency = consistency$table, figures = figures$figures,
       findings = findings)
}
