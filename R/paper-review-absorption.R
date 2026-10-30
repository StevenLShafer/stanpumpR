# Single Paper Review: extravascular absorption, and parameters from curves.
#
# A paper often fits no absorption model and still prints enough to simulate
# its oral (or IM, SC, IN, SL) doses: a bioavailability, a mean absorption
# time, an oral MRT beside the intravenous one, a tmax, or a figure of the
# oral curve.  The first real paper reviewed (Singhvi 1988, fosinopril)
# printed F, MAT and tmax for two formulations and no ka, and Shafer asked
# that such routes be simulated too, and that parameters be derived from the
# displayed curves where possible.  So, for each extravascular regimen:
#
#   ka   printed; else ln 2 / an absorption half-life; else 1 / (MAT - tlag);
#        else 1 / (MRT_oral - Vss/CL - tlag); else fitted to that regimen's
#        digitised points; else solved so the model's tmax is the printed one.
#   F    printed; else from the paper's dose-normalised oral and intravenous
#        AUCs (the crossover ratio, read from the same study, never fitted to
#        another study's exposure); else 1 for apparent parameters; else
#        fitted with ka to the digitised points.
#   dose the fraction of the dosed amount that is the analyte, as the paper
#        states it (a prodrug salt equimolar to so much drug).
#
# Absorption is first order throughout; the report says which source each
# number came from and that a curve-derived value is only as good as Claude's
# reading of the figure.  The disposition is stanpumpR's own: its bolus
# coefficients and eigenvalues come from getDrugPK().
#
# An intravenous curve is also fitted with one, two and three exponentials,
# and the report says how many the digitised points support; where the
# printed numbers do not close the model, the fitted curve can.
# (Claude Code, 2026-10-30, at the request of Steven L. Shafer.)

# mg of drug to the model's amount units, so that amount / litres is in the
# model's concentration units per mL.
paperReviewAmountFactor <- function(concUnits)
  c(pg = 1e6, ng = 1e3, mcg = 1)[[concUnits]]

#' The disposition's unit-bolus coefficients and eigenvalues, from getDrugPK()
#'
#' @param drug a paperReviewDrug() result (its disposition is what is used)
#' @param patient the patient
#' @returns a list: \code{coef} (1/L, per mg), \code{lambda} (1/min); NULL
#'   when the model cannot be built
#' @keywords internal
paperReviewDispositionModes <- function(drug, patient)
{
  model <- drug$model
  model$PK$default <- model$PK$default[c("v1", "v2", "v3", "cl1", "cl2", "cl3")]
  model$tPeak <- 0; model$ke0 <- NULL
  pk <- tryCatch(paperReviewQuietly(function()
    getDrugPK(drug$defaults$Drug, patient$weight, patient$height, patient$age, patient$sex,
              drug$defaults, model = model)), error = function(e) NULL)
  if (is.null(pk)) return(NULL)
  s <- pk$PK$default
  lambda <- c(s$lambda_1, s$lambda_2, s$lambda_3)
  coef <- c(s$p_coef_bolus_l1, s$p_coef_bolus_l2, s$p_coef_bolus_l3)
  keep <- lambda > 0
  list(coef = coef[keep], lambda = lambda[keep])
}

#' Concentrations after first-order extravascular doses
#'
#' The closed form for each dose: amount F (sum over modes of
#' c_i ka / (ka - lambda_i) (exp(-lambda_i tau) - exp(-ka tau))), tau the
#' time since the dose and its lag.
#'
#' @param t times, minutes
#' @param doses data frame of Time (min) and amount (in the model's amount
#'   units)
#' @param modes paperReviewDispositionModes()
#' @param ka,F,tlag the absorption
#' @returns concentrations in the model's units
#' @keywords internal
paperReviewExtravascularCurve <- function(t, doses, modes, ka, F, tlag = 0)
{
  # Keep ka off every eigenvalue, as getDrugPK() does (separateEigenvalues()).
  if (any(abs(ka - modes$lambda) < 1e-9 * ka)) ka <- ka * (1 + 1e-6)
  out <- numeric(length(t))
  for (d in seq_len(nrow(doses))) {
    tau <- t - doses$Time[d] - tlag
    on <- tau > 0
    if (!any(on)) next
    tau <- tau[on]
    shape <- colSums(modes$coef * ka / (ka - modes$lambda) *
                       (exp(-outer(modes$lambda, tau)) - matrix(exp(-ka * tau), length(modes$lambda),
                                                                 length(tau), byrow = TRUE)))
    out[on] <- out[on] + doses$amount[d] * F * shape
  }
  out
}

# The extravascular doses of a regimen, with amounts in the model's units and
# the analyte fraction the paper states; NULL when there are none.
paperReviewExtravascularDoses <- function(regimen, reference, concUnits)
{
  rows <- list()
  for (d in regimen$doses %or% list()) {
    if (!(d$route %or% "other") %in% names(PAPER_REVIEW_ROUTE_SUFFIX)) next
    mg <- c(g = 1000, mg = 1, mcg = 1e-3, ng = 1e-6)[d$mass_unit %or% "other"]
    minutes <- paperReviewMinutes(d$time_units %or% "min")
    if (is.na(mg) || is.na(minutes)) next
    amount <- d$amount * mg * (if (identical(d$per_kg, "yes")) reference$weight else 1)
    rows[[length(rows) + 1]] <- data.frame(
      Time = d$time * minutes, amount = amount * paperReviewAmountFactor(concUnits),
      fraction = paperReviewNum(d$analyte_fraction), route = d$route,
      stringsAsFactors = FALSE)
  }
  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
}

#' The digitised concentration series of the paper's figures
#'
#' @param extraction the extraction
#' @param concUnits the model's concentration units
#' @returns a list of series: \code{figure}, \code{label}, \code{regimen},
#'   \code{location}, \code{logY}, \code{points} (time in minutes,
#'   observed in the model's units)
#' @keywords internal
paperReviewFigureSeries <- function(extraction, concUnits)
{
  out <- list()
  for (f in extraction$figures %or% list()) {
    if (!identical(f$y_quantity, "concentration")) next
    factor <- paperReviewConcentrationFactor(f$y_units %or% "other", concUnits,
                                             extraction$drug$molecular_weight)
    minutes <- paperReviewMinutes(f$time_units %or% "min")
    if (!is.finite(factor) || is.na(minutes)) next
    for (s in f$series %or% list()) {
      p <- s$points %or% list()
      if (length(p) == 0) next
      pts <- data.frame(
        time = vapply(p, function(x) paperReviewNum(x$time), numeric(1)) * minutes,
        observed = vapply(p, function(x) paperReviewNum(x$value), numeric(1)) * factor)
      pts <- pts[is.finite(pts$time) & is.finite(pts$observed) & pts$observed > 0, , drop = FALSE]
      if (nrow(pts) == 0) next
      out[[length(out) + 1]] <- list(
        figure = f$figure_id %or% "Figure", label = s$label %or% "",
        regimen = s$regimen_id %or% f$regimen_id %or% NA_character_,
        location = f$location %or% "", logY = identical(f$log_y, "yes"), points = pts)
    }
  }
  out
}

# Fit ka (and F when it is not known) to digitised points: least squares on
# log concentrations, which weighs a log-axis figure's points evenly.
paperReviewFitAbsorption <- function(points, doses, modes, F, tlag)
{
  sse <- function(ka, f) {
    pred <- paperReviewExtravascularCurve(points$time, doses, modes, ka, f, tlag)
    if (any(!is.finite(pred)) || any(pred <= 0)) return(1e10)
    sum((log(points$observed) - log(pred))^2)
  }
  lambdaMin <- min(modes$lambda)
  range <- log(c(lambdaMin / 100, 100 * max(modes$lambda, 1 / 10)))
  # The error is not unimodal in ka, so a grid first, then a local search
  # around its best point (a golden-section search alone found ka = 53 /min
  # for a curve made with 0.02).
  search <- function(objective) {
    grid <- seq(range[1], range[2], length.out = 241)
    values <- vapply(grid, objective, numeric(1))
    i <- which.min(values)
    step <- grid[2] - grid[1]
    stats::optimize(objective, c(grid[max(1, i - 1)] - step / 2, grid[min(length(grid), i + 1)] + step / 2))
  }
  if (is.finite(F)) {
    fit <- search(function(lk) sse(exp(lk), F))
    return(list(ka = exp(fit$minimum), F = F, rss = fit$objective))
  }
  # F unknown: for any ka the best F is the geometric-mean ratio, closed form.
  bestF <- function(ka) {
    pred <- paperReviewExtravascularCurve(points$time, doses, modes, ka, 1, tlag)
    ok <- pred > 0
    if (!any(ok)) return(NA_real_)
    exp(mean(log(points$observed[ok]) - log(pred[ok])))
  }
  fit <- search(function(lk) { f <- bestF(exp(lk)); if (!is.finite(f)) 1e10 else sse(exp(lk), f) })
  ka <- exp(fit$minimum)
  list(ka = ka, F = bestF(ka), rss = fit$objective)
}

# The time of the peak after a single dose at time 0.
paperReviewExtravascularTmax <- function(modes, ka, tlag)
{
  dose <- data.frame(Time = 0, amount = 1)
  horizon <- tlag + 20 / ka + 5 / min(modes$lambda)
  grid <- tlag + exp(seq(log(1e-3), log(horizon - tlag), length.out = 400))
  c0 <- paperReviewExtravascularCurve(grid, dose, modes, ka, 1, tlag)
  i <- which.max(c0)
  lo <- grid[max(1, i - 1)]; hi <- grid[min(length(grid), i + 1)]
  stats::optimize(function(t) paperReviewExtravascularCurve(t, dose, modes, ka, 1, tlag),
                  c(lo, hi), maximum = TRUE)$maximum
}

#' Absorption for one extravascular regimen
#'
#' @param extraction the extraction
#' @param regimen one entry of \code{extraction$regimens}, or NULL for the
#'   route-level parameters alone
#' @param route the extraction's route word ("oral", "im", ...)
#' @param drug paperReviewDrug() without absorption, at the reference patient
#' @param reference the reference patient
#' @returns a list: \code{ok}, \code{route}, \code{ka} (1/min), \code{F},
#'   \code{tlag} (min), \code{fraction}, \code{sources} (named text, one per
#'   quantity), \code{locations}, \code{used} (keys "quantity|regimen" of
#'   the derived quantities consumed), \code{crossCheck} (text or NULL),
#'   \code{regimen}
#' @keywords internal
paperReviewAbsorption <- function(extraction, regimen, route, drug, reference)
{
  rid <- if (is.null(regimen)) NULL else regimen$id
  w <- reference$weight
  concUnits <- drug$defaults$Concentration.Units
  sources <- c(); locations <- character(0); used <- character(0)
  out <- list(ok = FALSE, route = route, regimen = rid, ka = NA_real_, F = NA_real_,
              tlag = 0, fraction = 1)

  # A printed parameter of this regimen, else of the route.
  param <- function(role) {
    cands <- Filter(function(p) identical(p$role, role) &&
                      (is.null(p$route) || identical(p$route, route)),
                    extraction$parameters %or% list())
    own <- Filter(function(p) !is.null(rid) && identical(p$regimen_id, rid), cands)
    any <- Filter(function(p) is.null(p$regimen_id), cands)
    pick <- if (length(own)) own[[1]] else if (length(any)) any[[1]] else NULL
    if (is.null(pick)) return(NULL)
    unit <- paperReviewUnit(pick$units %or% "other")
    if (is.na(unit$kind) || unit$kind != PAPER_REVIEW_ROLE_KIND[[role]]) return(NULL)
    list(value = paperReviewNum(pick$value) * unit$factor * (if (unit$perKg) w else 1),
         location = pick$location %or% NA_character_)
  }
  # A printed derived quantity of this regimen.
  derived <- function(q, id = rid) {
    if (is.null(id)) return(NULL)
    for (d in paperReviewDerived(extraction, q, w, concUnits))
      if (identical(d$regimen_id, id) && identical(d$kind, PAPER_REVIEW_DERIVED_KIND[[q]]) &&
          is.finite(d$internal))
        return(list(value = d$internal, location = d$location %or% NA_character_,
                    key = paste(q, id, sep = "|")))
    NULL
  }
  take <- function(x, what, text) {
    sources[[what]] <<- text
    locations <<- c(locations, x$location)
    if (!is.null(x$key)) used <<- c(used, x$key)
    x$value
  }

  doses <- if (is.null(regimen)) NULL else paperReviewExtravascularDoses(regimen, reference, concUnits)
  fraction <- if (!is.null(doses) && is.finite(doses$fraction[1])) doses$fraction[1] else 1
  if (fraction != 1) sources[["dose"]] <- paste0(
    "the paper's statement that a dosed mg is ", prFmt(fraction), " mg of the analyte")
  out$fraction <- fraction

  tl <- param("tlag")
  tlag <- if (is.null(tl)) 0 else take(tl, "tlag", "printed")
  modes <- paperReviewDispositionModes(drug, reference)
  if (is.null(modes)) return(c(out, list(sources = sources, locations = locations, used = used,
                                         crossCheck = NULL)))
  vss <- drug$model$PK$default$v1 +
    sum(unlist(drug$model$PK$default[c("v2", "v3")])[c(drug$model$PK$default$cl2,
                                                        drug$model$PK$default$cl3) > 0])
  mrtIV <- vss / drug$model$PK$default$cl1

  # F
  F <- NA_real_
  apparent <- identical(extraction$structure$apparent_oral, "yes")
  p <- param("F"); d <- derived("F")
  if (!is.null(p)) F <- take(p, "F", "printed")
  else if (!is.null(d)) F <- take(d, "F", "printed for this regimen")
  else if (apparent) { F <- 1; sources[["F"]] <- "1: the parameters are apparent (CL/F, V/F)" }
  else {
    aucPO <- derived("AUC_inf")
    iv <- Filter(function(r) all(vapply(r$doses %or% list(), function(x)
      (x$route %or% "") %in% c("iv_bolus", "iv_infusion"), logical(1))), extraction$regimens %or% list())
    for (r in iv) {
      aucIV <- derived("AUC_inf", r$id)
      if (is.null(aucPO) || is.null(aucIV) || is.null(doses)) break
      ivAmount <- paperReviewRegimenAmount(paperReviewRegimenDoses(r)$doses, drug, reference)
      poAmount <- sum(doses$amount) / paperReviewAmountFactor(concUnits) * fraction
      F <- (aucPO$value / poAmount) / (aucIV$value / ivAmount)
      take(aucPO, "F", paste0("the paper's dose-normalised AUCs, this regimen's over ", r$id,
                              "'s (a crossover ratio from the same study)"))
      take(aucIV, "F2", "")
      sources[["F2"]] <- NULL
      break
    }
  }

  # ka
  ka <- NA_real_
  p <- param("ka")
  if (!is.null(p)) ka <- take(p, "ka", "printed")
  if (!is.finite(ka) && !is.null(d <- derived("t_half_absorption")))
    ka <- log(2) / take(d, "ka", "ln 2 / the printed absorption half-life")
  if (!is.finite(ka) && !is.null(d <- derived("MAT"))) {
    mat <- d$value - tlag
    if (mat > 0) ka <- 1 / take(d, "ka", "1 / the printed mean absorption time (first-order absorption assumed)")
  }
  if (!is.finite(ka) && !is.null(d <- derived("MRT"))) {
    mat <- d$value - mrtIV - tlag
    if (mat > 0) ka <- 1 / take(d, "ka", paste0(
      "1 / (the printed MRT - the model's intravenous MRT ", prFmt(mrtIV / 60), " h), first order assumed"))
  }

  # The digitised points of this regimen: a fit, or a cross-check.
  series <- Filter(function(s) !is.null(rid) && identical(s$regimen, rid),
                   paperReviewFigureSeries(extraction, concUnits))
  crossCheck <- NULL
  fittedFigures <- character(0)
  if (length(series) && !is.null(doses)) {
    pts <- do.call(rbind, lapply(series, function(s) s$points))
    fit <- tryCatch(paperReviewFitAbsorption(pts, doses, modes, if (is.finite(F)) F * fraction else NA, tlag),
                    error = function(e) NULL)
    figs <- paste(unique(vapply(series, function(s) s$figure, "")), collapse = ", ")
    if (!is.null(fit) && is.finite(fit$ka)) {
      if (!is.finite(ka)) {
        ka <- fit$ka
        fittedFigures <- unique(vapply(series, function(s) s$figure, ""))
        sources[["ka"]] <- paste0("fitted to ", figs, "'s digitised points (approximate)")
        locations <- c(locations, vapply(series, function(s) s$location, ""))
        if (!is.finite(F)) {
          F <- fit$F / fraction
          sources[["F"]] <- paste0("fitted with ka to ", figs, "'s digitised points (approximate)")
        }
      } else {
        crossCheck <- paste0("A fit to ", figs, "'s digitised points gives ka ",
                             prFmt(fit$ka * 60), " /h against ", prFmt(ka * 60), " /h from ",
                             sources[["ka"]], ".")
      }
    }
  }
  if (!is.finite(ka) && !is.null(d <- derived("tmax")) && is.finite(F)) {
    target <- d$value
    f <- function(lk) paperReviewExtravascularTmax(modes, exp(lk), tlag) - target
    lo <- log(min(modes$lambda) / 10); hi <- log(100)
    if (target > tlag && f(lo) > 0 && f(hi) < 0) {
      ka <- exp(stats::uniroot(f, c(lo, hi))$root)
      take(d, "ka", "solved so the model's tmax is the printed one (a mean of individual tmax values; approximate)")
    }
  }
  out$ka <- ka; out$F <- F; out$tlag <- tlag
  out$ok <- is.finite(ka) && ka > 0 && is.finite(F) && F > 0
  c(out, list(sources = sources, locations = unique(locations[!is.na(locations)]),
              used = used, crossCheck = crossCheck, fittedFigures = fittedFigures))
}

#' Absorption for every extravascular regimen of a paper
#'
#' @returns a named list (by regimen id; "route" for the route-level
#'   parameters when no regimen is extravascular)
#' @keywords internal
paperReviewAbsorptions <- function(extraction, drug, reference)
{
  out <- list()
  for (r in extraction$regimens %or% list()) {
    routes <- vapply(r$doses %or% list(), function(d) d$route %or% "other", "")
    ev <- intersect(routes, names(PAPER_REVIEW_ROUTE_SUFFIX))
    if (length(ev) == 0 || is.null(r$id)) next
    out[[r$id]] <- paperReviewAbsorption(extraction, r, ev[[1]], drug, reference)
  }
  route <- paperReviewExtravascularRoute(extraction)
  if (length(out) == 0 && !is.na(route))
    out[["route"]] <- paperReviewAbsorption(extraction, NULL, route, drug, reference)
  out
}

# --- Exponentials from an intravenous curve ----------------------------------

#' Fit a sum of n exponentials to points, on log concentrations
#'
#' @param t,y times and concentrations
#' @param n 1, 2 or 3
#' @returns a list: \code{A}, \code{lambda} (fastest first), \code{rss},
#'   \code{aic}; NULL when it fails
#' @keywords internal
paperReviewFitExponentials <- function(t, y, n)
{
  m <- length(t)
  if (m < 2 * n + 1) return(NULL)
  ly <- log(y)
  sse <- function(theta) {
    A <- exp(theta[seq_len(n)]); l <- exp(theta[n + seq_len(n)])
    pred <- colSums(A * exp(-outer(l, t)))
    if (any(!is.finite(pred)) || any(pred <= 0)) return(1e10)
    sum((ly - log(pred))^2)
  }
  tail <- order(t, decreasing = TRUE)[1:min(3, m)]
  slope <- -stats::coef(stats::lm(ly[tail] ~ t[tail]))[[2]]
  if (!is.finite(slope) || slope <= 0) slope <- 1 / max(t)
  best <- NULL
  for (spread in c(3, 10, 30, 100)) {
    l0 <- slope * spread^(rev(seq_len(n)) - 1)
    A0 <- rep(max(y) / n, n)
    fit <- tryCatch(stats::optim(c(log(A0), log(l0)), sse, method = "Nelder-Mead",
                                 control = list(maxit = 4000, reltol = 1e-12)),
                    error = function(e) NULL)
    if (!is.null(fit)) fit <- tryCatch(stats::optim(fit$par, sse, method = "BFGS",
                                                    control = list(maxit = 1000)),
                                       error = function(e) fit)
    if (!is.null(fit) && (is.null(best) || fit$value < best$value)) best <- fit
  }
  if (is.null(best)) return(NULL)
  A <- exp(best$par[seq_len(n)]); l <- exp(best$par[n + seq_len(n)])
  o <- order(l, decreasing = TRUE)
  rss <- best$value
  list(A = A[o], lambda = l[o], rss = rss,
       aic = m * log(max(rss, 1e-12) / m) + 2 * (2 * n))
}

#' The intravenous series a sum of exponentials can be fitted to
#'
#' A single bolus, or a single infusion shorter than a tenth of the series'
#' span (its points during the infusion dropped), with its amount.
#'
#' @returns a list of series, each with \code{amount} (model units) added
#' @keywords internal
paperReviewIntravenousSeries <- function(extraction, reference, concUnits)
{
  out <- list()
  for (s in paperReviewFigureSeries(extraction, concUnits)) {
    r <- paperReviewRegimen(extraction, if (is.na(s$regimen)) NULL else s$regimen)
    if (is.null(r) || length(r$doses) != 1) next
    d <- r$doses[[1]]
    mg <- c(g = 1000, mg = 1, mcg = 1e-3, ng = 1e-6)[d$mass_unit %or% "other"]
    if (is.na(mg)) next
    amount <- d$amount * mg * (if (identical(d$per_kg, "yes")) reference$weight else 1)
    minutes <- paperReviewMinutes(d$time_units %or% "min")
    start <- d$time * minutes
    pts <- s$points
    if (identical(d$route, "iv_infusion")) {
      duration <- paperReviewNum(d$duration) * minutes
      if (!is.finite(duration) || duration > 0.1 * (max(pts$time) - start)) next
      if ((d$per_time %or% "none") == "per_min") amount <- amount * duration
      if ((d$per_time %or% "none") == "per_h") amount <- amount * duration / 60
      pts <- pts[pts$time > start + duration, , drop = FALSE]
    } else if (!identical(d$route, "iv_bolus")) next
    pts$time <- pts$time - start
    pts <- pts[pts$time > 0, , drop = FALSE]
    if (nrow(pts) < 3) next
    s$points <- pts
    s$amount <- amount * paperReviewAmountFactor(concUnits)
    s$regimenLocation <- r$location %or% ""
    out[[length(out) + 1]] <- s
  }
  out
}
