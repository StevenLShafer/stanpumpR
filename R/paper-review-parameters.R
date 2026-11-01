# Single Paper Review: every parameter a complete intravenous model implies.
#
# Steven L. Shafer's Convert.xls (last updated 12/02) has five tabs, one per
# set of inputs that closes a one-, two- or three-compartment mammillary
# model: (1) volumes and clearances, (2) V1 and the micro rate constants,
# (3) V1, Vdss, CL and the half-lives, (4) the coefficients and exponents of
# the unit disposition function, (5) V1, the exponents, k21 and k31.  Each
# tab then lists every other parameter.  The review closes the model by the
# same five routes (paperReviewIdentifiability()), and paperReviewPKTable()
# is the list each tab prints, for one patient: the covariates act on the
# volumes and clearances, and everything else follows from them.

#' A mammillary model from V1, its exponents, k21 and k31
#'
#' Convert.xls tab 5.  With three compartments, k10 = l1 l2 l3 / (k21 k31),
#' k12 = ((l1 l2 + l1 l3 + l2 l3) - k21 (l1 + l2 + l3) - k10 k31 + k21^2) /
#' (k31 - k21) and k13 = l1 + l2 + l3 - (k10 + k12 + k21 + k31); with two,
#' k10 = l1 l2 / k21 and k12 = l1 + l2 - k21 - k10.
#'
#' @param v1 the central volume
#' @param exponents the two or three exponents, fastest first
#' @param k21,k31 the return rate constants (k31 only for three compartments)
#' @returns a list of \code{v1}.. and \code{cl1}..
#' @keywords internal
compartmentsFromExponentsAndRates <- function(v1, exponents, k21, k31 = NULL)
{
  l <- exponents
  if (length(l) == 2) {
    k10 <- l[1] * l[2] / k21
    k12 <- l[1] + l[2] - k21 - k10
    k <- c(k10, k12, k21)
    if (!all(is.finite(k)) || !all(k > 0))
      stop("These exponents and k21 have no physically admissible two-compartment model")
    return(list(v1 = v1, v2 = v1 * k12 / k21, cl1 = v1 * k10, cl2 = v1 * k12))
  }
  k10 <- prod(l) / (k21 * k31)
  k12 <- ((l[2] * l[3] + l[1] * l[2] + l[1] * l[3]) - k21 * sum(l) - k10 * k31 + k21^2) / (k31 - k21)
  k13 <- sum(l) - (k10 + k12 + k21 + k31)
  k <- c(k10, k12, k13, k21, k31)
  if (!all(is.finite(k)) || !all(k > 0))
    stop("These exponents, k21 and k31 have no physically admissible three-compartment model")
  list(v1 = v1, v2 = v1 * k12 / k21, v3 = v1 * k13 / k31,
       cl1 = v1 * k10, cl2 = v1 * k12, cl3 = v1 * k13)
}

#' A time unit that reads well for a model
#'
#' Minutes unless the terminal half-life is long: a drug whose terminal
#' half-life is days reads better per hour or per day.
#' @keywords internal
paperReviewPKTableUnit <- function(set)
{
  l <- c(set$lambda_1, set$lambda_2, set$lambda_3)
  l <- l[is.finite(l) & l > 0]
  terminal <- if (length(l)) log(2) / min(l) else 0
  if (terminal < 10 * 60) "min" else if (terminal < 10 * 1440) "h" else "day"
}

#' Every parameter of a complete intravenous model
#'
#' The parameters Convert.xls lists on each tab, from one PK set (a
#' getDrugPK() \code{PK$default}, in litres and minutes): the volumes and
#' Vdss, the clearances, the micro rate constants, the exponents and
#' half-lives, the coefficients of the unit disposition function
#' sum A(i) exp(-lambda(i) t) (per litre), and the fractional coefficients
#' A(i) / sum A(i), each exponent's share of the fall after a bolus; then ke0,
#' its half-life and the time to peak effect after a bolus when the model has
#' an effect site.
#'
#' @param set the PK set
#' @param timeUnit the time unit of the rates and clearances (a
#'   PAPER_REVIEW_SIM_TIME_UNITS value); half-lives each take a unit that
#'   reads
#' @returns a data frame: \code{Group}, \code{Parameter}, \code{Value},
#'   \code{Units}
#' @keywords internal
paperReviewPKTable <- function(set, timeUnit = paperReviewPKTableUnit(set))
{
  # An absent compartment carries a placeholder volume with no clearance.
  has <- c(TRUE, isTRUE(set$cl2 > 0), isTRUE(set$cl3 > 0))
  n <- sum(has)
  f <- paperReviewMinutes(timeUnit)
  per <- paste0("1/", timeUnit)
  lambda <- c(set$lambda_1, set$lambda_2, set$lambda_3)[seq_len(n)]
  coef <- c(set$p_coef_bolus_l1, set$p_coef_bolus_l2, set$p_coef_bolus_l3)[seq_len(n)]
  v <- c(set$v1, set$v2, set$v3)[has]
  cl <- c(set$cl1, set$cl2, set$cl3)[has]
  micro <- c(k10 = set$k10, k12 = set$k12, k13 = set$k13, k21 = set$k21, k31 = set$k31)
  micro <- micro[list("k10", c("k10", "k12", "k21"), names(micro))[[n]]]
  readable <- function(minutes)
    if (minutes < 120) list(v = minutes, u = "min") else if (minutes < 2 * 1440) list(v = minutes / 60, u = "h")
    else list(v = minutes / 1440, u = "day")
  rows <- list()
  add <- function(group, parameter, value, units)
    rows[[length(rows) + 1]] <<- data.frame(Group = group, Parameter = parameter,
                                            Value = prFmt(value, 4), Units = units,
                                            stringsAsFactors = FALSE)
  for (i in seq_len(n)) add("Volumes", paste0("V", i), v[i], "L")
  add("Volumes", "Vdss", sum(v), "L")
  for (i in seq_len(n)) add("Clearances", if (i == 1) "CL (metabolic)" else paste0("CL", i),
                            cl[i] * f, paste0("L/", timeUnit))
  for (k in names(micro)) add("Micro rate constants", k, micro[[k]] * f, per)
  for (i in seq_len(n)) add("Exponents", paste0("lambda", i), lambda[i] * f, per)
  for (i in seq_len(n)) {
    h <- readable(log(2) / lambda[i])
    add("Half-lives", paste0("t1/2 lambda", i), h$v, h$u)
  }
  for (i in seq_len(n)) add("Coefficients (unit disposition function)", paste0("A", i), coef[i], "1/L")
  for (i in seq_len(n)) add("Fractional coefficients", paste0("A", i, " / sum(A)"),
                            100 * coef[i] / sum(coef), "%")
  if (isTRUE(set$ke0 > 0)) {
    add("Effect site", "ke0", set$ke0 * f, per)
    h <- readable(log(2) / set$ke0)
    add("Effect site", "t1/2 ke0", h$v, h$u)
    peak <- effectSitePeakTime(coef, lambda, set$ke0)
    if (is.finite(peak)) {
      h <- readable(peak)
      add("Effect site", "Time to peak effect after a bolus", h$v, h$u)
    }
  }
  t <- do.call(rbind, rows)
  t$Group[duplicated(t$Group)] <- ""
  t
}
