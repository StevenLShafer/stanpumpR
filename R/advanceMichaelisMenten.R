# -----------------------------------------------------------------------------
# Numerical engine: one compartment with Michaelis-Menten elimination
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-10-10, at the request of Steven L. Shafer, for
# phenytoin, the one antiseizure drug whose systemic elimination saturates at
# therapeutic concentrations.  Verified by tests/testthat/test-michaelis-menten.R:
# against the closed-form engine when Km is so large that the elimination is
# linear, against the implicit analytical solution of an intravenous bolus,
# against the steady state Css = Km R / (Vmax - R) of a constant infusion, and
# by mass balance.
#
# WHY A SEPARATE ENGINE
# =====================
# Every other engine in the library is closed form: it adds exponentials, and
# superposition makes a dose table a sum of independent doses.  Michaelis-
# Menten elimination,
#
#     dA/dt = inputs(t) - Vmax C / (Km + C),        C = A / V,
#
# has no such solution for an arbitrary dose history, and superposition fails
# by design: the second dose is cleared more slowly because the first is still
# there, which is the clinically important behaviour (a modest dose increase
# gives a disproportionate rise at steady state).  So this engine integrates
# the whole dose table at once, with the classical fourth-order Runge-Kutta
# method, and nothing upstream may split the table into parts that are run
# separately and added (simCpCe() dispatches here before it splits oral
# formulations).
#
# THE INPUTS
# ==========
# All the absorption and conversion processes are linear, first order, and are
# integrated as states alongside the central amount:
#
#   - one depot per oral formulation (the drug's default oral form and each
#     entry of PK$oralFormulations), each with its own ka, bioavailability and
#     lag; a dose enters its depot at the time given plus the lag;
#   - an intravenous bolus enters the central compartment directly;
#   - an intravenous rate row ("mg/min", "mg/hr") is a running input to the
#     central compartment until the next such row;
#   - a prodrug given in drug equivalents ("mg PE", fosphenytoin) enters a
#     conversion compartment, which empties into the central compartment at
#     kConversion; given intramuscularly ("mg PE IM") it first enters an
#     intramuscular depot absorbed at ka_IM; given as a rate ("mg PE/min") it
#     is a running input to the conversion compartment until the next such row.
#
# Each dose is multiplied by its SALT FACTOR before it enters: the mass of the
# modelled moiety in a unit of what was given (phenytoin sodium is 0.92
# phenytoin acid by mass; see R/drugs_phenytoin.R).  The factors are in the
# model's michaelisMenten block, keyed "IV", "PE", "default" (the default oral
# form) and the names of the further oral formulations.
#
# STEP SIZE
# =========
# The engine reports on the library's time line (simulationTimeGrid()), so the
# plot and the derived series behave as for any other drug, and takes Runge-
# Kutta steps of at most MM_MAX_STEP minutes between its points.  The fastest
# process is the conversion of fosphenytoin (half-time about 8 to 15 minutes);
# at a 2 minute step the method's local error on it is below 1e-7 of the dose.
# Elimination is slower still.  A jump (a bolus, a dose arriving in a depot)
# and a change of rate happen only at points of the time line, which always
# carries every dose time, so no step straddles one.
#
# NO EFFECT SITE AND NO TIME UNTIL THRESHOLD
# ==========================================
# Ce is NA throughout (the drug is plotted as plasma only), and the time until
# threshold is not computed: it would need a fresh nonlinear simulation from
# every point of the time line.  The Recovery column is NA.
# -----------------------------------------------------------------------------

#' Largest Runge-Kutta step of the Michaelis-Menten engine, minutes
#' @keywords internal
MM_MAX_STEP <- 2

#' Validate a model's michaelisMenten block
#'
#' @param mm the block a drug model returned, or NULL
#' @param drug the drug's name, for the error message
#' @returns `mm`, unchanged, if valid
#' @keywords internal
validateMichaelisMenten <- function(mm, drug)
{
  if (is.null(mm)) return(NULL)
  ok <- is.list(mm) &&
    is_valid_number(mm$vmax) && mm$vmax > 0 &&
    is_valid_number(mm$km) && mm$km > 0
  if (!ok)
    stop("Invalid michaelisMenten for ", drug, ": needs a positive vmax ",
         "(mass per minute) and a positive km (concentration).")
  for (s in names(mm$saltFactor))
    if (!is_valid_number(mm$saltFactor[[s]], 0, 1) || mm$saltFactor[[s]] <= 0)
      stop("Invalid michaelisMenten for ", drug, ": salt factor '", s,
           "' must lie in (0, 1].")
  if (!is.null(mm$kConversion) &&
      !(is_valid_number(mm$kConversion) && mm$kConversion > 0))
    stop("Invalid michaelisMenten for ", drug, ": kConversion must be positive.")
  mm
}

#' Is a dose unit given in prodrug equivalents ("mg PE", fosphenytoin)?
#' @param units character vector of dose units
#' @returns logical vector
#' @keywords internal
isEquivalentUnit <- function(units)
{
  grepl(" PE( |/|$)", as.character(units))
}

#' Simulate a one-compartment Michaelis-Menten drug
#'
#' See the header of R/advanceMichaelisMenten.R.
#'
#' @param dose the drug's dose table, in base units, with the route flags
#'   \code{simCpCe()} sets (\code{Bolus}, \code{PO}, \code{IM}, ...)
#' @param PK the drug's PK from \code{getDrugPK()}, with a
#'   \code{michaelisMenten} block
#' @param maximum end of the simulation, minutes
#' @param plotRecovery ignored: see the header
#' @param emerge ignored: see the header
#'
#' @returns a data frame of \code{Time}, \code{Cp}, \code{Ce} (NA) and
#'   \code{Recovery} (NA), as the closed-form engines return
#' @keywords internal
advanceMichaelisMenten <- function(dose, PK, maximum, plotRecovery = FALSE,
                                   emerge = 0)
{
  mm <- PK$michaelisMenten
  pkSet <- PK$PK[[PK_EVENT_DEFAULT]]
  V <- pkSet$v1
  vmax <- mm$vmax
  km <- mm$km
  salt <- function(key) {
    s <- mm$saltFactor[[key]]
    if (is.null(s)) 1 else s
  }

  units <- as.character(dose$Units)
  pe <- isEquivalentUnit(units)
  rate <- isRateUnit(units)
  route <- doseRoute(units)
  formulation <- doseFormulation(units)

  # The oral depots: the default form, then each further formulation.
  forms <- c("default", names(PK$oralFormulations))
  absorption <- lapply(forms, function(f) {
    set <- if (f == "default") pkSet else PK$oralFormulations[[f]][[PK_EVENT_DEFAULT]]
    list(ka = set$ka_PO, F = set$bioavailability_PO, tlag = set$tlag_PO,
         salt = salt(f))
  })
  names(absorption) <- forms
  oralForm <- ifelse(!is.na(formulation) & formulation %in% forms, formulation,
                     "default")

  # States: the oral depots, the intramuscular depot of the prodrug, the
  # prodrug's conversion compartment, and the central amount.
  nOral <- length(forms)
  iIM <- nOral + 1
  iConv <- nOral + 2
  iC <- nOral + 3
  ka <- c(vapply(absorption, function(a) a$ka, numeric(1)),
          if (is.null(mm$ka_IM)) 0 else mm$ka_IM)
  kc <- if (is.null(mm$kConversion)) 0 else mm$kConversion

  # Every input as a jump: state index, time it enters, amount.
  jumps <- data.frame(time = numeric(0), state = integer(0), amount = numeric(0))
  addJump <- function(use, time, state, amount) {
    if (!any(use)) return(invisible())
    jumps <<- rbind(jumps, data.frame(time = time[use], state = state[use],
                                      amount = amount[use]))
  }
  t0 <- as.numeric(dose$Time)
  D  <- as.numeric(dose$Dose)

  isOral <- route == ROUTE_PO & !rate
  if (any(isOral)) {
    j <- match(oralForm, forms)
    lag <- vapply(absorption, function(a) a$tlag, numeric(1))[j]
    Fo  <- vapply(absorption, function(a) a$F * a$salt, numeric(1))[j]
    addJump(isOral, t0 + lag, j, D * Fo)
  }
  ivBolus <- route == ROUTE_IV & !rate & !pe
  addJump(ivBolus, t0, rep(iC, length(t0)), D * salt("IV"))
  peBolus <- route == ROUTE_IV & !rate & pe
  addJump(peBolus, t0, rep(iConv, length(t0)), D * salt("PE"))
  peIM <- route == ROUTE_IM & !rate & pe
  if (any(peIM)) {
    lagIM <- if (is.null(mm$tlag_IM)) 0 else mm$tlag_IM
    fIM <- if (is.null(mm$bioavailability_IM)) 1 else mm$bioavailability_IM
    addJump(peIM, t0 + lagIM, rep(iIM, length(t0)), D * fIM * salt("PE"))
  }
  other <- !(isOral | ivBolus | peBolus | peIM | rate)
  if (any(other & D != 0))
    stop("The Michaelis-Menten engine cannot simulate units: ",
         paste(unique(units[other & D != 0]), collapse = ", "))

  # Two running rates, each set by its own rows until the next: the drug
  # intravenously (into the central compartment) and the prodrug (into the
  # conversion compartment).
  ivRate <- rate & !pe
  peRate <- rate & pe

  # The time line: every dose time and arrival, and the instant before each.
  knots <- unique(c(t0, jumps$time))
  timeLine <- simulationTimeGrid(c(knots, knots - PRE_DOSE_OFFSET), maximum,
                                 gridStart(0))
  timeLine <- timeLine[timeLine >= 0]
  L <- length(timeLine)

  rateAt <- function(use, times) {
    if (!any(use)) return(rep(0, length(times)))
    o <- order(t0[use])
    tt <- t0[use][o]
    rr <- D[use][o]
    # The last row at or before each time; zero before the first.
    c(0, rr)[findInterval(times, tt) + 1]
  }
  ivRateLine <- rateAt(ivRate, timeLine) * salt("IV")
  peRateLine <- rateAt(peRate, timeLine) * salt("PE")

  jumpAt <- match(jumps$time, timeLine)
  if (anyNA(jumpAt[jumps$time <= maximum]))
    stop("Internal error: a dose time is missing from the time line.")

  deriv <- function(x, rIV, rPE) {
    C <- x[iC] / V
    dx <- numeric(length(x))
    out <- ka * x[seq_len(iIM)]
    dx[seq_len(iIM)] <- -out
    dx[iConv] <- out[iIM] + rPE - kc * x[iConv]
    dx[iC] <- sum(out[seq_len(nOral)]) + kc * x[iConv] + rIV -
      vmax * C / (km + C)
    dx
  }

  x <- numeric(iC)
  Cp <- numeric(L)
  for (i in seq_len(L)) {
    if (i > 1) {
      dt <- timeLine[i] - timeLine[i - 1]
      n <- max(1L, ceiling(dt / MM_MAX_STEP))
      h <- dt / n
      rIV <- ivRateLine[i - 1]
      rPE <- peRateLine[i - 1]
      for (k in seq_len(n)) {
        k1 <- deriv(x, rIV, rPE)
        k2 <- deriv(x + h / 2 * k1, rIV, rPE)
        k3 <- deriv(x + h / 2 * k2, rIV, rPE)
        k4 <- deriv(x + h * k3, rIV, rPE)
        x <- x + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
      }
      # The central amount cannot fall below zero; RK4 can overshoot by
      # rounding when the compartment is empty.
      x[x < 0] <- 0
    }
    arriving <- which(jumpAt == i)
    for (j in arriving) x[jumps$state[j]] <- x[jumps$state[j]] + jumps$amount[j]
    Cp[i] <- x[iC] / V
  }

  data.frame(
    Time = timeLine,
    Cp = Cp,
    Ce = rep(NA_real_, L),
    Recovery = rep(NA_real_, L)
  )
}
