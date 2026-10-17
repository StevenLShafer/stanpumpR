# -----------------------------------------------------------------------------
# Numerical engine: Michaelis-Menten elimination (one or two compartments)
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
# THE PERIPHERAL COMPARTMENT (OPTIONAL)
# =====================================
# A model may declare first-order distribution to one peripheral compartment
# with the rate constants k23 and k32 (the source's own naming, central = 2
# and peripheral = 3), which adds
#
#     dA/dt  <- ... - k23 A + k32 Ap,     dAp/dt = k23 A - k32 Ap
#
# to the system above.  Saturable elimination still acts on the CENTRAL
# concentration only.  Both constants are optional and default to zero, which
# leaves the peripheral compartment permanently empty and every one-
# compartment drug's numbers bit-for-bit unchanged (test-michaelis-menten.R
# pins that).  The engine carries it on the same peripheral state as a PK set's
# cl2 and v2 (below): k23 A - k32 Ap is q (C - Cp) with q = k23 V and
# Vp = V k23 / k32.  A drug declaring one must declare both, and the peripheral
# volume is implied by k23/k32 x V rather than given: the source that needed
# this (GHB, Chen 2020) reports the rate constants and derives the volume the
# same way.
#
# WHY NOT A LINEAR TWO-COMPARTMENT FIT INSTEAD
# ============================================
# The peripheral compartment is linear, so it alone would not need this
# engine.  It is here because the ELIMINATION is saturable: a drug needing
# both (GHB) cannot use any closed-form engine, and could not be reduced to
# one compartment without losing the distribution phase its source fitted.
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
#   - a subcutaneous dose, for a drug whose block carries saturableSC
#     (exenatide, R/drugs_exenatide.R; Cirincione 2017): a share F (1 - fr)
#     of the dose enters the central compartment at a constant rate over the
#     next `duration` minutes, and the rest, D (1 - F (1 - fr)), then enters
#     a depot that empties into the central compartment at the saturable rate
#     F fr kaMax S / (kmKa + S).  The doses share the depot.
#
# Each dose is multiplied by its SALT FACTOR before it enters: the mass of the
# modelled moiety in a unit of what was given (phenytoin sodium is 0.92
# phenytoin acid by mass; see R/drugs_phenytoin.R).  The factors are in the
# model's michaelisMenten block, keyed "IV", "PE", "default" (the default oral
# form) and the names of the further oral formulations.
#
# DISPOSITION
# ===========
# The central compartment is the PK set's v1, eliminated by the saturable
# pathway and, when the block carries clLinear (volume per minute), by a
# parallel linear one.  A PK set with a positive cl2 adds a peripheral
# compartment of volume v2 (exenatide); phenytoin's cl2 is 0, which leaves
# it out.  The PK set's cl1 is not read: it holds the low-concentration
# limit for the help tables.  Elimination and distribution act on the
# UNBOUND concentrations, which are the amounts over the volumes unless the
# drug binds a target:
#
#   - rapidBinding (linagliptin, R/drugs_linagliptin.R; Retlich 2015): the
#     drug binds at equilibrium, with dissociation constant kd, to a
#     capacity bmaxCentral (a concentration) in the central compartment and
#     amaxPeripheral (an amount) in the peripheral one.  The states are the
#     total amounts; the unbound concentration is the root of the binding
#     equilibrium (unboundAtEquilibrium()), and the plasma concentration
#     plotted is the total, bound and unbound, as the assay measured it.
#   - targetBinding (vildagliptin, R/drugs_vildagliptin.R; Landersdorfer
#     2012): binding is a process, at a rate (rmax - DR) k2 C / (kd + C)
#     into a drug-target complex DR in each compartment, which dissociates
#     at koff and is hydrolysed at kdeg (freeing the target).  The central
#     and peripheral states are the free drug, and the plasma concentration
#     is the free drug, as the paper's output equation has it.
#
# An oral dose of a drug whose block carries kaTransit (vildagliptin's two
# sequential absorption compartments) passes through a transit compartment,
# emptied at kaTransit, before the default oral depot.
#
# STEP SIZE
# =========
# The engine reports on the library's time line (simulationTimeGrid()), so the
# plot and the derived series behave as for any other drug, and takes Runge-
# Kutta steps of at most MM_MAX_STEP minutes between its points.  The fastest
# process is the conversion of fosphenytoin (half-time about 8 to 15 minutes);
# at a 2 minute step the method's local error on it is below 1e-7 of the dose.
# Elimination is slower still.  The fastest process of all is vildagliptin's
# binding to tissue DPP-4 while the target is free (a rate constant near
# 0.7/min, inside the method's stability limit of 2.78 / 2 min): against a
# 0.05 minute step its concentrations differ by about 1e-5.  A jump (a bolus, a dose
# arriving in a depot) and a change of rate happen only at points of the
# time line, which always carries every dose time, so no step straddles one.
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
    is_valid_number(mm$vmax) && mm$vmax >= 0 &&
    is_valid_number(mm$km) && mm$km > 0 &&
    (mm$vmax > 0 || (is_valid_number(mm$clLinear) && mm$clLinear > 0))
  if (!ok)
    stop("Invalid michaelisMenten for ", drug, ": needs a vmax (mass per ",
         "minute) and a positive km (concentration), and a positive vmax ",
         "unless clLinear is positive.")
  for (s in names(mm$saltFactor))
    if (!is_valid_number(mm$saltFactor[[s]], 0, 1) || mm$saltFactor[[s]] <= 0)
      stop("Invalid michaelisMenten for ", drug, ": salt factor '", s,
           "' must lie in (0, 1].")
  if (!is.null(mm$kConversion) &&
      !(is_valid_number(mm$kConversion) && mm$kConversion > 0))
    stop("Invalid michaelisMenten for ", drug, ": kConversion must be positive.")
  if (!is.null(mm$clLinear) &&
      !(is_valid_number(mm$clLinear) && mm$clLinear >= 0))
    stop("Invalid michaelisMenten for ", drug, ": clLinear must be zero or positive.")
  sc <- mm$saturableSC
  if (!is.null(sc)) {
    ok <- is.list(sc) &&
      is_valid_number(sc$fraction, 0, 1) &&
      is_valid_number(sc$duration) && sc$duration > 0 &&
      is_valid_number(sc$kaMax) && sc$kaMax > 0 &&
      is_valid_number(sc$kmKa) && sc$kmKa > 0 &&
      is_valid_number(sc$bioavailability, 0, 1) && sc$bioavailability > 0
    if (!ok)
      stop("Invalid michaelisMenten for ", drug, ": saturableSC needs a fraction ",
           "and a bioavailability in [0, 1] (bioavailability above 0), and a ",
           "positive duration (min), kaMax (mass/min) and kmKa (mass).")
  }
  positive <- function(v) is_valid_number(v) && v > 0
  rb <- mm$rapidBinding
  if (!is.null(rb) && !(is.list(rb) && positive(rb$kd) &&
                        is_valid_number(rb$bmaxCentral) && rb$bmaxCentral >= 0 &&
                        is_valid_number(rb$amaxPeripheral) && rb$amaxPeripheral >= 0))
    stop("Invalid michaelisMenten for ", drug, ": rapidBinding needs a positive ",
         "kd and a bmaxCentral (concentration) and amaxPeripheral (amount) of ",
         "zero or more.")
  tb <- mm$targetBinding
  if (!is.null(tb) && !(is.list(tb) && positive(tb$kd) && positive(tb$k2) &&
                        is_valid_number(tb$koff) && tb$koff >= 0 &&
                        is_valid_number(tb$kdeg) && tb$kdeg >= 0 &&
                        is_valid_number(tb$rmaxCentral) && tb$rmaxCentral >= 0 &&
                        is_valid_number(tb$rmaxPeripheral) && tb$rmaxPeripheral >= 0))
    stop("Invalid michaelisMenten for ", drug, ": targetBinding needs a positive ",
         "kd and k2, and koff, kdeg, rmaxCentral and rmaxPeripheral of zero or more.")
  if (!is.null(rb) && !is.null(tb))
    stop("Invalid michaelisMenten for ", drug, ": rapidBinding and targetBinding ",
         "are alternatives.")
  if (!is.null(mm$kaTransit) && !positive(mm$kaTransit))
    stop("Invalid michaelisMenten for ", drug, ": kaTransit must be positive.")
  # The optional peripheral compartment: both rate constants or neither, since
  # one alone is a one-way compartment that silently swallows the dose.
  peripheral <- c("k23", "k32")
  given <- peripheral[vapply(peripheral, function(k) !is.null(mm[[k]]), logical(1))]
  if (length(given) == 1L)
    stop("Invalid michaelisMenten for ", drug, ": a peripheral compartment ",
         "needs both k23 and k32; only ", given, " was given.")
  for (k in given)
    if (!(is_valid_number(mm[[k]]) && mm[[k]] > 0))
      stop("Invalid michaelisMenten for ", drug, ": ", k, " must be positive.")
  mm
}

#' Unbound concentration at rapid binding equilibrium
#'
#' Solves total = Cu + B Cu / (kd + Cu) for Cu, the positive root of
#' Cu^2 + (B + kd - total) Cu - total kd = 0, in the form that does not
#' cancel when total is small.
#'
#' @param total total concentration (amount over volume)
#' @param B binding capacity as a concentration
#' @param kd dissociation constant
#' @returns the unbound concentration
#' @keywords internal
unboundAtEquilibrium <- function(total, B, kd)
{
  if (total <= 0) return(0)
  b <- B + kd - total
  root <- sqrt(b * b + 4 * total * kd)
  if (b > 0) 2 * total * kd / (b + root) else (root - b) / 2
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
  clLinear <- if (is.null(mm$clLinear)) 0 else mm$clLinear
  q <- if (is.null(pkSet$cl2)) 0 else pkSet$cl2
  sc <- mm$saturableSC
  rb <- mm$rapidBinding
  tb <- mm$targetBinding
  # Unbound concentrations from the amounts: the amount over the volume, or,
  # with rapid binding, the root of the binding equilibrium.
  V2 <- if (is.null(pkSet$v2)) 1 else pkSet$v2
  unboundC <- if (is.null(rb)) function(a) a / V else
    function(a) unboundAtEquilibrium(a / V, rb$bmaxCentral, rb$kd)
  unboundP <- if (is.null(rb)) function(a) a / V2 else
    function(a) unboundAtEquilibrium(a / V2, rb$amaxPeripheral / V2, rb$kd)
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
    # A drug with no oral route (exenatide) has no oral depot to fill.
    orZero <- function(v, zero = 0) if (is.null(v)) zero else v
    list(ka = orZero(set$ka_PO), F = orZero(set$bioavailability_PO, 1),
         tlag = orZero(set$tlag_PO), salt = salt(f))
  })
  names(absorption) <- forms
  oralForm <- ifelse(!is.na(formulation) & formulation %in% forms, formulation,
                     "default")

  # States: the oral depots, the intramuscular depot of the prodrug, the
  # prodrug's conversion compartment, the central amount, the peripheral
  # amount and the saturable subcutaneous depot.
  nOral <- length(forms)
  iIM <- nOral + 1
  iConv <- nOral + 2
  iC <- nOral + 3
  iP <- nOral + 4
  iS <- nOral + 5
  iT <- nOral + 6      # transit compartment before the default oral depot
  iDRC <- nOral + 7    # drug-target complex, central (targetBinding)
  iDRP <- nOral + 8    # drug-target complex, peripheral
  kT <- if (is.null(mm$kaTransit)) 0 else mm$kaTransit
  # A peripheral compartment declared as rate constants on the amounts (GHB,
  # Chen 2020: k23 and k32; see the header) is the same linear distribution
  # as a PK set's cl2 and v2: q = k23 V and V2 = V k23 / k32.
  if (!is.null(mm$k23)) {
    if (q > 0)
      stop("Invalid michaelisMenten for ", PK$drug, ": a peripheral compartment ",
           "from both cl2 and k23/k32.")
    q <- mm$k23 * V
    V2 <- V * mm$k23 / mm$k32
  }
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
    # With a transit compartment the default form's doses enter it first.
    addJump(isOral, t0 + lag, ifelse(j == 1 & kT > 0, iT, j), D * Fo)
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
  # A subcutaneous dose: its zero-order share is a box of rate (scRateAt()
  # below), the rest arrives in the saturable depot when the box ends.
  isSC <- route == ROUTE_SC & !rate & !is.null(sc)
  if (any(isSC)) {
    tau <- sc$duration
    addJump(isSC, t0 + tau, rep(iS, length(t0)),
            D * salt("SC") * (1 - sc$bioavailability * (1 - sc$fraction)))
  }
  other <- !(isOral | ivBolus | peBolus | peIM | isSC | rate) |
    (rate & route != ROUTE_IV)
  if (any(other & D != 0))
    stop("The Michaelis-Menten engine cannot simulate units: ",
         paste(unique(units[other & D != 0]), collapse = ", "))

  # Two running rates, each set by its own rows until the next: the drug
  # intravenously (into the central compartment) and the prodrug (into the
  # conversion compartment).
  ivRate <- rate & !pe & route == ROUTE_IV
  peRate <- rate & pe & route == ROUTE_IV

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
  # The subcutaneous boxes add: each runs over [t0, t0 + duration).  Every
  # box edge is on the time line (the depot arrivals are jumps), so the rate
  # at the start of a step holds through it.
  scRateLine <- rep(0, L)
  for (k in which(isSC & D != 0)) {
    inBox <- timeLine >= t0[k] & timeLine < t0[k] + sc$duration
    scRateLine[inBox] <- scRateLine[inBox] + D[k] * salt("SC") *
      sc$bioavailability * (1 - sc$fraction) / sc$duration
  }

  jumpAt <- match(jumps$time, timeLine)
  if (anyNA(jumpAt[jumps$time <= maximum]))
    stop("Internal error: a dose time is missing from the time line.")

  scAbsorption <- if (is.null(sc)) function(S) 0 else
    function(S) sc$bioavailability * sc$fraction * sc$kaMax * S / (sc$kmKa + S)

  # Elimination and distribution act on the unbound concentrations.  With
  # targetBinding the central and peripheral amounts are the free drug, and
  # binding to the target (k2, saturable in the free target, Rmax - DR),
  # dissociation (koff) and hydrolysis of the complex (kdeg, which frees the
  # target) are states of their own (Landersdorfer 2012, vildagliptin).
  deriv <- function(x, rIV, rPE) {
    Cu <- unboundC(x[iC])
    Cpu <- unboundP(x[iP])
    dx <- numeric(length(x))
    out <- ka * x[seq_len(iIM)]
    dx[seq_len(iIM)] <- -out
    dx[iT] <- -kT * x[iT]
    dx[1] <- dx[1] + kT * x[iT]
    dx[iConv] <- out[iIM] + rPE - kc * x[iConv]
    fromS <- scAbsorption(x[iS])
    dx[iS] <- -fromS
    toP <- q * (Cu - Cpu)
    dx[iP] <- toP
    dx[iC] <- sum(out[seq_len(nOral)]) + kc * x[iConv] + rIV + fromS -
      vmax * Cu / (km + Cu) - clLinear * Cu - toP
    if (!is.null(tb)) {
      bindC <- (tb$rmaxCentral - x[iDRC]) * tb$k2 * Cu / (tb$kd + Cu)
      bindP <- (tb$rmaxPeripheral - x[iDRP]) * tb$k2 * Cpu / (tb$kd + Cpu)
      dx[iDRC] <- bindC - (tb$koff + tb$kdeg) * x[iDRC]
      dx[iDRP] <- bindP - (tb$koff + tb$kdeg) * x[iDRP]
      dx[iC] <- dx[iC] - bindC + tb$koff * x[iDRC]
      dx[iP] <- dx[iP] - bindP + tb$koff * x[iDRP]
    }
    dx
  }

  x <- numeric(iDRP)
  Cp <- numeric(L)
  for (i in seq_len(L)) {
    if (i > 1) {
      dt <- timeLine[i] - timeLine[i - 1]
      n <- max(1L, ceiling(dt / MM_MAX_STEP))
      h <- dt / n
      rIV <- ivRateLine[i - 1] + scRateLine[i - 1]
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
