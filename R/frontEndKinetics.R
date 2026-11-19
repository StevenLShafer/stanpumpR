# -----------------------------------------------------------------------------
# Front-end (recirculatory) pharmacokinetics: an exact linear engine
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-11-18, at the request of Steven L. Shafer, so
# that a paper with front-end kinetics uploaded to the Single Paper Review can
# be simulated, and so that the principles can be taught even for drugs the
# library never adopts.  The methodology is that of Henthorn, Avram and
# Krejcie; the two papers the engine was built and validated on are
#
#   Avram MJ, Krejcie TC.  Using front-end kinetics to optimize
#     target-controlled drug infusions.  Anesthesiology 2003; 99:1078-86.
#   Henthorn TK, Avram MJ, Dahan A, Gustafsson LL, Persson J, Krejcie TC,
#     Olofsen E.  Combined recirculatory-compartmental population
#     pharmacokinetic modeling of arterial and venous plasma S(+) and R(-)
#     ketamine concentrations.  Anesthesiology 2018; 129:260-70.
#
# Design, validation and open questions: docs/front-end-kinetics.md.
# Tests: tests/testthat/test-front-end-kinetics.R.
#
# WHAT IT IS FOR
# ==============
# Mammillary models put an intravenous dose into a central volume that mixes
# instantly.  It does not: the dose travels with the venous return to the
# heart, through the lungs, back to the heart and out through the arteries,
# and only then recirculates.  The arterial concentration over the first
# minutes oscillates (first pass, recirculation peak), and during an infusion
# arterial blood carries "unmixed" drug from upstream on top of the mixed,
# recirculated concentration.  Front-end models describe that.  Most of it is
# filtered away before it reaches an effect site (the brain's equilibration is
# a low-pass filter), but it decides the size of the central volume, and so
# the loading dose, the early overshoot of a target-controlled infusion, and
# the arterial-venous difference.
#
# TWO MODEL FAMILIES, ONE LINEAR SYSTEM
# =====================================
# A model is a JSON-shaped list (`spec`), so the Single Paper Review can
# extract one from a manuscript without any code being evaluated:
#
#   type = "recirculatory"   (Krejcie/Avram/Henthorn; Upton 2004 is the
#            one-chain, one-tissue case)
#     cardiac_output       total pharmacokinetic flow, L/min (the sum of all
#                          clearances from arterial blood)
#     central              the heart-lung circulation: one or more PARALLEL
#                          delay elements, each list(fraction, volume, n): a
#                          chain of n identical well-stirred compartments of
#                          total volume `volume` (L) through which `fraction`
#                          of the flow passes.  Its transit-time density is
#                          Erlang(n, n * flow / volume), the parallel Erlang
#                          functions of Krejcie 1996.  Pulmonary tissue uptake
#                          lives inside `volume` (the thiopental VC exceeds the
#                          indocyanine green VC by the lung tissue volume).
#     pathways             the peripheral circulation, in parallel, each fed
#                          from arterial blood at its `flow`:
#                            type "delay"  a nondistributive circuit, a chain
#                                          like the central ones (volume, n)
#                            type "tissue" a well-stirred, flow-limited
#                                          tissue volume (volume); optional
#                                          `clearance`, an elimination inside
#                                          the tissue (Upton's body compartment)
#     elimination_clearance  ClE, from arterial blood (Krejcie's convention)
#     elimination_in_flow  TRUE (default): ClE is one of the clearances summed
#                          to cardiac output, its blood returning drug-free;
#                          FALSE: the pathways' flows alone sum to it, and
#                          ClE is extracted from the arterial blood feeding
#                          them (Upton 2004's two-compartment model)
#   A delay element (central or "delay" pathway) may instead be a series of
#   segments, list(segments = list(list(volume, n), ...)): Henthorn 1992's
#   initial volume, time lag and sampling volume are three.  A pathway may
#   carry `clearance` from its last compartment, on top of its outflow or,
#   with clearance_in_flow = TRUE, taken out of it (Henthorn 1992 eq. 11;
#   Henthorn 1999's elimination from the fast tissue).
#   The dose (bolus or infusion) enters the venous return, ahead of the
#   central delay elements, split between them by `fraction`.
#
#   type = "mixing"         (Upton 2004's steady-state idea as fitted by
#            Henthorn 2018: a front-end mixing volume ahead of a mammillary
#            model, and Levitt's arm)
#     unmixed_volume, flow  the first-pass volume (L) and its clearance, the
#                          total pharmacokinetic flow (L/min), into...
#     v1..v3, cl1..cl3      ...the mixed central compartment of a mammillary
#                          model (cl1 elimination, cl2/cl3 intercompartmental)
#     arm                  optional list(k, fraction, partition, driver):
#                          an arm compartment linked like an effect site,
#                          dC/dt = k (C_driver - C), with the venous
#                          concentration fraction * partition * C_arm +
#                          (1 - fraction) * C_central (Henthorn 2018 eq. 4).
#                          `driver` is "central" (the default) or
#                          "arterial": Henthorn 2018's Figure 6 is
#                          reproduced only with "central" (the arm link fed
#                          by the mixed concentration, as its eq. 4's
#                          shunted fraction is); docs/front-end-kinetics.md.
#   Arterial = central + unmixed.  The dose enters the unmixed volume.
#
#   type = "mammillary"     the ordinary model in the same frame, for
#            comparison and for designing infusions (v1..v3, cl1..cl3).
#
#   type = "compartmental"  any linear model: named compartments, one-way
#            clearances between them, a dose site and named observations
#            (frontEndCompartmental(); Olofsen 2010's venous chain).
#
# Each compiles to dx/dt = A x + B r(t), with observations y = O x, where r
# holds the infusions.  An infusion's rate is constant or decays
# exponentially (rate * exp(-decay (t - start))), which is what the BET
# regimen of a target-controlled infusion is, so the inputs are themselves
# the solution of dr/dt = -D r and the augmented system
#
#     d/dt [x; r] = [[A, B], [0, -D]] [x; r]
#
# is autonomous between events.  Its exact solution over a step dt is
# expm(M dt), from expmPade() (R/advanceClosedFormGas.R), as in the other
# exact engines of this package; no numerical integrator is involved.  A
# chain of n identical compartments has one eigenvalue repeated n times,
# which is why the closed-form coefficient engines cannot carry it (see
# R/advanceTransitChain.R) and the matrix exponential can.
#
# An effect site (dCe/dt = ke0 (C_driver - Ce)) can be added to any system
# with frontEndAddEffectSite(); it is one more linear state.
# -----------------------------------------------------------------------------

#' The largest system the engine builds (states, inputs excluded)
#' @keywords internal
FRONT_END_MAX_STATES <- 400L

#' The longest delay chain accepted (Krejcie 1996 used up to 30)
#' @keywords internal
FRONT_END_MAX_CHAIN <- 100L

#' Relative tolerance on the flow balance of a recirculatory model
#' @keywords internal
FRONT_END_FLOW_TOLERANCE <- 0.01


# -----------------------------------------------------------------------------
# Building a system
# -----------------------------------------------------------------------------

#' A front-end model as a linear system
#'
#' @param spec a model list (see the header of R/frontEndKinetics.R)
#' @returns a list: \code{A} (generator), \code{entry} (where a dose lands, a
#'   vector of fractions), \code{O} (observation matrix, one named row per
#'   concentration), \code{amount} (logical, which states hold amounts, for
#'   the mass balance), \code{spec}
#' @keywords internal
frontEndSystem <- function(spec)
{
  if (!is.list(spec) || is.null(spec$type)) stop("A front-end model needs a type.")
  sys <- switch(spec$type,
    recirculatory = frontEndRecirculatory(spec),
    mixing        = frontEndMixing(spec),
    mammillary    = frontEndMammillary(spec),
    compartmental = frontEndCompartmental(spec),
    stop("Unknown front-end model type '", spec$type, "'."))
  if (nrow(sys$A) > FRONT_END_MAX_STATES)
    stop("The model has ", nrow(sys$A), " states; at most ", FRONT_END_MAX_STATES, " are simulated.")
  sys$spec <- spec
  sys
}

frontEndPositive <- function(x, what)
{
  if (!(is.numeric(x) && length(x) == 1 && is.finite(x) && x > 0))
    stop(what, " must be a finite number above 0.")
  x
}

frontEndNonNegative <- function(x, what)
{
  if (is.null(x)) return(0)
  if (!(is.numeric(x) && length(x) == 1 && is.finite(x) && x >= 0))
    stop(what, " must be a finite number of at least 0.")
  x
}

#' The compartment volumes of a delay element: list(volume, n), n identical
#' compartments, or list(segments = list(list(volume, n), ...)), segments in
#' series, each n identical compartments (n = 1 a well-stirred volume, a long
#' chain a near-discrete delay: Henthorn 1992's initial volume, time lag and
#' sampling volume in series are three segments)
#' @keywords internal
frontEndChainVolumes <- function(el, what)
{
  segs <- if (is.null(el$segments)) list(el) else el$segments
  if (!is.list(segs) || !length(segs)) stop(what, " has no segments.")
  unlist(lapply(seq_along(segs), function(j) {
    n <- frontEndChainLength(segs[[j]]$n, paste0(what, ", segment ", j, ", length"))
    rep(frontEndPositive(segs[[j]]$volume, paste0(what, ", segment ", j, ", volume")) / n, n)
  }))
}

frontEndChainLength <- function(n, what)
{
  if (is.null(n)) n <- 1
  if (!(is.numeric(n) && length(n) == 1 && is.finite(n) && n >= 1 && n == round(n) &&
        n <= FRONT_END_MAX_CHAIN))
    stop(what, " must be a whole number of compartments from 1 to ", FRONT_END_MAX_CHAIN, ".")
  as.integer(n)
}

#' An empty system with named states, grown by frontEndAddState()
#' @keywords internal
frontEndNewSystem <- function()
  list(A = matrix(0, 0, 0), names = character(0), amount = logical(0), volume = numeric(0))

frontEndAddState <- function(sys, name, amount = TRUE, volume = NA_real_)
{
  n <- length(sys$names)
  A <- matrix(0, n + 1, n + 1)
  if (n) A[seq_len(n), seq_len(n)] <- sys$A
  sys$A <- A
  sys$names <- c(sys$names, name)
  sys$amount <- c(sys$amount, amount)
  sys$volume <- c(sys$volume, volume)
  sys
}

frontEndIndex <- function(sys, name) match(name, sys$names)

#' A flow `flow` (L/min) carrying state `from` (an amount in volume
#' sys$volume[from]) into state `to` (NULL: out of the system)
#' @keywords internal
frontEndFlow <- function(sys, from, to, flow)
{
  i <- frontEndIndex(sys, from)
  k <- flow / sys$volume[i]
  sys$A[i, i] <- sys$A[i, i] - k
  if (!is.null(to)) {
    j <- frontEndIndex(sys, to)
    sys$A[j, i] <- sys$A[j, i] + k
  }
  sys
}

#' The recirculatory model (Krejcie, Avram and Henthorn)
#'
#' States, in order: each central delay element's compartments, each
#' pathway's (a chain for a delay, one compartment for a tissue), then
#' `eliminated`.  Arterial blood is not a state: it is the flow-weighted
#' mixture of the central delay elements' last compartments, and whatever
#' leaves them is handed at once to the pathways by flow and to elimination.
#' @keywords internal
frontEndRecirculatory <- function(spec)
{
  CO <- frontEndPositive(spec$cardiac_output, "Cardiac output (total pharmacokinetic flow)")
  central <- spec$central
  if (!is.list(central) || !length(central)) stop("A recirculatory model needs a central circulation.")
  if (!is.null(central$volume)) central <- list(central)
  fractions <- vapply(central, function(ch) frontEndPositive(ch$fraction %or% (1 / length(central)),
                                                               "A central delay element's fraction"), 0)
  if (abs(sum(fractions) - 1) > FRONT_END_FLOW_TOLERANCE)
    stop("The central delay elements' fractions sum to ", signif(sum(fractions), 4), ", not 1.")
  fractions <- fractions / sum(fractions)
  pathways <- spec$pathways %or% list()
  if (!is.null(pathways$type)) pathways <- list(pathways)
  ClE <- frontEndNonNegative(spec$elimination_clearance, "Elimination clearance")
  inFlow <- !identical(spec$elimination_in_flow, FALSE)
  pname <- vapply(seq_along(pathways), function(k) pathways[[k]]$name %or% paste0("pathway", k), "")
  if (anyDuplicated(pname)) stop("Two pathways share a name.")
  flows <- vapply(seq_along(pathways), function(k)
    frontEndPositive(pathways[[k]]$flow, paste0("Pathway '", pname[k], "' flow")), 0)
  total <- sum(flows) + if (inFlow) ClE else 0
  if (abs(total - CO) > FRONT_END_FLOW_TOLERANCE * CO)
    stop("The pathways' flows", if (inFlow) " and the elimination clearance" else "", " sum to ",
         signif(total, 4), " L/min, not the cardiac output of ", signif(CO, 4),
         " L/min: blood would be created or lost.")
  # Rounding in a paper's table leaves a residue within the tolerance; it
  # returns at once as a shunt of no volume, so that mass is conserved.
  shunt <- CO - total

  # The states.
  sys <- frontEndNewSystem()
  chains <- list()                     # name -> list(states, flow)
  addChain <- function(prefix, volumes, flow) {
    nm <- if (length(volumes) == 1) prefix else paste0(prefix, "[", seq_along(volumes), "]")
    for (j in seq_along(nm)) sys <<- frontEndAddState(sys, nm[j], TRUE, volumes[j])
    chains[[prefix]] <<- list(states = nm, flow = flow)
  }
  for (i in seq_along(central))
    addChain(paste0("central", i), frontEndChainVolumes(central[[i]], "A central delay element"), fractions[i] * CO)
  tissue <- character(0)
  clearance <- numeric(0); clearanceInFlow <- logical(0)
  for (k in seq_along(pathways)) {
    p <- pathways[[k]]
    what <- paste0("Pathway '", pname[k], "'")
    type <- p$type %or% ""
    if (identical(type, "delay")) {
      addChain(pname[k], frontEndChainVolumes(p, what), flows[k])
    } else if (identical(type, "tissue")) {
      addChain(pname[k], frontEndPositive(p$volume, paste0(what, " volume")), flows[k])
      tissue <- c(tissue, pname[k])
    } else stop(what, " has unknown type '", type, "'.")
    clearance[pname[k]] <- frontEndNonNegative(p$clearance, paste0(what, " clearance"))
    clearanceInFlow[pname[k]] <- isTRUE(p$clearance_in_flow)
    if (clearanceInFlow[pname[k]] && clearance[pname[k]] >= flows[k])
      stop(what, " clearance taken from its flow must be below the flow.")
  }
  sys <- frontEndAddState(sys, "eliminated", TRUE, NA_real_)
  idx <- function(s) frontEndIndex(sys, s)
  nState <- length(sys$names)
  A <- matrix(0, nState, nState)
  # Flow through each chain: every compartment empties at flow / its volume.
  # Within a chain it passes to the next compartment; the last compartments'
  # outflows are routed below.
  for (nm in names(chains)) {
    st <- chains[[nm]]$states
    for (j in seq_along(st)) {
      a <- idx(st[j])
      A[a, a] <- A[a, a] - chains[[nm]]$flow / sys$volume[a]
      if (j < length(st)) A[idx(st[j + 1]), a] <- A[idx(st[j + 1]), a] + chains[[nm]]$flow / sys$volume[a]
    }
  }
  # Arterial concentration, Ca = aRow . x
  aRow <- numeric(nState)
  for (i in seq_along(central)) {
    last <- idx(tail(chains[[paste0("central", i)]]$states, 1))
    aRow[last] <- fractions[i] / sys$volume[last]
  }
  # Arterial blood feeds every pathway at its flow, and elimination at ClE.
  # When ClE is not one of the flows (elimination_in_flow FALSE), it is taken
  # out of the arterial blood on its way to the pathways, in proportion to
  # their flows (Upton 2004 eq. 2b: the body receives Q_CO Ca - Q_Cl Ca);
  # drug is neither created nor lost.
  extracted <- if (inFlow) 1 else 1 - ClE / CO
  if (extracted <= 0) stop("The elimination clearance must be below the cardiac output.")
  for (k in seq_along(pathways)) {
    head <- idx(chains[[pname[k]]]$states[1])
    A[head, ] <- A[head, ] + extracted * flows[k] * aRow
  }
  e <- idx("eliminated")
  A[e, ] <- A[e, ] + ClE * aRow
  # Venous return (each pathway's outflow, and the shunt) enters the central
  # delay elements' heads by fraction.
  # Elimination inside a pathway, from its last compartment: on top of the
  # outflow (Upton's body compartment), or taken out of it (clearance_in_flow:
  # the eliminating organ's venous blood leaves cleared of drug, as Henthorn
  # 1992 modelled indocyanine green from the slow circuit, eq. 11).
  venous <- numeric(nState)
  for (k in seq_along(pathways)) {
    last <- idx(tail(chains[[pname[k]]]$states, 1))
    cl <- clearance[[pname[k]]]
    out <- if (clearanceInFlow[[pname[k]]]) flows[k] - cl else flows[k]
    venous[last] <- out / sys$volume[last]
    if (cl > 0) {
      if (!clearanceInFlow[[pname[k]]]) A[last, last] <- A[last, last] - cl / sys$volume[last]
      A[e, last] <- A[e, last] + cl / sys$volume[last]
    }
  }
  venous <- venous + shunt * aRow
  for (i in seq_along(central)) {
    head <- idx(chains[[paste0("central", i)]]$states[1])
    A[head, ] <- A[head, ] + fractions[i] * venous
  }

  entry <- numeric(nState)
  for (i in seq_along(central)) entry[idx(chains[[paste0("central", i)]]$states[1])] <- fractions[i]
  # Observations, as concentrations (an amount in mg over litres is mg/L,
  # which is mcg/mL).  Mixed venous blood is the venous return over the
  # total flow (blood cleared of drug still returns).  A tissue's
  # concentration is its plasma-apparent one.
  O <- rbind(arterial = aRow, mixed_venous = venous / CO)
  for (nm in tissue) {
    r <- numeric(nState); t <- idx(nm); r[t] <- 1 / sys$volume[t]
    O <- rbind(O, r); rownames(O)[nrow(O)] <- nm
  }
  list(A = A, entry = entry, O = O, amount = sys$amount, states = sys$names)
}

#' The front-end mixing model (Henthorn 2018; after Upton 2004)
#' @keywords internal
frontEndMixing <- function(spec)
{
  Vu <- frontEndPositive(spec$unmixed_volume %or% 1, "The unmixed volume")
  Q <- frontEndPositive(spec$flow, "The total pharmacokinetic flow")
  v1 <- frontEndPositive(spec$v1, "V1")
  cl1 <- frontEndPositive(spec$cl1, "The elimination clearance")
  v2 <- if (is.null(spec$v2)) NULL else frontEndPositive(spec$v2, "V2")
  v3 <- if (is.null(spec$v3)) NULL else frontEndPositive(spec$v3, "V3")
  sys <- frontEndNewSystem()
  sys <- frontEndAddState(sys, "unmixed", TRUE, Vu)
  sys <- frontEndAddState(sys, "central", TRUE, v1)
  if (!is.null(v2)) sys <- frontEndAddState(sys, "peripheral2", TRUE, v2)
  if (!is.null(v3)) sys <- frontEndAddState(sys, "peripheral3", TRUE, v3)
  sys <- frontEndAddState(sys, "eliminated", TRUE, NA_real_)
  sys <- frontEndFlow(sys, "unmixed", "central", Q)
  sys <- frontEndFlow(sys, "central", "eliminated", cl1)
  for (p in c(2, 3)) {
    v <- if (p == 2) v2 else v3
    if (is.null(v)) next
    cl <- frontEndPositive(spec[[paste0("cl", p)]], paste0("Cl", p))
    sys <- frontEndFlow(sys, "central", paste0("peripheral", p), cl)
    sys <- frontEndFlow(sys, paste0("peripheral", p), "central", cl)
  }
  conc <- function(name) { r <- numeric(length(sys$names)); i <- frontEndIndex(sys, name); r[i] <- 1 / sys$volume[i]; r }
  arm <- spec$arm
  if (!is.null(arm)) {
    k <- frontEndPositive(arm$k, "The arm link rate constant")
    f <- frontEndNonNegative(arm$fraction, "The arm's exchanging fraction")
    if (f > 1) stop("The arm's exchanging fraction must be at most 1.")
    pc <- frontEndPositive(arm$partition %or% 1, "The arm partition coefficient")
    driver <- arm$driver %or% "central"
    if (!driver %in% c("arterial", "central")) stop("The arm's driver must be 'arterial' or 'central'.")
    sys <- frontEndAddState(sys, "arm", FALSE, NA_real_)
    i <- frontEndIndex(sys, "arm")
    drive <- conc("central") + if (driver == "arterial") conc("unmixed") else 0
    sys$A[i, ] <- sys$A[i, ] + k * drive
    sys$A[i, i] <- sys$A[i, i] - k
  }
  nState <- length(sys$names)
  O <- rbind(arterial = conc("central") + conc("unmixed"), central = conc("central"),
             unmixed = conc("unmixed"))
  if (!is.null(arm)) {
    r <- numeric(nState); r[frontEndIndex(sys, "arm")] <- 1
    O <- rbind(O, venous = f * pc * r + (1 - f) * conc("central"), arm = r, arm_tissue = pc * r)
  }
  entry <- numeric(nState); entry[frontEndIndex(sys, "unmixed")] <- 1
  list(A = sys$A, entry = entry, O = O, amount = sys$amount, states = sys$names)
}

#' The ordinary mammillary model in the same frame
#' @keywords internal
frontEndMammillary <- function(spec)
{
  v1 <- frontEndPositive(spec$v1, "V1")
  cl1 <- frontEndPositive(spec$cl1, "The elimination clearance")
  sys <- frontEndNewSystem()
  sys <- frontEndAddState(sys, "central", TRUE, v1)
  for (p in c(2, 3)) {
    v <- spec[[paste0("v", p)]]
    if (is.null(v)) next
    sys <- frontEndAddState(sys, paste0("peripheral", p), TRUE, frontEndPositive(v, paste0("V", p)))
  }
  sys <- frontEndAddState(sys, "eliminated", TRUE, NA_real_)
  sys <- frontEndFlow(sys, "central", "eliminated", cl1)
  for (p in c(2, 3)) {
    if (is.null(spec[[paste0("v", p)]])) next
    cl <- frontEndPositive(spec[[paste0("cl", p)]], paste0("Cl", p))
    sys <- frontEndFlow(sys, "central", paste0("peripheral", p), cl)
    sys <- frontEndFlow(sys, paste0("peripheral", p), "central", cl)
  }
  nState <- length(sys$names)
  O <- matrix(0, 1, nState, dimnames = list("central", NULL))
  O[1, 1] <- 1 / v1
  entry <- numeric(nState); entry[1] <- 1
  list(A = sys$A, entry = entry, O = O, amount = sys$amount, states = sys$names)
}

#' Any linear compartmental model, for papers whose structure is neither of
#' the two families (Olofsen 2010's arterial model feeding a venous chain;
#' Krejcie 1994's tissues fed from two blood circuits)
#'
#' spec: compartments = list(list(name, volume), ...); flows = list(list(from,
#' to, clearance), ...), each one-way (an exchange is two flows; `to` NULL or
#' "eliminated" removes drug); dose_into = a compartment name, or a named
#' vector of fractions; observe = a named list, each a compartment name (its
#' concentration) or a named vector of weights on compartment concentrations.
#' @keywords internal
frontEndCompartmental <- function(spec)
{
  comps <- spec$compartments
  if (!is.list(comps) || !length(comps)) stop("A compartmental model needs compartments.")
  nm <- vapply(comps, function(cmp) as.character(cmp$name %or% ""), "")
  if (any(!nzchar(nm)) || anyDuplicated(nm) || "eliminated" %in% nm) stop("Compartments need distinct names.")
  sys <- frontEndNewSystem()
  for (j in seq_along(comps))
    sys <- frontEndAddState(sys, nm[j], TRUE, frontEndPositive(comps[[j]]$volume, paste0("Compartment '", nm[j], "' volume")))
  sys <- frontEndAddState(sys, "eliminated", TRUE, NA_real_)
  for (f in spec$flows %or% list()) {
    if (!(f$from %in% nm)) stop("A flow leaves an unknown compartment '", f$from, "'.")
    to <- if (is.null(f$to) || identical(f$to, "eliminated")) "eliminated" else f$to
    if (!(to %in% c(nm, "eliminated"))) stop("A flow enters an unknown compartment '", to, "'.")
    sys <- frontEndFlow(sys, f$from, to, frontEndPositive(f$clearance, paste0("The clearance from '", f$from, "'")))
  }
  nState <- length(sys$names)
  weights <- function(w, what) {
    if (is.character(w) && length(w) == 1) w <- stats::setNames(1, w)
    if (is.null(names(w)) || !all(names(w) %in% nm)) stop(what, " names an unknown compartment.")
    r <- numeric(nState); r[match(names(w), sys$names)] <- as.numeric(w); r
  }
  entry <- weights(spec$dose_into %or% nm[1], "The dose")
  if (abs(sum(entry) - 1) > 1e-9) stop("The dose fractions must sum to 1.")
  obs <- spec$observe %or% stats::setNames(list(nm[1]), nm[1])
  O <- do.call(rbind, lapply(names(obs), function(o) {
    r <- weights(obs[[o]], paste0("Observation '", o, "'"))
    r / ifelse(is.na(sys$volume), 1, sys$volume)
  }))
  rownames(O) <- names(obs)
  list(A = sys$A, entry = entry, O = O, amount = sys$amount, states = sys$names)
}

#' Add an effect site to a system
#'
#' @param sys a frontEndSystem()
#' @param ke0 the rate constant, 1/min
#' @param driver the observation that drives it ("arterial" by default)
#' @param name the observation's name
#' @returns the system, with one more state and observation
#' @keywords internal
frontEndAddEffectSite <- function(sys, ke0, driver = "arterial", name = "effect")
{
  frontEndPositive(ke0, "ke0")
  if (!driver %in% rownames(sys$O)) stop("No observation '", driver, "' to drive the effect site.")
  n <- nrow(sys$A)
  A <- matrix(0, n + 1, n + 1); A[seq_len(n), seq_len(n)] <- sys$A
  A[n + 1, seq_len(n)] <- ke0 * sys$O[driver, ]
  A[n + 1, n + 1] <- -ke0
  sys$A <- A
  sys$O <- cbind(sys$O, 0)
  r <- numeric(n + 1); r[n + 1] <- 1
  sys$O <- rbind(sys$O, r); rownames(sys$O)[nrow(sys$O)] <- name
  sys$entry <- c(sys$entry, 0)
  sys$amount <- c(sys$amount, FALSE)
  sys$states <- c(sys$states, name)
  sys
}


# -----------------------------------------------------------------------------
# Doses and the solution
# -----------------------------------------------------------------------------

#' A dose list for frontEndSimulate()
#'
#' @param bolus a data frame of time (min) and amount (mg), or NULL
#' @param infusion a data frame of start, end (min), rate (mg/min) and
#'   optionally decay (1/min; the rate is rate * exp(-decay (t - start))),
#'   or NULL
#' @keywords internal
frontEndDoses <- function(bolus = NULL, infusion = NULL)
{
  if (is.null(bolus)) bolus <- data.frame(time = numeric(0), amount = numeric(0))
  if (is.null(infusion)) infusion <- data.frame(start = numeric(0), end = numeric(0), rate = numeric(0))
  if (is.null(infusion$decay)) infusion$decay <- rep(0, nrow(infusion))
  ok <- function(x) all(is.finite(x))
  if (!ok(bolus$time) || !ok(bolus$amount) || any(bolus$time < 0))
    stop("Every bolus needs a finite time (at least 0) and amount.")
  if (!ok(infusion$start) || !ok(infusion$end) || !ok(infusion$rate) || !ok(infusion$decay) ||
      any(infusion$start < 0) || any(infusion$end < infusion$start) || any(infusion$decay < 0))
    stop("Every infusion needs a finite start (at least 0), an end after it, a rate and a decay of at least 0.")
  list(bolus = bolus, infusion = infusion)
}

#' Simulate a front-end system
#'
#' Exact: between events the augmented system is advanced by its matrix
#' exponential, cached on the step length, so a uniform grid costs one
#' exponential per distinct step.
#'
#' @param sys a frontEndSystem(), with any effect sites added
#' @param doses a frontEndDoses() list
#' @param times the output times, minutes (sorted or not; at least 0)
#' @returns a data frame: time, then one column per observation, then
#'   \code{body} (amount in the body) and \code{dosed} (amount given so far),
#'   whose difference less \code{eliminated} is the mass balance error
#' @keywords internal
frontEndSimulate <- function(sys, doses, times)
{
  times <- sort(unique(as.numeric(times)))
  if (!length(times) || any(!is.finite(times)) || any(times < 0)) stop("Output times must be finite and at least 0.")
  inf <- doses$infusion
  bol <- doses$bolus
  # One input state for all constant infusions (their rates add), one each
  # for decaying ones.
  constant <- inf$decay == 0
  decaying <- which(!constant)
  nIn <- 1 + length(decaying)
  n <- nrow(sys$A)
  M <- matrix(0, n + nIn, n + nIn)
  M[seq_len(n), seq_len(n)] <- sys$A
  M[seq_len(n), n + seq_len(nIn)] <- sys$entry
  for (j in seq_along(decaying)) M[n + 1 + j, n + 1 + j] <- -inf$decay[decaying[j]]
  # Events: (time, kind, index).  At a time, infusions end before others start,
  # and boluses are given, before the output is read.
  ev <- rbind(
    data.frame(time = inf$end, order = rep(1, nrow(inf)), kind = rep("stop", nrow(inf)), i = seq_len(nrow(inf))),
    data.frame(time = inf$start, order = rep(2, nrow(inf)), kind = rep("start", nrow(inf)), i = seq_len(nrow(inf))),
    data.frame(time = bol$time, order = rep(3, nrow(bol)), kind = rep("bolus", nrow(bol)), i = seq_len(nrow(bol))))
  ev <- ev[order(ev$time, ev$order), , drop = FALSE]
  cache <- new.env(parent = emptyenv())
  step <- function(z, dt) {
    if (dt <= 0) return(z)
    key <- sprintf("%.12g", dt)
    E <- cache[[key]]
    if (is.null(E)) { E <- expmPade(M * dt); cache[[key]] <- E }
    drop(E %*% z)
  }
  inputSlot <- function(i) if (constant[i]) n + 1 else n + 1 + match(i, decaying)
  z <- numeric(n + nIn)
  t <- 0
  dosed <- 0
  out <- matrix(NA_real_, length(times), nrow(sys$O) + 2)
  stops <- sort(unique(c(times, ev$time)))
  e <- 1; k <- 1
  dosedAt <- function(tt) {
    # Amount given by time tt: boluses plus the integral of every infusion.
    b <- sum(bol$amount[bol$time <= tt])
    s <- 0
    for (i in seq_len(nrow(inf))) {
      a <- inf$start[i]; b2 <- min(inf$end[i], tt)
      if (b2 <= a) next
      s <- s + if (inf$decay[i] == 0) inf$rate[i] * (b2 - a) else
        inf$rate[i] * (1 - exp(-inf$decay[i] * (b2 - a))) / inf$decay[i]
    }
    b + s
  }
  amountIdx <- which(sys$amount & sys$states != "eliminated")
  elimIdx <- which(sys$states == "eliminated")
  for (s in stops)
  {
    z <- step(z, s - t); t <- s
    while (e <= nrow(ev) && ev$time[e] <= s + 1e-12) {
      i <- ev$i[e]
      if (ev$kind[e] == "stop") {
        if (inf$start[i] < inf$end[i]) {
          slot <- inputSlot(i)
          z[slot] <- if (constant[i]) z[slot] - inf$rate[i] else 0
        }
      } else if (ev$kind[e] == "start") {
        if (inf$start[i] < inf$end[i]) {
          slot <- inputSlot(i)
          z[slot] <- z[slot] + inf$rate[i]
        }
      } else {
        z[seq_len(n)] <- z[seq_len(n)] + bol$amount[i] * sys$entry
      }
      e <- e + 1
    }
    if (abs(z[n + 1]) < 1e-13 * max(1, abs(inf$rate))) z[n + 1] <- 0
    while (k <= length(times) && abs(times[k] - s) <= 1e-12) {
      x <- z[seq_len(n)]
      out[k, ] <- c(drop(sys$O %*% x), sum(x[amountIdx]) + if (length(elimIdx)) x[elimIdx] else 0,
                    dosedAt(s))
      k <- k + 1
    }
  }
  res <- data.frame(time = times, out)
  names(res) <- c("time", rownames(sys$O), "body_plus_eliminated", "dosed")
  res
}


# -----------------------------------------------------------------------------
# Helpers for teaching and validation
# -----------------------------------------------------------------------------

#' Pharmacokinetic flow from cardiac output (Stec 1979)
#'
#' Plasma concentrations are carried by blood whose red cells hold drug too,
#' so the flow that clears plasma is cardiac output times the blood:plasma
#' ratio, (1 - Hct) + Hct * E:P.
#'
#' @param cardiacOutput L/min
#' @param hematocrit fraction
#' @param erythrocytePlasma the red cell:plasma partition ratio (0 for
#'   indocyanine green, 1 for antipyrine)
#' @returns L/min
#' @keywords internal
frontEndPkFlow <- function(cardiacOutput, hematocrit, erythrocytePlasma)
  cardiacOutput * ((1 - hematocrit) + hematocrit * erythrocytePlasma)

#' The BET regimen for a mammillary model (Schwilden 1981)
#'
#' A bolus of target * V1, and infusions target * Cl1, target * Cl2
#' exp(-Cl2/V2 t) and target * Cl3 exp(-Cl3/V3 t), which hold the model's
#' central concentration at the target exactly.
#'
#' @param target the target concentration (mg/L)
#' @param pk a list of v1..v3, cl1..cl3 (v2/v3 may be absent)
#' @param duration minutes
#' @param start minutes
#' @returns a frontEndDoses() list
#' @keywords internal
frontEndBET <- function(target, pk, duration, start = 0)
{
  inf <- data.frame(start = start, end = start + duration, rate = target * pk$cl1, decay = 0)
  for (p in c(2, 3)) {
    v <- pk[[paste0("v", p)]]
    if (is.null(v)) next
    cl <- pk[[paste0("cl", p)]]
    inf <- rbind(inf, data.frame(start = start, end = start + duration, rate = target * cl, decay = cl / v))
  }
  frontEndDoses(bolus = data.frame(time = start, amount = target * pk$v1), infusion = inf)
}

#' The area between a concentration history and a target (trapezoids)
#'
#' Signed, as Avram and Krejcie's "infusion delta AUC": above the target
#' counts positive.
#'
#' @keywords internal
frontEndAreaFromTarget <- function(time, conc, target)
{
  d <- conc - target
  sum(diff(time) * (head(d, -1) + tail(d, -1)) / 2)
}

#' Fit a mammillary model to concentrations, as a conventional analysis would
#'
#' Least squares on log concentrations (a proportional error), from start
#' values, through optim().  For the validation of the papers' claims that
#' the conventional fit's central volume depends on where and when blood is
#' sampled; not a population method.
#'
#' @param time,conc the observations (min, mg/L)
#' @param doses a frontEndDoses() list
#' @param start a list of v1..v3, cl1..cl3
#' @param fixed names of parameters held at their start values
#' @returns the fitted list of v1..v3, cl1..cl3, with \code{value} the
#'   residual sum of squares
#' @keywords internal
frontEndFitMammillary <- function(time, conc, doses, start, fixed = character(0))
{
  keep <- conc > 0 & is.finite(conc)
  time <- time[keep]; conc <- conc[keep]
  names0 <- c("v1", "v2", "v3", "cl1", "cl2", "cl3")
  names0 <- names0[names0 %in% names(start)]
  free <- setdiff(names0, fixed)
  par0 <- log(unlist(start[free]))
  pkOf <- function(p) { pk <- start; pk[free] <- as.list(exp(p)); pk$type <- "mammillary"; pk }
  obj <- function(p) {
    pk <- pkOf(p)
    sim <- tryCatch(frontEndSimulate(frontEndSystem(pk), doses, time), error = function(e) NULL)
    if (is.null(sim)) return(1e10)
    pred <- sim$central[match(time, sim$time)]
    if (any(!is.finite(pred) | pred <= 0)) return(1e10)
    sum((log(conc) - log(pred))^2)
  }
  fit <- stats::optim(par0, obj, method = "Nelder-Mead", control = list(maxit = 4000, reltol = 1e-10))
  fit <- stats::optim(fit$par, obj, method = "BFGS", control = list(maxit = 500, reltol = 1e-12))
  out <- pkOf(fit$par); out$type <- NULL
  out$value <- fit$value
  out
}
