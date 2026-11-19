# -----------------------------------------------------------------------------
# Front-end models from the literature, for teaching and for validation
# -----------------------------------------------------------------------------
# Drafted by Claude Code, 2026-11-19, at the request of Steven L. Shafer;
# extended the same day with the Northwestern dog models (Henthorn 1992,
# Krejcie 1996a, Henthorn 1999, Avram 2002), Upton 2004, Olofsen 2010 and
# Kuipers 2001.
# These are NOT library drugs: they have no drugDefaults row, no Category and
# no help page, and are not offered in the simulator.  They are the models of
# the papers the front-end engine (R/frontEndKinetics.R) was built on,
# kept so that the engine is validated against them in
# tests/testthat/test-front-end-kinetics.R, so that the Single Paper Review
# has worked examples of the shape a front-end extraction takes, and so that
# the principles can be taught.  Every number is printed in its source; where
# a number had to be inferred, the function says so and
# docs/front-end-kinetics.md records how.
# -----------------------------------------------------------------------------

#' Ketamine base per mg of ketamine hydrochloride (237.72 / 274.19)
#' @keywords internal
FRONT_END_KETAMINE_BASE_FRACTION <- 237.72 / 274.19

#' Henthorn 2018: S(+) and R(-) ketamine, front-end mixing and an arm vein
#'
#' Henthorn TK, Avram MJ, Dahan A, et al. Anesthesiology 2018; 129:260-70,
#' Table 2 (the final model).  Ten healthy men, 30-minute infusions of about
#' 7 mg of each enantiomer's hydrochloride, simultaneous radial artery and arm
#' vein samples.
#'
#' Read from the paper:
#'   VC 44.93 L, V2 136.58 L, V3 377.97 L, Cl2 3.17, Cl3 0.65 L/min (shared by
#'   the enantiomers); ClE 1.23 (S) and 1.06 (R) L/min; total pharmacokinetic
#'   flow 7.59 L/min into a 1 L unmixed volume (fixed); arm link 0.18 /min;
#'   fraction of arm blood exchanging 0.04; PCarm = V2 / VC.
#' Decided here, each checked against the paper's Figure 6
#' (docs/front-end-kinetics.md, "Henthorn 2018"):
#'   - VC 44.93 L is the S(+) enantiomer's.  The paper links the two by its
#'     eq. 9, VC,S = VC,R * ClE,S / ClE,R, without saying which it printed;
#'     Figure 6, a simulation of S(+), is reproduced with 44.93 (a free fit
#'     returns 45.2), so R(-)'s is 44.93 * 1.06 / 1.23 = 38.7 L.
#'   - The arm link is driven by the mixed central concentration (eq. 4's
#'     shunted blood carries CC); driven by arterial blood, the arm curve of
#'     Figure 6 is missed by 13% even refitted.
#'   - The parameters are in mg of ketamine BASE: Figure 6's "7 mg
#'     S(+)-ketamine hydrochloride" is reproduced with 7 x 0.867 mg (a free fit
#'     returns 0.856).  Use FRONT_END_KETAMINE_BASE_FRACTION on a salt dose.
#'     (Reading Table 2 as R(-)'s base parameters instead, with S(+)'s scaled
#'     up, misses Figure 6's arterial curve by 5.5% rather than 2.3%.)
#' Settled by the paper's Supplemental Digital Content (the PML code of the
#' final model, http://links.lww.com/ALN/B718), which estimates R(-)'s VC
#' and scales S(+)'s by ClE,S / ClE,R:
#'   - the arm link is driven by the mixed central concentration
#'     (deriv(Carmr = karm0 * (Cr - Carmr))), as Figure 6 had shown;
#'   - PCarm is V2 / VC of each enantiomer: 3.04 for S(+), 3.53 for R(-);
#'   - the arm rate constant scales with ClE like VC: Table 2's 0.18 /min is
#'     read as S(+)'s, like its VC (Figure 6 is fitted no better by 0.21,
#'     the other reading), so R(-)'s is 0.18 * 1.06 / 1.23 = 0.155.
#' The code's fixef() values (VC,R 38.76, V2 101.5, V3 98.5, Cl3 1.13,
#' total flow 8.33) are PML starting values and differ from Table 2; Table
#' 2 is used.
#'
#' @param enantiomer "S" or "R"
#' @returns a front-end model list (type "mixing")
#' @keywords internal
frontEndHenthorn2018Ketamine <- function(enantiomer = c("S", "R"))
{
  enantiomer <- match.arg(enantiomer)
  clS <- 1.23; clR <- 1.06; vcS <- 44.93
  scale <- if (enantiomer == "S") 1 else clR / clS
  cl1 <- if (enantiomer == "S") clS else clR
  v1 <- vcS * scale
  list(type = "mixing", unmixed_volume = 1, flow = 7.59,
       v1 = v1, v2 = 136.58, v3 = 377.97, cl1 = cl1, cl2 = 3.17, cl3 = 0.65,
       arm = list(k = 0.18 * scale, fraction = 0.04, partition = 136.58 / v1, driver = "central"),
       units = list(amount = "mg ketamine base", volume = "L", time = "min"),
       reference = paste0("Henthorn TK et al. Anesthesiology 2018; 129:260-70, Table 2; ",
                          enantiomer, "(", if (enantiomer == "S") "+" else "-", ") ketamine"))
}


# -----------------------------------------------------------------------------
# The Northwestern dog models (Krejcie, Avram, Henthorn)
# -----------------------------------------------------------------------------

#' The central circulation's parallel Erlang chains, Krejcie 1996a Table 4
#'
#' Krejcie TC, Henthorn TK, Niemann CU, et al. J Pharmacol Exp Ther 1996;
#' 278:1050-7 (the "general model" the later Northwestern papers cite):
#' first-pass histories fitted to two parallel Erlang functions, a fast
#' chain of 24 and a slow chain of 16 identical compartments, k the exiting
#' rate constant (/min), `fraction` the fast chain's fractional first-pass
#' area, which is its share of cardiac output (Krejcie 1996b eq. 17).  Each
#' chain's share of VC is its flow times its mean transit time n/k.  Avram
#' 2002, Henthorn 1999 and Kuipers 2001 fitted the shape per subject and did
#' not print it; these mean shapes stand in for it.
#' @keywords internal
FRONT_END_KREJCIE1996_CENTRAL <- list(
  icg        = list(fraction = 0.72, n = c(24L, 16L), k = c(92.8, 43.2)),
  antipyrine = list(fraction = 0.66, n = c(24L, 16L), k = c(86.4, 41.7)))

#' The two central chains for a central volume VC (L) of a given shape
#' @keywords internal
frontEndKrejcieCentral <- function(VC, shape = FRONT_END_KREJCIE1996_CENTRAL$antipyrine)
{
  f <- c(shape$fraction, 1 - shape$fraction)
  share <- f * shape$n / shape$k
  share <- share / sum(share)
  lapply(1:2, function(i) list(fraction = f[i], volume = VC * share[i], n = shape$n[i]))
}

#' Compartments in a nondistributive (peripheral blood) chain
#'
#' The Northwestern papers chose it per subject ("the optimum number of
#' tanks-in-series were determined iteratively", Henthorn 1999) and printed
#' none.  This one is fitted to the one dog whose thiopental history is
#' published (Avram 2002 Figure 4, the same dog as Avram 2003 Figure 3), with
#' the central chains of FRONT_END_KREJCIE1996_CENTRAL fixed: 1, 2, 3, 5, 10
#' and 20 compartments gave RMS log errors of 8.4, 10.5, 14.7, 19.3, 23.4 and
#' 24.7% (docs/front-end-kinetics.md).  One dog decides it.
#' @keywords internal
FRONT_END_ND_CHAIN <- 1L

#' Avram 2002: indocyanine green, antipyrine and thiopental in dogs
#'
#' Avram MJ, Krejcie TC, Henthorn TK.  The concordance of early antipyrine
#' and thiopental distribution kinetics.  J Pharmacol Exp Ther 2002;
#' 302:594-600, Table 2 (means of five dogs under 1.5% halothane, whole-blood
#' concentrations, ICG cardiac output 3.00 L/min).  Volumes in L, clearances
#' in L/min, read from the table:
#'   ICG         VC 0.92, VND-F 0.17 / ClND-F 1.02, VND-S 1.74 / ClND-S 1.65,
#'               ClE 0.33 (Vss 2.83)
#'   antipyrine  VC 1.07, VND 0.67 / ClND 1.09, VT-F 7.11 / ClT-F 1.12,
#'               VT-S 24.15 / ClT-S 0.66, ClE 0.13 (Vss 33.00)
#'   thiopental  VC 1.11, VND 0.70 / ClND 1.18, VT-F 14.26 / ClT-F 1.29,
#'               VT-S 37.05 / ClT-S 0.36, ClE 0.17 (Vss 53.12)
#' Elimination is from arterial blood, one of the clearances summed to
#' cardiac output.  Not printed, and taken from elsewhere: the central
#' chains' shape (FRONT_END_KREJCIE1996_CENTRAL: ICG's for ICG, antipyrine's
#' for both lipophilic markers, which the paper shows share one central
#' circulation) and the nondistributive chains' length (FRONT_END_ND_CHAIN).
#' The thiopental model is Avram and Krejcie 2003's Table 1 recirculatory
#' model.
#'
#' @param marker "thiopental", "antipyrine" or "icg"
#' @returns a front-end model list (type "recirculatory")
#' @keywords internal
frontEndAvram2002 <- function(marker = c("thiopental", "antipyrine", "icg"))
{
  marker <- match.arg(marker)
  shape <- FRONT_END_KREJCIE1996_CENTRAL
  nd <- FRONT_END_ND_CHAIN
  out <- switch(marker,
    icg = list(central = frontEndKrejcieCentral(0.92, shape$icg),
      pathways = list(list(name = "nondistributive_fast", type = "delay", flow = 1.02, volume = 0.17, n = nd),
                      list(name = "nondistributive_slow", type = "delay", flow = 1.65, volume = 1.74, n = nd)),
      elimination_clearance = 0.33),
    antipyrine = list(central = frontEndKrejcieCentral(1.07, shape$antipyrine),
      pathways = list(list(name = "nondistributive", type = "delay", flow = 1.09, volume = 0.67, n = nd),
                      list(name = "fast", type = "tissue", flow = 1.12, volume = 7.11),
                      list(name = "slow", type = "tissue", flow = 0.66, volume = 24.15)),
      elimination_clearance = 0.13),
    thiopental = list(central = frontEndKrejcieCentral(1.11, shape$antipyrine),
      pathways = list(list(name = "nondistributive", type = "delay", flow = 1.18, volume = 0.70, n = nd),
                      list(name = "fast", type = "tissue", flow = 1.29, volume = 14.26),
                      list(name = "slow", type = "tissue", flow = 0.36, volume = 37.05)),
      elimination_clearance = 0.17))
  c(list(type = "recirculatory", cardiac_output = 3.00), out,
    list(units = list(amount = "mg", volume = "L", time = "min", concentration = "whole blood"),
         reference = paste0("Avram MJ, Krejcie TC, Henthorn TK. J Pharmacol Exp Ther 2002; 302:594-600, Table 2; ",
                            marker)))
}

#' Avram and Krejcie 2003: thiopental in dogs, recirculatory and mammillary
#'
#' Avram MJ, Krejcie TC. Anesthesiology 2003; 99:1078-86, Table 1 (means of
#' five dogs under 1.5% halothane).  The recirculatory model is Avram 2002's
#' thiopental model (frontEndAvram2002("thiopental")): the same dogs.
#' Models 1-4 are Table 1's three-compartment fits (VC, VF, VS, ClF, ClS,
#' ClE): 1, to samples from 1 min; 2, from the recirculation peak; 3, with VC
#' fixed to VC + VND of the recirculatory model; 4, to a simulated 2-minute
#' infusion.
#'
#' @param model "recirculatory", "model1", "model2", "model3" or "model4"
#' @returns a front-end model list (type "recirculatory" or "mammillary"),
#'   amounts in mg, concentrations in mg/L (mcg/mL)
#' @keywords internal
frontEndAvram2003Thiopental <- function(model = c("recirculatory", "model1", "model2", "model3", "model4"))
{
  model <- match.arg(model)
  ref <- "Avram MJ, Krejcie TC. Anesthesiology 2003; 99:1078-86, Table 1 (thiopental, five dogs)"
  mam <- function(v1, vf, vs, clf, cls, cle, label)
    list(type = "mammillary", v1 = v1, v2 = vf, v3 = vs, cl1 = cle, cl2 = clf, cl3 = cls,
         reference = paste0(ref, "; ", label))
  switch(model,
    recirculatory = {
      m <- frontEndAvram2002("thiopental")
      m$reference <- paste0(ref, "; recirculatory model (", m$reference, ")")
      m
    },
    model1 = mam(3.37, 15.85, 38.58, 2.03, 0.40, 0.17, "model 1 (samples from 1 min)"),
    model2 = mam(3.04, 15.71, 38.54, 1.81, 0.38, 0.17, "model 2 (from the recirculation peak)"),
    model3 = mam(1.80, 13.69, 38.23, 1.41, 0.39, 0.17, "model 3 (VC fixed to VC + VND)"),
    model4 = mam(1.47, 13.70, 36.89, 1.28, 0.39, 0.17, "model 4 (simulated 2-min infusion)"))
}

#' The dog of Avram 2003 Figure 3 (Avram 2002 Figure 4, dog 5), as fitted
#'
#' Its first 5 minutes after 100 mg of thiopental, fitted (RMS log error
#' 8.4%) with the central chains' shape fixed to
#' FRONT_END_KREJCIE1996_CENTRAL (antipyrine) and the nondistributive chain
#' FRONT_END_ND_CHAIN long; free, its cardiac output, VC, nondistributive
#' flow and volume, and fast tissue volume (the slow tissue and ClE at Table
#' 1's means).  The paper did not print this dog's parameters; these are
#' plausible ones (cardiac output 2.0 L/min, VC 0.73 L), the source of
#' FRONT_END_ND_CHAIN, and a demonstration that the engine carries the
#' observed oscillations.
#' @keywords internal
FRONT_END_THIOPENTAL_FIGURE3_DOG <- list(cardiac_output = 2.002, VC = 0.734, Qnd = 0.726, Vnd = 0.388, Vf = 7.496)

#' @rdname FRONT_END_THIOPENTAL_FIGURE3_DOG
#' @keywords internal
frontEndThiopentalFigure3Dog <- function()
{
  d <- FRONT_END_THIOPENTAL_FIGURE3_DOG
  list(type = "recirculatory", cardiac_output = d$cardiac_output,
       central = frontEndKrejcieCentral(d$VC),
       pathways = list(
         list(name = "nondistributive", type = "delay", flow = d$Qnd, volume = d$Vnd, n = FRONT_END_ND_CHAIN),
         list(name = "fast", type = "tissue", flow = d$cardiac_output - d$Qnd - 0.36 - 0.17, volume = d$Vf),
         list(name = "slow", type = "tissue", flow = 0.36, volume = 37.05)),
       elimination_clearance = 0.17,
       reference = "Avram MJ, Krejcie TC. Anesthesiology 2003; 99:1078-86, Figure 3 (one dog), as fitted here")
}

#' Henthorn 1999: S(+) and R(-) ketamine in dogs
#'
#' Henthorn TK, Krejcie TC, Niemann CU, et al.  Ketamine distribution
#' described by a recirculatory pharmacokinetic model is not stereoselective.
#' Anesthesiology 1999; 91:1733-43, Table 2, ketamine model 4 (the one the
#' paper reports): the enantiomers share every volume and distribution
#' clearance and differ only in ClE, modelled from the fast tissue and part
#' of its clearance (Table 2's footnote: ClE "is already represented in the
#' clearance (ClT-F) to the compartment from which ClE is modeled").  Five
#' dogs under 2% halothane; ICG cardiac output 3.58 L/min.  VC 1.26
#' (pulmonary tissue 0.31 included), VND 0.48 / ClND 1.17, VT-F 6.17 / ClT-F
#' 1.54, VT-S 38.5 / ClT-S 0.86, ClE 0.66 (S) or 0.49 (R) L/min; Vss 46.4 L.
#' Central chains as antipyrine's (FRONT_END_KREJCIE1996_CENTRAL) and the
#' nondistributive chain FRONT_END_ND_CHAIN long: neither is printed.
#'
#' @param enantiomer "S" or "R"
#' @returns a front-end model list (type "recirculatory")
#' @keywords internal
frontEndHenthorn1999Ketamine <- function(enantiomer = c("S", "R"))
{
  enantiomer <- match.arg(enantiomer)
  list(type = "recirculatory", cardiac_output = 3.58,
       central = frontEndKrejcieCentral(1.26),
       pathways = list(
         list(name = "nondistributive", type = "delay", flow = 1.17, volume = 0.48, n = FRONT_END_ND_CHAIN),
         list(name = "fast", type = "tissue", flow = 1.54, volume = 6.17,
              clearance = if (enantiomer == "S") 0.66 else 0.49, clearance_in_flow = TRUE),
         list(name = "slow", type = "tissue", flow = 0.86, volume = 38.5)),
       elimination_clearance = 0,
       units = list(amount = "mg", volume = "L", time = "min"),
       reference = paste0("Henthorn TK et al. Anesthesiology 1999; 91:1733-43, Table 2, model 4; ",
                          enantiomer, "(", if (enantiomer == "S") "+" else "-", ") ketamine"))
}

#' Henthorn 1992: the minimal model of circulatory mixing of indocyanine green
#'
#' Henthorn TK, Avram MJ, Krejcie TC, Shanks CA, Asada A, Kaczynski DA.  Am J
#' Physiol 1992; 262:H903-10, Table 1 (five dogs under halothane, whole-blood
#' ICG).  Central circulation: an initial well-stirred volume, a time lag
#' (its volume is cardiac output times the lag) and the sampling volume, in
#' series; cardiac output then divides between a fast and a slow peripheral
#' circuit, each a lag and a well-stirred volume, returning to the initial
#' volume; ICG is eliminated from the slow circuit, its clearance taken out
#' of that circuit's flow (eq. 11: VS = ClS / (kE + kS1)); cardiac output =
#' ClF + ClS.  The lags were delays in SAAM 30; here each is a chain of
#' `delay_n` compartments.
#'
#' Dog 2 is the dog of Figure 3.  Simulated from its Table 1 row, ICG first
#' appears 0.25 min after injection (0.47 L / 1.89 L/min), but the paper's
#' own fitted curve in Figure 3 rises at 0.20 min: the figure's time axis
#' runs about 0.04 min ahead of the parameters (docs/front-end-kinetics.md).
#' Krejcie 1994 prints a second, different parameter set for the same dog
#' (its Figure 6), and its model curve also starts before a pure delay could.
#'
#' @param dog "mean" or 1-5
#' @param delay_n compartments in each delay chain
#' @returns a front-end model list (type "recirculatory")
#' @keywords internal
frontEndHenthorn1992ICG <- function(dog = "mean", delay_n = 50L)
{
  tab <- rbind(
    `1`  = c(0.10, 0.39, 0.15, 0.04, 0.19, 0.59, 1.04, 1.46, 1.36, 0.25),
    `2`  = c(0.16, 0.47, 0.07, 0.04, 0.10, 0.48, 0.78, 0.95, 0.94, 0.16),
    `3`  = c(0.16, 0.52, 0.15, 0.05, 0.06, 0.54, 1.14, 1.04, 1.43, 0.24),
    `4`  = c(0.25, 0.35, 0.09, 0.05, 0.12, 0.39, 0.80, 1.17, 1.34, 0.21),
    `5`  = c(0.15, 0.45, 0.13, 0.08, 0.48, 0.50, 0.93, 2.06, 0.80, 0.25),
    mean = c(0.16, 0.44, 0.12, 0.05, 0.19, 0.50, 0.94, 1.34, 1.17, 0.22))
  colnames(tab) <- c("Vinit", "VClag", "Vsamp", "VFlag", "VF", "VSlag", "VS", "ClF", "ClS", "ClE")
  dog <- as.character(dog)
  if (!(dog %in% rownames(tab))) stop("Henthorn 1992 has dogs 1-5 and their mean.")
  p <- as.list(tab[dog, ])
  list(type = "recirculatory", cardiac_output = p$ClF + p$ClS,
       central = list(list(segments = list(list(volume = p$Vinit, n = 1), list(volume = p$VClag, n = delay_n),
                                           list(volume = p$Vsamp, n = 1)))),
       pathways = list(
         list(name = "fast", type = "delay", flow = p$ClF,
              segments = list(list(volume = p$VFlag, n = delay_n), list(volume = p$VF, n = 1))),
         list(name = "slow", type = "delay", flow = p$ClS,
              segments = list(list(volume = p$VSlag, n = delay_n), list(volume = p$VS, n = 1)),
              clearance = p$ClE, clearance_in_flow = TRUE)),
       elimination_clearance = 0, elimination_in_flow = FALSE,
       units = list(amount = "mg", volume = "L", time = "min", concentration = "whole blood"),
       reference = paste0("Henthorn TK et al. Am J Physiol 1992; 262:H903-10, Table 1, ",
                          if (dog == "mean") "mean of five dogs" else paste("dog", dog)))
}

# -----------------------------------------------------------------------------
# Upton's two-compartment recirculatory model
# -----------------------------------------------------------------------------

#' Upton 2004: the two-compartment recirculatory model
#'
#' Upton RN.  The two-compartment recirculatory pharmacokinetic model - an
#' introduction to recirculatory pharmacokinetic concepts.  Br J Anaesth
#' 2004; 92:475-84.  A lung (V_L; its concentration is arterial) and a body
#' (V_B; its concentration is mixed venous) joined by cardiac output Q_CO;
#' clearance Q_Cl acts on the arterial blood entering the body (eqs. 2a-2b:
#' V_B dCv/dt = Q_CO (Ca - Cv) - Q_Cl Ca), elimination_in_flow FALSE here.
#' The engine reproduces the paper's closed forms (tests): recirculating rate
#' constants 2.2128 and 0.12051 /min, steady states R0 / Q_Cl arterial and
#' (R0 / Q_Cl)(1 - Q_Cl / Q_CO) venous, Figure 3's peak of 6.30 mg/L and
#' Figure 4's peaks.  A bolus in the paper is an infusion over its injection
#' time.
#'
#' @param example "default" (Q_CO 5, Q_Cl 2 L/min, V_L 2.5, V_B 15 L, the
#'   paper's illustrative drug) or "thiopental_sheep" (Table 3: V_L 3.07, V_B
#'   12.3 L, Q_CO 4.54, Q_Cl 1.15 L/min, fitted to Mather's sheep)
#' @param cardiac_output optional, to replace Q_CO (Figure 4 uses 3, 6, 9)
#' @returns a front-end model list (type "recirculatory")
#' @keywords internal
frontEndUpton2004 <- function(example = c("default", "thiopental_sheep"), cardiac_output = NULL)
{
  example <- match.arg(example)
  p <- switch(example, default = c(CO = 5, Cl = 2, VL = 2.5, VB = 15),
              thiopental_sheep = c(CO = 4.54, Cl = 1.15, VL = 3.07, VB = 12.3))
  CO <- cardiac_output %or% p[["CO"]]
  list(type = "recirculatory", cardiac_output = CO,
       central = list(list(fraction = 1, volume = p[["VL"]], n = 1)),
       pathways = list(list(name = "body", type = "tissue", flow = CO, volume = p[["VB"]])),
       elimination_clearance = p[["Cl"]], elimination_in_flow = FALSE,
       units = list(amount = "mg", volume = "L", time = "min"),
       reference = paste0("Upton RN. Br J Anaesth 2004; 92:475-84; ", example))
}

# -----------------------------------------------------------------------------
# Arterial and venous sampling: Olofsen 2010
# -----------------------------------------------------------------------------

#' Olofsen 2010: arterial and venous morphine-6-glucuronide
#'
#' Olofsen E, Mooren R, van Dorp E, et al.  Arterial and venous
#' pharmacokinetics of morphine-6-glucuronide and impact of sample site on
#' pharmacodynamic parameter estimates.  Anesth Analg 2010; 111:626-32, Table
#' 1 (typical values, 17 volunteers).  An arterial three-compartment model
#' (VA1 4.45, VA2 4.73, VA3 5.07 L; CLA1 0.14, CLA2 0.55, CLA3 0.07 L/min)
#' whose elimination flow passes through a venous compartment exchanging
#' with a second (Appendix: k14 = k10; VV2 1.88 L, CLV2 0.06 L/min, t1/2 kV0
#' 2.00 min), so that venous equals arterial at steady state.  Table 1's VV1
#' of 0.05 L equals CLA1 x kV0; CLA1 / kV0 = 0.404 L is the volume the
#' model's own definitions give, and it reproduces the Discussion's
#' venous-site half-lives of 1.4 and 33 min (0.05 L would give 0.17 min).
#' Amounts in mg, concentrations in mg/L.  A different arterial-venous link
#' from Henthorn 2018's arm.
#' @returns a front-end model list (type "compartmental")
#' @keywords internal
frontEndOlofsen2010M6G <- function()
{
  kV0 <- log(2) / 2.00
  list(type = "compartmental",
       compartments = list(list(name = "arterial", volume = 4.45), list(name = "arterial2", volume = 4.73),
                           list(name = "arterial3", volume = 5.07), list(name = "venous", volume = 0.14 / kV0),
                           list(name = "venous2", volume = 1.88)),
       flows = list(list(from = "arterial", to = "arterial2", clearance = 0.55),
                    list(from = "arterial2", to = "arterial", clearance = 0.55),
                    list(from = "arterial", to = "arterial3", clearance = 0.07),
                    list(from = "arterial3", to = "arterial", clearance = 0.07),
                    list(from = "arterial", to = "venous", clearance = 0.14),
                    list(from = "venous", to = "venous2", clearance = 0.06),
                    list(from = "venous2", to = "venous", clearance = 0.06),
                    list(from = "venous", to = "eliminated", clearance = 0.14)),
       dose_into = "arterial",
       observe = list(arterial = "arterial", venous = "venous"),
       units = list(amount = "mg", volume = "L", time = "min"),
       reference = "Olofsen E et al. Anesth Analg 2010; 111:626-32, Table 1 (VV1 from CLA1 / kV0)")
}

# -----------------------------------------------------------------------------
# Kuipers 2001: rocuronium in patients
# -----------------------------------------------------------------------------

#' Kuipers 2001: recirculatory rocuronium, with its effect site
#'
#' Kuipers JA, Boer F, Olofsen E, Bovill JG, Burm AG.  Recirculatory
#' pharmacokinetics and pharmacodynamics of rocuronium in patients: the
#' influence of cardiac output.  Anesthesiology 2001; 94:47-55, Tables 2, 3
#' and 6 (means of 15 patients, radial arterial whole blood, cardiac output
#' 3.86 L/min).  VC 1.52 (from ICG), VND-F 0.18 / ClND-F 1.02, VND-S 0.82 /
#' ClND-S 0.68, VT-F 4.92 / ClT-F 1.47, VT-S 9.85 / ClT-S 0.23 (L, L/min);
#' ClE 0.45 L/min from arterial blood, one of the clearances summed to
#' cardiac output; Vss 17.29 L.  Effect site (n = 13): ke0 0.129 /min (0.033
#' CO + 0.0042), Ce50 876 ng/mL (0.876 mg/L), gamma 8.75.  The dose (0.35
#' mg/kg) went into a peripheral vein, its transit folded into VC.  Means of
#' individual estimates, not a typical patient.  Not printed: the central
#' chains' shape (ICG's of FRONT_END_KREJCIE1996_CENTRAL, from dogs, stands
#' in) and the nondistributive chains' length (FRONT_END_ND_CHAIN).
#' @returns a front-end model list (type "recirculatory") with a `pd` element
#' @keywords internal
frontEndKuipers2001Rocuronium <- function()
{
  list(type = "recirculatory", cardiac_output = 3.86,
       central = frontEndKrejcieCentral(1.52, FRONT_END_KREJCIE1996_CENTRAL$icg),
       pathways = list(
         list(name = "nondistributive_fast", type = "delay", flow = 1.02, volume = 0.18, n = FRONT_END_ND_CHAIN),
         list(name = "nondistributive_slow", type = "delay", flow = 0.68, volume = 0.82, n = FRONT_END_ND_CHAIN),
         list(name = "fast", type = "tissue", flow = 1.47, volume = 4.92),
         list(name = "slow", type = "tissue", flow = 0.23, volume = 9.85)),
       elimination_clearance = 0.45,
       pd = list(ke0 = 0.129, ce50 = 0.876, gamma = 8.75, driver = "arterial"),
       units = list(amount = "mg", volume = "L", time = "min", concentration = "mg/L whole blood"),
       reference = "Kuipers JA et al. Anesthesiology 2001; 94:47-55, Tables 2, 3 and 6")
}
