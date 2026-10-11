# -----------------------------------------------------------------------------
# Antipsychotic profiles: provenance, route basis and pharmacodynamic endpoints
# -----------------------------------------------------------------------------
# The antipsychotic drug files (drugs_quetiapine.R and the rest) give the
# engine its pharmacokinetics.  What the engine has no place for is recorded
# here, one entry per named profile:
#
#   - which route and formulation the parameters describe, and whether they
#     are apparent (divided by an unmeasured F) or absolute;
#   - the source population, analyte and matrix (serum or plasma);
#   - how much of the transcription has been checked against primary text;
#   - the pharmacodynamic endpoint, if any, with its driver and its PAIRED
#     Emax and EC50.
#
# None of these endpoints is plotted.  The Effect Site trace is a
# concentration (only aripiprazole has one, from a PET ke0), and the MEAC and
# band are zero for every antipsychotic: a D2 occupancy is a receptor
# biomarker, not a dosing target, and no validated target is specified.
# antipsychoticOccupancy() converts a concentration the caller has simulated
# into occupancy with a named fit, and refuses an endpoint that is not an
# occupancy.
#
# Two profiles are registered here but NOT in the drug library, because no
# complete primary parameter vector is available:
#   - quetiapineXR: Brogren & Nyberg 2010 (PAGE abstract) gives CL/F and the
#     mean transit times but not the transit number, Q/F or volumes.
#   - chlorpromazine: Chetty 1994's accessible report has no structural or
#     coefficient table; Yeung 1993's summary statistics are not a population
#     model.  No typical vector is invented.
#
# Each fit's `verified` says whether its numbers were read in primary text
# (TRUE) or are the implementation brief's and still need checking (FALSE).
# -----------------------------------------------------------------------------

# Free-base molecular weights, g/mol, for nmol/L <-> ng/mL
QUETIAPINE_MW <- 383.5

#' One concentration-driven D2 occupancy fit
#' @noRd
occupancyFit <- function(name, driver, Emax, EC50, source, verified,
                         ke0 = NA_real_, note = "")
{
  list(endpoint = "D2_occupancy", name = name, driver = driver,
       Emax = Emax, EC50 = EC50, ke0 = ke0, source = source,
       verified = verified, note = note)
}

#' Registry of antipsychotic profiles
#'
#' One entry per named profile.  `drug` is the drug-library name, or `NA` for a
#' profile that is registered as research-gated and has no parameters.  EC50
#' values are ng/mL of the named driver; ke0 is per minute.  A function rather
#' than a constant because it reads constants from files that load after this
#' one.
#'
#' @returns a named list
#' @export
antipsychoticProfiles <- function() list(
  quetiapine = list(
    drug = "quetiapine", route = ROUTE_PO, formulation = "immediate release",
    parameterBasis = "apparent_oral",
    sourcePopulation = "99 Chinese inpatients with bipolar affective disorder",
    analyte = "quetiapine", matrix = "plasma",
    pkSource = "Zheng 2024, doi:10.3389/fpsyt.2024.1497119",
    modelStatus = "implemented", pkVerified = TRUE,
    warnings = c("Immediate release only; extended release is not this model.",
                 "Occupancy fit is from healthy men, PK from bipolar patients."),
    pd = list(
      occupancyFit("Nord 2011", "parent_plasma", Emax = 100,
                   EC50 = 1369 * QUETIAPINE_MW / 1000,   # 1369 nmol/L
                   source = "Nord 2011, doi:10.1017/S1461145711000514",
                   verified = TRUE,
                   note = "Striatal PET, 11 healthy men, IR and XR pooled; Emax fixed.")
    )
  ),

  quetiapineXR = list(
    drug = NA_character_, route = ROUTE_PO, formulation = "extended release",
    parameterBasis = "apparent_oral",
    sourcePopulation = "58 healthy volunteers",
    analyte = "quetiapine", matrix = "plasma",
    pkSource = "Brogren & Nyberg 2010, PAGE abstract 1686",
    modelStatus = "blocked", pkVerified = FALSE,
    warnings = "Transit number, Q/F and volumes not published; not registered.",
    pd = list()
  ),

  risperidone = list(
    drug = "risperidone", route = ROUTE_PO, formulation = "oral",
    parameterBasis = "apparent_oral",
    sourcePopulation = "512 genotyped adults in therapeutic drug monitoring",
    analyte = c("risperidone", "9-hydroxyrisperidone"), matrix = "serum",
    pkSource = "Storset 2024, doi:10.1007/s00228-024-03721-6",
    modelStatus = "implemented", pkVerified = FALSE,
    warnings = c("V/F, Vm and CLm not yet checked against Table 2.",
                 "Serum PK; the occupancy fits were calibrated in plasma.",
                 "Mostly 9-30 h samples: early peaks are poorly informed.",
                 "Ultrarapid CYP2D6 is an extrapolation; NFIB not applied."),
    pd = list(
      occupancyFit("Uchida 2011, unconstrained", "active_moiety", Emax = 88,
                   EC50 = 4.9,
                   source = "Uchida 2011 (PMID 21508857), as quoted by Sakurai 2013",
                   verified = FALSE,
                   note = "Driver is risperidone + 9-hydroxyrisperidone, ng/mL."),
      occupancyFit("Uchida 2011, Emax fixed", "active_moiety", Emax = 100,
                   EC50 = 8.2,
                   source = "Uchida 2011, as used by Lindauer 2025 (doi:10.1002/jcph.6152)",
                   verified = TRUE,
                   note = "Driver is risperidone + 9-hydroxyrisperidone, ng/mL.")
    )
  ),

  aripiprazole = list(
    drug = "aripiprazole", route = ROUTE_PO, formulation = "oral",
    parameterBasis = "apparent_oral",
    sourcePopulation = "80 Korean psychiatric patients at steady state",
    analyte = c("aripiprazole", "dehydroaripiprazole"), matrix = "plasma",
    pkSource = "Kim JR 2008, doi:10.1111/j.1365-2125.2008.03223.x",
    modelStatus = "implemented", pkVerified = FALSE,
    warnings = c("Genotype-class CL/F, CLm/fm, Vm/fm and the formation convention not yet checked against full text.",
                 "PK and PD fitted in different populations (patients; healthy men).",
                 "Poor and ultrarapid CYP2D6 metabolisers were not studied."),
    pd = list(
      occupancyFit("Kim 2012", "parent_effect_site", Emax = 100, EC50 = 8.63,
                   ke0 = 0.725 / 60,
                   source = "Kim E 2012, doi:10.1038/jcbfm.2011.180",
                   verified = FALSE,
                   note = paste("EC50 checked in the abstract; ke0 and Emax not.",
                                "Parent only: do not add dehydroaripiprazole."))
    )
  ),

  olanzapine = list(
    drug = "olanzapine", route = ROUTE_PO, formulation = "oral",
    parameterBasis = "apparent_oral",
    sourcePopulation = "601 healthy subjects and patients with schizophrenia",
    analyte = "olanzapine", matrix = "plasma",
    pkSource = "Sun 2021, doi:10.1002/jcph.1911",
    modelStatus = "implemented", pkVerified = TRUE,
    warnings = c("Nonsmoker, fasted, no interacting drugs: smoking raises CL/F 30%.",
                 "Power form of the age term on Vc/F not confirmed."),
    pd = list(
      occupancyFit("Kapur 1998", "parent_plasma", Emax = 100, EC50 = 10.3,
                   source = "Kapur 1998, doi:10.1176/ajp.155.7.921",
                   verified = FALSE,
                   note = "The abstract reports occupancy by dose only; EC50 unchecked.")
    )
  ),

  haloperidol = list(
    drug = "haloperidol", route = ROUTE_PO, formulation = "oral",
    parameterBasis = "apparent_oral",
    sourcePopulation = "122 patients with schizophrenia",
    analyte = "haloperidol", matrix = "plasma",
    pkSource = "Pilla Reddy 2013, doi:10.1097/JCP.0b013e3182a4ee2c",
    modelStatus = "implemented", pkVerified = FALSE,
    warnings = "Parameters not yet checked against the full text.",
    pd = list(
      list(endpoint = "PANSS", name = "Pilla Reddy 2013", driver = "Css",
           Emax = 0.31, EC50 = 3.58, ke0 = NA_real_,
           source = "Pilla Reddy 2013", verified = FALSE,
           note = paste("Drug term on average steady-state concentration inside",
                        "an untranscribed Weibull placebo and dropout model;",
                        "not computed."))
    )
  ),

  haloperidolIV = list(
    drug = "haloperidolIV", route = ROUTE_IV, formulation = "intravenous",
    parameterBasis = "absolute_iv",
    sourcePopulation = "22 critically ill adults treated for delirium",
    analyte = "haloperidol", matrix = "plasma",
    pkSource = "Li 2022, doi:10.3390/pharmaceutics14030549",
    modelStatus = "implemented", pkVerified = TRUE,
    warnings = c("CRP covariate not applied (function in an unread supplement).",
                 "One compartment: no early distribution phase."),
    pd = list()
  ),

  droperidolIM = list(
    drug = "droperidolIM", route = ROUTE_IM, formulation = "intramuscular",
    parameterBasis = "apparent_im",
    sourcePopulation = "41 acutely agitated patients",
    analyte = "droperidol", matrix = "serum",
    pkSource = "Foo 2016, doi:10.1111/bcp.13093",
    modelStatus = "implemented", pkVerified = FALSE,
    warnings = c("Q/F and Vp/F not checked in primary text.",
                 "IM bioavailability unmeasured: parameters are apparent."),
    pd = list()
  ),

  droperidol = list(
    drug = "droperidol", route = ROUTE_IV, formulation = "intravenous",
    parameterBasis = "absolute_iv",
    sourcePopulation = "7 healthy men",
    analyte = "droperidol", matrix = "plasma",
    pkSource = "Cooper 2018, doi:10.1177/2050312118813283",
    modelStatus = "implemented_with_warning", pkVerified = TRUE,
    unobservedMinutes = DROPERIDOL_UNOBSERVED_MIN,
    warnings = c("No samples in the first 25 min after the start of a dose.",
                 "Authors report IV clearance underestimated (NCA median 33.8 L/h vs 15.3)."),
    pd = list()
  ),

  chlorpromazine = list(
    drug = NA_character_, route = ROUTE_PO, formulation = "oral",
    parameterBasis = "apparent_oral",
    sourcePopulation = "31 patients with chronic schizophrenia",
    analyte = "chlorpromazine", matrix = "plasma",
    pkSource = "Chetty 1994, doi:10.1007/BF00196109",
    modelStatus = "blocked", pkVerified = FALSE,
    warnings = "No structural model or coefficients in the accessible report; not registered.",
    pd = list()
  )
)

#' Look up an antipsychotic profile
#'
#' @param profile a name in \code{antipsychoticProfiles()}
#' @returns that profile's entry
#' @export
antipsychoticProfile <- function(profile)
{
  profiles <- antipsychoticProfiles()
  if (length(profile) != 1 || !profile %in% names(profiles))
    stop("Unknown antipsychotic profile: ", paste(profile, collapse = ", "))
  profiles[[profile]]
}

#' D2 receptor occupancy from a concentration
#'
#' Applies one of a profile's PAIRED occupancy fits,
#' \code{Emax * C / (EC50 + C)}, to concentrations the caller has already
#' simulated.  The concentration must be the fit's driver: parent plasma,
#' the risperidone + 9-hydroxyrisperidone sum (active moiety), or for
#' aripiprazole the effect-site concentration.  Endpoints that are not an
#' occupancy (haloperidol's PANSS term) are refused.
#'
#' @param profile a name in \code{antipsychoticProfiles()}
#' @param concentration driver concentration(s), ng/mL
#' @param fit index or name of the fit within the profile's \code{pd}
#' @returns occupancy in percent
#' @export
antipsychoticOccupancy <- function(profile, concentration, fit = 1)
{
  p <- antipsychoticProfile(profile)
  if (length(p$pd) == 0)
    stop("No pharmacodynamic endpoint is available for ", profile)
  if (is.character(fit)) {
    fit <- match(fit, vapply(p$pd, `[[`, "", "name"))
    if (is.na(fit)) stop("Unknown fit for ", profile)
  }
  f <- p$pd[[fit]]
  if (!identical(f$endpoint, "D2_occupancy"))
    stop(profile, ": the ", f$endpoint, " endpoint is not a concentration-occupancy relation")
  if (any(concentration < 0, na.rm = TRUE))
    stop("Concentrations must be nonnegative")
  f$Emax * concentration / (f$EC50 + concentration)
}
