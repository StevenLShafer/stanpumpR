# Builds the Single Paper Review fixtures: extractions of fictional papers
# whose models are known, so that every check has a right answer.  Run from
# the package root:
#   Rscript tests/testthat/fixtures/make-paper-review-fixtures.R
#
# The derived quantities a "paper" prints are computed here with base R
# (eigen() of the rate matrix, the analytical solution of a bolus and an
# infusion), not with stanpumpR's cube() or simCpCe(), so the tests compare
# stanpumpR against an independent calculation.  ke0 is the one exception: it
# is what a paper reporting both tPeak and ke0 would print, solved here by
# bisection on the effect-site curve.
#
# Exampleol (inst/paper-review/example-extraction.json, also the app's
# example) is a three-compartment intravenous drug with one deliberate
# misprint (the second half-life, printed 30% high), an independent-groups
# difference claimed as significant that is not, and a volume read as fat.
# (Claude Code, 2026-10-30.)

V1 <- 8; V2 <- 20; V3 <- 120; CL <- 1.2; Q2 <- 2.0; Q3 <- 0.6     # L, L/min
k10 <- CL / V1; k12 <- Q2 / V1; k13 <- Q3 / V1; k21 <- Q2 / V2; k31 <- Q3 / V3
K <- matrix(c(-(k10 + k12 + k13), k21, k31,
              k12, -k21, 0,
              k13, 0, -k31), 3, byrow = TRUE)
lambda <- sort(-eigen(K)$values, decreasing = TRUE)                  # fastest first
# Bolus coefficients per unit dose: C(t) = sum A_i exp(-lambda_i t)
A <- vapply(1:3, function(i) {
  l <- lambda[i]
  (k21 - l) * (k31 - l) / ((lambda[-i][1] - l) * (lambda[-i][2] - l)) / V1
}, numeric(1))
stopifnot(abs(sum(A) - 1 / V1) < 1e-12)
bolus <- function(t, dose) dose * colSums(A * exp(-outer(lambda, t)))
infusion <- function(t, rate, duration) {
  on <- pmin(t, duration)
  rate * colSums(A / lambda * (exp(-outer(lambda, pmax(t - on, 0))) - exp(-outer(lambda, t))))
}
# tPeak 3 min -> ke0, by bisection on the effect-site bolus curve
tPeak <- 3
peakAt <- function(ke0) {
  ce <- function(t) sum(A * ke0 / (ke0 - lambda) * (exp(-lambda * t) - exp(-ke0 * t)))
  stats::optimize(ce, c(0, 60), maximum = TRUE)$maximum
}
lo <- 0.01; hi <- 10
for (i in 1:200) { mid <- sqrt(lo * hi); if (peakAt(mid) > tPeak) lo <- mid else hi <- mid }
ke0 <- sqrt(lo * hi)

thalf <- log(2) / lambda
weight <- 70
dose1 <- 1 * weight                     # 1 mg/kg bolus, mg
rate2 <- 0.1 * weight                   # 0.1 mg/kg/min for 60 min
aucNgHmL <- dose1 / CL * 1000 / 60      # mg.min/L = mcg.min/mL -> ng.h/mL
noise <- c(1.04, 0.97, 1.03, 0.96, 1.05, 0.98, 1.02, 0.95, 1.03)
t1 <- c(2, 5, 10, 20, 30, 60, 120, 240, 360)
y1 <- bolus(t1, dose1) * 1000 * noise   # mg/L -> ng/mL
t2 <- c(10, 30, 60, 90, 120, 180)
y2 <- infusion(t2, rate2, 60) * 1000 * noise[1:6]

num <- function(x, digits = 3) signif(x, digits)
txt <- function(x, digits = 3) trimws(formatC(signif(x, digits), format = "fg", digits = digits))
unc <- function(type = "not_reported", value = NULL, lower = NULL, upper = NULL)
  list(type = type, value = value, lower = lower, upper = upper)
param <- function(name, role, value, units, se, location = "Table 2, p. 5")
  list(name_in_paper = name, role = role, route = NULL, value = value,
       value_text = as.character(value), units = units,
       uncertainty = unc("RSE_percent", se), status = "estimated", location = location)
derived <- function(q, value, units, location = "Table 3, p. 6", regimen = NULL,
                    statistic = "typical_value", digits = 3)
  list(quantity = q, value = num(value, digits), value_text = txt(value, digits), units = units,
       regimen_id = regimen, statistic = statistic, uncertainty = unc(), location = location)

example <- list(
  paper = list(title = "Pharmacokinetics of exampleol in adult volunteers (a fictional paper)",
               first_author = "Example", year = 2026, journal = "Fixture", doi = NULL),
  drug = list(name = "exampleol", analyte = "parent", matrix = "arterial plasma",
              concentration_units = "ng/mL", molecular_weight = 300.4,
              dose_basis = "salt", dose_basis_note = "Doses are exampleol hydrochloride (Methods, p. 3).",
              location = "Methods, p. 3"),
  structure = list(compartments = 3, elimination = "linear", absorption = "none",
                   absorption_lag = "not_applicable", routes = list("iv_bolus", "iv_infusion"),
                   apparent_oral = "not_applicable", effect_site = "tpeak",
                   pd_model = "Effect site from the time to peak EEG effect",
                   parameterization = "Volumes and clearances", location = "Results, p. 5"),
  parameters = list(
    param("V1", "V1", V1, "L", 12), param("V2", "V2", V2, "L", 15), param("V3", "V3", V3, "L", 20),
    param("CL", "CL", CL, "L/min", 6), param("Q2", "Q2", Q2, "L/min", 18), param("Q3", "Q3", Q3, "L/min", 22),
    param("tpeak", "tPeak", tPeak, "min", 10, "Results, p. 7")),
  covariate_effects = list(
    list(parameter_role = "CL", covariate = "weight", form = "power", coefficient = 0.75,
         reference_value = 70, covariate_units = "kg",
         equation_as_printed = "CL = 1.2 x (WT/70)^0.75", location = "Equation 2, p. 5"),
    list(parameter_role = "V1", covariate = "age", form = "linear", coefficient = -0.005,
         reference_value = 40, covariate_units = "years",
         equation_as_printed = "V1 = 8 x (1 - 0.005 (AGE - 40))", location = "Equation 3, p. 5")),
  population = list(n = 24, description = "Healthy adult volunteers",
                    age_years = list(central = 42, central_kind = "mean", min = 25, max = 65),
                    weight_kg = list(central = 75, central_kind = "mean", min = 55, max = 110),
                    height_cm = list(central = 174, central_kind = "mean", min = 158, max = 192),
                    n_female = 12, n_male = 12, other = "ASA I, no co-medication",
                    location = "Table 1, p. 4"),
  regimens = list(
    list(id = "R1", description = "1 mg/kg intravenous bolus",
         doses = list(list(route = "iv_bolus", time = 0, amount = 1, mass_unit = "mg", per_kg = "yes",
                           per_time = "none", duration = NULL, time_units = "min")),
         location = "Methods, p. 3"),
    list(id = "R2", description = "0.1 mg/kg/min for 60 min",
         doses = list(list(route = "iv_infusion", time = 0, amount = 0.1, mass_unit = "mg", per_kg = "yes",
                           per_time = "per_min", duration = 60, time_units = "min")),
         location = "Methods, p. 3")),
  derived_quantities = list(
    derived("t_half_1", thalf[1], "min"),
    derived("t_half_2", thalf[2] * 1.3, "min"),          # the deliberate misprint
    derived("t_half_3", thalf[3] / 60, "h"),
    derived("Vss", V1 + V2 + V3, "L"),
    derived("ke0", ke0, "1/min", "Results, p. 7"),
    derived("AUC_inf", aucNgHmL, "ng*h/mL", regimen = "R1"),
    derived("coefficient_1", A[1], "1/L", digits = 4),
    derived("coefficient_2", A[2], "1/L", digits = 4),
    derived("coefficient_3", A[3], "1/L", digits = 4)),
  figures = list(
    list(figure_id = "Figure 2", caption = "Mean arterial concentrations after a 1 mg/kg bolus",
         regimen_id = "R1", y_quantity = "concentration", y_units = "ng/mL", time_units = "min",
         log_y = "yes", data_kind = "mean", error_bars = "SD",
         series = list(list(label = "Mean", points = lapply(seq_along(t1), function(i)
           list(time = t1[i], value = num(y1[i]), error_lower = num(y1[i] * 0.8),
                error_upper = num(y1[i] * 1.2))))),
         digitization_note = "Read from a log axis; about 5% precision.", location = "Figure 2, p. 6"),
    list(figure_id = "Figure 3", caption = "Mean concentrations during and after a 60-min infusion",
         regimen_id = "R2", y_quantity = "concentration", y_units = "ng/mL", time_units = "min",
         log_y = "no", data_kind = "mean", error_bars = "none",
         series = list(list(label = "Mean", points = lapply(seq_along(t2), function(i)
           list(time = t2[i], value = num(y2[i]), error_lower = NULL, error_upper = NULL)))),
         digitization_note = "Read from a linear axis; about 5% precision.", location = "Figure 3, p. 7")),
  reported_differences = list(
    list(description = "Clearance, women against men", group1_value = 1.1, group1_se = 0.08,
         group2_value = 1.3, group2_se = 0.09, difference = 0.2, difference_se = NULL,
         paired = "no", p_value = 0.04, location = "Results, p. 6")),
  bioavailability = list(source = "not_applicable", note = "", location = ""),
  interpretive_claims = list(
    list(category = "volume_as_anatomy", claim = "V3 represents the drug's distribution into fat",
         location = "Discussion, p. 9")),
  reviewer_notes = list(
    list(severity = "concern", topic = "Second half-life",
         finding = "The intermediate half-life in Table 3 does not follow from Table 2.",
         location = "Table 3, p. 6")),
  extraction_limitations = "Figure points are read by eye from the plotted symbols."
)

# Esmolol-like: two compartments, CL, Vss and both half-lives, closed only
# through twoCompartmentFromHalfLives(), with standard errors wide enough to
# leave the admissible region.
twoComp <- function(seCL, seVss, seT1, seT2) list(
  paper = list(title = "A two-compartment fixture", first_author = "Fixture", year = 2026,
               journal = NULL, doi = NULL),
  drug = list(name = "twocompol", analyte = "parent", matrix = "whole blood",
              concentration_units = "mcg/mL", molecular_weight = NULL, dose_basis = "base",
              dose_basis_note = "", location = "Methods"),
  structure = list(compartments = 2, elimination = "linear", absorption = "none",
                   absorption_lag = "not_applicable", routes = list("iv_infusion"),
                   apparent_oral = "not_applicable", effect_site = "none", pd_model = NULL,
                   parameterization = "Non-compartmental CL and Vss with two half-lives",
                   location = "Results"),
  parameters = list(),
  covariate_effects = list(),
  population = list(n = 10, description = "Volunteers",
                    age_years = list(central = NULL, central_kind = "unstated", min = NULL, max = NULL),
                    weight_kg = list(central = 70, central_kind = "mean", min = NULL, max = NULL),
                    height_cm = list(central = NULL, central_kind = "unstated", min = NULL, max = NULL),
                    n_female = NULL, n_male = NULL, other = "", location = "Table 1"),
  regimens = list(),
  derived_quantities = list(
    list(quantity = "CL", value = 0.285, value_text = "0.285", units = "L/min/kg", regimen_id = NULL,
         statistic = "mean_of_individuals", uncertainty = unc("SE", seCL), location = "Table 2"),
    list(quantity = "Vss", value = 1.9, value_text = "1.9", units = "L/kg", regimen_id = NULL,
         statistic = "mean_of_individuals", uncertainty = unc("SE", seVss), location = "Table 2"),
    list(quantity = "t_half_1", value = 2.0, value_text = "2.0", units = "min", regimen_id = NULL,
         statistic = "mean_of_individuals", uncertainty = unc("SE", seT1), location = "Table 2"),
    list(quantity = "t_half_2", value = 9.2, value_text = "9.2", units = "min", regimen_id = NULL,
         statistic = "mean_of_individuals", uncertainty = unc("SE", seT2), location = "Table 2")),
  figures = list(), reported_differences = list(),
  bioavailability = list(source = "not_applicable", note = "", location = ""),
  interpretive_claims = list(), reviewer_notes = list(), extraction_limitations = "")

write <- function(x, path)
  writeLines(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA, pretty = TRUE), path)
write(example, "inst/paper-review/example-extraction.json")
write(twoComp(0.06, 0.6, 0.6, 3.0), "tests/testthat/fixtures/paper-review-ill-conditioned.json")
write(twoComp(0.005, 0.03, 0.05, 0.2), "tests/testthat/fixtures/paper-review-well-conditioned.json")

# The answers the tests pin, from the calculation above.
cat("lambda", format(lambda, digits = 10), "\n")
cat("thalf", format(thalf, digits = 10), "\n")
cat("A", format(A, digits = 10), "\n")
cat("ke0", format(ke0, digits = 10), "\n")
cat("AUC ng.h/mL", format(aucNgHmL, digits = 10), "\n")
