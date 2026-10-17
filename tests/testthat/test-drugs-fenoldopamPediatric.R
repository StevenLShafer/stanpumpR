# Fenoldopam in children (R/drugs_fenoldopamPediatric.R): Hammer 2008's
# one-compartment model.  Drafted by Claude Code, 2026-10-17.
#
# The expectations are derived from the published numbers independently of the
# drug file.

# Hammer GB et al., BMC Anesthesiol 2008;8:6, compartmental analysis
KEL <- 13.2    # /h
CLH <- 3.06    # L/h/kg

events <- data.frame(Time = double(), Event = character())

# A child of the source population's average size, with the scaling switch
# off so the published per-kilogram values apply unchanged
simulateFenoldopam <- function(dose, maximum = 240, adjustToFFM = FALSE,
                               weight = 16, height = 100, age = 4,
                               sex = "male") {
  simulateDrugsWithCovariates(
    dose, events, weight, height, age, sex, maximum, FALSE,
    adjustToFFM = adjustToFFM)$fenoldopamPediatric$wide
}

test_that("the model is one compartment with the published clearance and the derived volume", {
  PK <- getDrugPK("fenoldopamPediatric", 16, 100, 4, "male",
                  adjustToFFM = FALSE)
  set <- PK$PK$default
  expect_equal_rounded(CLH / 60 * 16, set$cl1)        # L/min
  expect_equal_rounded(CLH / KEL * 16, set$v1)        # V = CL/kel, derived
  # One compartment: the second and third are the library's placeholders
  expect_equal(set$cl2, 0)
  expect_equal(set$cl3, 0)
  # The elimination rate constant and half-life the paper prints
  expect_equal_rounded(KEL / 60, set$cl1 / set$v1)
  expect_equal_rounded(3.1507, round(log(2) / (set$cl1 / set$v1), 4))
  # Plasma in ng/mL, no effect site, not an opioid
  expect_identical(PK$Concentration.Units, "ng")
  expect_equal(PK$tPeak, 0)
  expect_equal(set$ke0, 0)
  expect_equal(PK$MEAC, 0)
})

test_that("the derived volume agrees with the paper's non-compartmental pair", {
  # Compartmental CL/kel = 3.06/13.2; non-compartmental 3.02/12.4.  The paper
  # publishes no volume, so agreement between the two analyses is the only
  # check available, and the specification requires the derivation to be
  # labelled as such.
  compartmental <- CLH / KEL
  nonCompartmental <- 3.02 / 12.4
  expect_equal_rounded(0.2318, round(compartmental, 4))
  expect_equal_rounded(0.2435, round(nonCompartmental, 4))
  expect_lt(abs(compartmental / nonCompartmental - 1), 0.05)
  audit <- antihypertensiveParameters()
  v <- audit[audit$parameter == "V" & grepl("hammer2008", audit$model_id), ]
  expect_equal(nrow(v), 1)
  expect_identical(v$estimate_status, "derived")
  expect_identical(v$reported_or_derived, "derived")
  expect_identical(v$verification, "derived_here")
})

test_that("an infusion reaches the steady state its clearance implies", {
  # Css = rate / CL.  1 mcg/kg/min at a clearance of 0.051 L/min/kg is
  # 19.608 ng/mL, whatever the child's weight, because both are per kilogram.
  css <- 1 / (CLH / 60) * 1                  # mcg/L = ng/mL
  for (weight in c(7, 16, 40)) {
    wide <- simulateFenoldopam(
      data.frame(Drug = "fenoldopamPediatric", Time = c(0, 60), Dose = c(1, 0),
                 Units = "mcg/kg/min"),
      maximum = 120, weight = weight)
    expect_equal_rounded(css, wide$Plasma[wide$Time == 60], tolerance = 1e-4)
  }
  expect_equal_rounded(19.6078, round(css, 4))
})

test_that("the shaded band is the effective dose range's steady state", {
  # The paper concluded 0.8 to 1.2 mcg/kg/min controlled pressure, with no
  # further benefit above 1.2.  The band is those rates divided by the
  # fitted clearance, and the help page says so.
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "fenoldopamPediatric", ]
  expect_equal_rounded(round(0.8 / (CLH / 60), 1), row$Lower)
  expect_equal_rounded(round(1.2 / (CLH / 60), 1), row$Upper)
  expect_equal_rounded(round(1.0 / (CLH / 60), 1), row$Typical)
  expect_identical(row$Category, "Antihypertensives")
})

test_that("clearance is linear in weight, as the paper reports it", {
  # Per kilogram with no allometric exponent: doubling the weight doubles
  # both the volume and the clearance when the switch is off
  small <- getDrugPK("fenoldopamPediatric", 10, 80, 2, "male",
                     adjustToFFM = FALSE)$PK$default
  large <- getDrugPK("fenoldopamPediatric", 20, 110, 6, "male",
                     adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(2, large$v1 / small$v1)
  expect_equal_rounded(2, large$cl1 / small$cl1)
  # And the half-life does not change with size
  expect_equal_rounded(small$cl1 / small$v1, large$cl1 / large$v1)
})

test_that("the blood-pressure relation is available as an endpoint, never as a plot", {
  id <- "fenoldopam__injection__iv__anaesthetised_children__map__hammer2008"
  # At the EC50 the change is exactly half the maximum, and it is a FALL:
  # the specification's sign test for a negative published Emax
  half <- antihypertensiveEndpoint(id, 9.1)
  expect_equal_rounded(-13.5, half$predicted_change)
  expect_identical(half$predicted_change_unit, "mmHg")
  expect_identical(half$direction, "decrease")
  # Zero concentration is no change, and the effect saturates below 27 mmHg
  expect_equal(antihypertensiveEndpoint(id, 0)$predicted_change, 0)
  expect_gt(antihypertensiveEndpoint(id, 1e6)$predicted_change, -27)
  expect_lt(antihypertensiveEndpoint(id, 1e6)$predicted_change, -26.9)
  # Monotone decreasing in concentration
  changes <- antihypertensiveEndpoint(id, c(0, 1, 5, 9.1, 20, 60))$predicted_change
  expect_true(all(diff(changes) < 0))
  # No blood pressure is simulated for the record, and the drug does not plot one
  expect_false(unique(antihypertensiveEndpoint(id, 9.1)$bp_simulation_enabled))
  expect_identical(
    antihypertensiveRegistry()$bp_simulation_enabled[
      antihypertensiveRegistry()$stanpumpr_drug %in% "fenoldopamPediatric"],
    c(FALSE, FALSE))
})

test_that("a concentration above the highest rate studied is flagged as an extrapolation", {
  id <- "fenoldopam__injection__iv__anaesthetised_children__map__hammer2008"
  # 3.2 mcg/kg/min, the top rate, gives 3.2 / 0.051 = 62.7 ng/mL
  inside <- antihypertensiveEndpoint(id, 60)
  outside <- antihypertensiveEndpoint(id, 200)
  expect_false(grepl("extrapolation", inside$warnings))
  expect_true(grepl("extrapolation", outside$warnings))
  # Every result carries the population, so a paediatric fit cannot be read
  # as an adult one by accident
  expect_match(inside$population, "children")
})

test_that("the adult record is registered as not implemented, and says why", {
  reg <- antihypertensiveRegistry()
  adult <- reg[grepl("frederickson1988", reg$model_id), ]
  expect_equal(nrow(adult), 1)
  expect_true(is.na(adult$stanpumpr_drug))
  expect_identical(adult$execution_status, "requires_parameter_fit")
  expect_match(adult$limitations, "inconsistent")
  # The paediatric drug is the only fenoldopam in the library
  dd <- getDrugDefaultsGlobal()
  expect_true("fenoldopamPediatric" %in% dd$Drug)
  expect_false("fenoldopam" %in% dd$Drug)
})
