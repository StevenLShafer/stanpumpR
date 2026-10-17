# Carvedilol (R/drugs_carvedilol.R): Hwang 2023's apparent oral two-compartment
# model with zero-order release.  Drafted by Claude Code, 2026-10-17.
#
# The parameters are those of the paper's Table 2, read from the full text.
# The expectations are written from the published values independently of the
# drug file, in the paper's own units (L and L/h), and converted here.

# Hwang S et al., J Korean Med Sci 2023;38:e173, Table 2
CL_F <- 153      # L/h
V1_F <- 440      # L
V2_F <- 754      # L
Q_F  <- 41.3     # L/h
D1_H <- 0.383    # h
TLAG_H <- 0.215  # h

events <- data.frame(Time = double(), Event = character())

test_that("the published apparent parameters are reproduced exactly", {
  PK <- getDrugPK("carvedilol", 70, 174, 27, "male", adjustToFFM = FALSE)
  set <- PK$PK$default
  expect_equal_rounded(V1_F, set$v1)
  expect_equal_rounded(V2_F, set$v2)
  expect_equal_rounded(CL_F, set$cl1 * 60)
  expect_equal_rounded(Q_F, set$cl2 * 60)
  # Two compartments: the third is the library's placeholder
  expect_equal(set$cl3, 0)
  # Plasma, ng/mL, no effect site, not an opioid, no band
  expect_identical(PK$Concentration.Units, "ng")
  expect_equal(PK$tPeak, 0)
  expect_equal(set$ke0, 0)
  expect_equal(PK$MEAC, 0)
  expect_equal(PK$typical, 0)
})

test_that("the zero-order release is the published duration and lag, in minutes", {
  PK <- getDrugPK("carvedilol", 70, 174, 27, "male")
  expect_equal_rounded(D1_H * 60, PK$oralZeroOrder$duration)
  expect_equal_rounded(TLAG_H * 60, PK$oralZeroOrder$lag)
  # 0.383 h is 22.98 min and 0.215 h is 12.9 min
  expect_equal_rounded(22.98, PK$oralZeroOrder$duration)
  expect_equal_rounded(12.9, PK$oralZeroOrder$lag)
})

test_that("absorption is zero order, because the source rejected first order", {
  # The paper reports that first-order, first-order-with-lag and dual
  # absorption models did not describe the data.  So the drug must carry no
  # absorption rate constant, and the engine must not apply a bioavailability:
  # F is inside the apparent parameters.
  set <- getDrugPK("carvedilol", 70, 174, 27, "male")$PK$default
  expect_equal(set$ka_PO, 0)
  expect_equal(set$bioavailability_PO, 0)
  expect_false(is.null(getDrugPK("carvedilol", 70, 174, 27, "male")$oralZeroOrder))
})

test_that("no intravenous or other route is offered for an apparent model", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "carvedilol", ]
  # getDrugDefaultsGlobal() parses Units into a list column
  units <- unlist(row$Units)
  # Every unit is oral: an apparent parameter set predicts an intravenous
  # concentration wrong by 1/F
  expect_true(all(doseRoute(units) == ROUTE_PO))
  expect_identical(row$Default.Units, "mg PO")
  expect_identical(row$Category, "Antihypertensives")
  # No bolus or infusion unit of its own
  expect_true(is.na(row$Bolus.Units) || !nzchar(as.character(row$Bolus.Units)))
  expect_true(is.na(row$Infusion.Units) ||
                !nzchar(as.character(row$Infusion.Units)))
})

test_that("the size switch leaves the published model unscaled when off", {
  off <- getDrugPK("carvedilol", 120, 180, 50, "male",
                   adjustToFFM = FALSE)$PK$default
  # No covariate in the source: with the switch off every patient gets the
  # published values, whatever their size
  expect_equal_rounded(V1_F, off$v1)
  expect_equal_rounded(CL_F, off$cl1 * 60)
  # With the switch on, a larger patient is scaled up from the reference man
  on <- getDrugPK("carvedilol", 120, 180, 50, "male",
                  adjustToFFM = TRUE)$PK$default
  expect_gt(on$v1, off$v1)
  expect_gt(on$cl1, off$cl1)
  # And the reference man is the anchor, where the two agree
  refOn  <- getDrugPK("carvedilol", 70, 170, 35, "male",
                      adjustToFFM = TRUE)$PK$default
  refOff <- getDrugPK("carvedilol", 70, 170, 35, "male",
                      adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(refOff$v1, refOn$v1)
  expect_equal_rounded(refOff$cl1, refOn$cl1)
})

test_that("the CYP2D6 effect is recorded but deliberately not applied", {
  # The source fitted CL/F = 153 x (1 - 0.328 x GT) for its second
  # intermediate-metaboliser group only.  The Patient Profile has one
  # "intermediate" value, which cannot separate that group from the paper's
  # IM-1 group, in whom no reduction was measured, so the model takes no
  # cyp2d6 argument at all.
  expect_false("cyp2d6" %in% names(formals(carvedilol)))
  # The fitted effect is in the audit, as an unapplied covariate
  audit <- antihypertensiveParameters()
  rows <- audit[grepl("hwang2023", audit$model_id), ]
  effect <- rows[grepl("CYP2D6", rows$parameter), ]
  expect_equal(nrow(effect), 1)
  expect_equal_rounded(0.328, effect$value)
  expect_match(effect$notes, "NOT APPLIED")
  # 1 - 0.328 is the 0.672 multiplier the handoff quoted
  expect_equal_rounded(0.672, 1 - effect$value)
})

test_that("a single 12.5 mg dose peaks where release ends, at a plausible height", {
  wide <- simulateDrugsWithCovariates(
    data.frame(Drug = "carvedilol", Time = 0, Dose = 12.5, Units = "mg PO"),
    events, 70, 174, 27, "male", 1440, FALSE,
    adjustToFFM = FALSE)$carvedilol$wide
  peakAt <- TLAG_H * 60 + D1_H * 60
  expect_equal_rounded(peakAt, wide$Time[which.max(wide$Plasma)])
  # The source's single dose was 12.5 mg; this model puts its peak at about
  # 26 ng/mL, which is the order the label and the literature report for a
  # 12.5 mg tablet (tens of ng/mL).
  expect_equal_rounded(26.14, round(max(wide$Plasma), 2))
  # Nothing before the lag
  expect_true(all(wide$Plasma[wide$Time <= TLAG_H * 60] == 0))
})

test_that("the whole oral dose is delivered in apparent coordinates", {
  # AUC = dose / (CL/F), the test that F is not applied a second time
  wide <- simulateDrugsWithCovariates(
    data.frame(Drug = "carvedilol", Time = 0, Dose = 25, Units = "mg PO"),
    events, 70, 174, 27, "male", 20160, FALSE,
    adjustToFFM = FALSE)$carvedilol$wide
  auc <- sum(diff(wide$Time) *
               (head(wide$Plasma, -1) + tail(wide$Plasma, -1)) / 2)
  expect_equal(25 / (CL_F / 60) * 1000, auc, tolerance = 0.01)
})

test_that("the disposition half-lives follow from the published parameters", {
  set <- getDrugPK("carvedilol", 70, 174, 27, "male",
                   adjustToFFM = FALSE)$PK$default
  # Computed here from the published values, not read from the engine
  k10 <- (CL_F / 60) / V1_F; k12 <- (Q_F / 60) / V1_F; k21 <- (Q_F / 60) / V2_F
  b <- k10 + k12 + k21; c2 <- k10 * k21
  l1 <- (b + sqrt(b^2 - 4 * c2)) / 2
  l2 <- (b - sqrt(b^2 - 4 * c2)) / 2
  expect_equal_rounded(l1, set$lambda_1)
  expect_equal_rounded(l2, set$lambda_2)
  # About 1.5 h and 16.5 h; the label's parent half-life is 7-10 h, between
  # them, which is what a two-compartment fit of a longer-sampled study does
  expect_equal_rounded(91.5, round(log(2) / l1, 1))
  expect_equal_rounded(992.3, round(log(2) / l2, 1))
})

test_that("the registry now carries carvedilol as executable, and still no blood pressure", {
  reg <- antihypertensiveRegistry()
  pk <- reg[reg$model_id ==
    "carvedilol__ir_tablet__oral__healthy_korean_men__concentration__hwang2023", ]
  expect_equal(nrow(pk), 1)
  expect_identical(pk$stanpumpr_drug, "carvedilol")
  expect_identical(pk$execution_status, "ready_for_source_reproduction")
  expect_identical(pk$pk_evidence, "primary_fitted_full")
  expect_false(pk$bp_simulation_enabled)
  # Its pharmacodynamic record is the isoproterenol challenge, still gated
  hr <- reg[reg$model_id ==
    "carvedilol__ir_tablet__oral__healthy_korean_men__iso_hr__hwang2023", ]
  expect_identical(hr$endpoint, "isoproterenol_induced_hr")
  # Catalogued, not executable: predicting that endpoint needs an injected
  # isoproterenol dose as a second input, which the package cannot express
  expect_identical(hr$execution_status, "descriptive_only")
  expect_identical(hr$pd_evidence, "primary_fitted_full")
  expect_false(hr$bp_simulation_enabled)
  # Every number behind the executable record is now verified primarily
  audit <- antihypertensiveParameters()
  used <- audit[audit$model_id == pk$model_id & !is.na(audit$value), ]
  expect_gt(nrow(used), 5)
  expect_true(all(used$verification == "primary_full_text"))
})

test_that("the published precision travels with every parameter", {
  audit <- antihypertensiveParameters()
  rows <- audit[audit$model_id ==
    "carvedilol__ir_tablet__oral__healthy_korean_men__concentration__hwang2023", ]
  # The paper's own precision type
  withPrecision <- rows[!is.na(rows$uncertainty), ]
  expect_gt(nrow(withPrecision), 5)
  expect_true(all(withPrecision$uncertainty_type ==
                    "relative standard error, %"))
  # The release duration is the weak parameter, and the audit says so
  d1 <- rows[rows$parameter == "D1 (zero-order release duration)", ]
  expect_equal(nrow(d1), 1)
  expect_equal_rounded(70, d1$uncertainty)
  expect_match(d1$notes, "uncertain")
})
