# Calcium gluconate: the same disposition as calcium chloride
# (R/drugs_calciumChloride.R, which holds the source, the population and the
# caveats), reached through the fitted dose ratio.
#
# Ansari and colleagues (Anesthesiology 2025;142:121-31) randomised
# parturients to calcium chloride 0.5 g or calcium gluconate 1.5 or 2 g by
# 10-minute infusion and found that, corrected for the dose ratio, the two
# salts give the same time course of ionized calcium: the ratio is a
# parameter of their model, 3.11 g of calcium gluconate per gram of calcium
# chloride (95% CI 2.77 to 3.48), close to the 3.05 that the molecular
# weights of the hydrated salts give (calcium gluconate monohydrate 448 g/mol,
# calcium chloride dihydrate 147 g/mol).  A dose of calcium gluconate is
# therefore divided by 3.11 and run through the calcium chloride model, which
# stanpumpR does by converting the dose to millimoles of calcium chloride
# equivalent at 147.01 x 3.11 = 457.2 g per mole: the `molar` block below.
#
# So 3 g of calcium gluconate plots as 3 / 3.11 = 0.965 of 1 g of calcium
# chloride, and 3.11 g plots identically to it.  The plotted quantity is the
# change in ionized calcium in mmol/L, as for calcium chloride.
#
# Drafted by Claude Code, 2026-10-13, at the request of Steven L. Shafer.

calciumGluconate <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  out <- calciumIonizedPK(weight, height, age, sex, adjustToFFM)
  # Grams of calcium gluconate per mole of calcium chloride dihydrate
  # equivalent: the fitted bioequivalence ratio, not the stoichiometric one.
  out$molar <- list(
    molecularWeight = CALCIUM_CHLORIDE_DIHYDRATE_MW * CALCIUM_GLUCONATE_DOSE_RATIO
  )
  out
}
