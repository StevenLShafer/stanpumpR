# Helpers for the Parkinson's disease drug tests (test-drugs-levodopa.R and
# its neighbours, test-transit.R): one drug's series from
# simulateDrugsWithCovariates(), its exposure and its peak.
pdNoEvents <- data.frame(Time = numeric(0), Event = character(0), Fill = character(0))

# The results of one drug given as `dt` (Drug, Time, Dose, Units), long form
# (Time, Site, Y), with the fat-free-mass switch off unless asked.
pdSimulate <- function(dt, maximum, weight = 70, height = 170, age = 60,
                       sex = "male", adjustToFFM = FALSE, plotRecovery = FALSE, ...)
{
  r <- simulateDrugsWithCovariates(dt, pdNoEvents, weight, height, age, sex,
                                   maximum = maximum, plotRecovery = plotRecovery,
                                   adjustToFFM = adjustToFFM, ...)
  r[[dt$Drug[1]]]$results
}

# Repeated doses of one unit every `tau` minutes, n of them
pdRegimen <- function(drug, units, dose, tau, n)
  data.frame(Drug = drug, Time = (seq_len(n) - 1) * tau, Dose = dose, Units = units)

# One site's series
pdSite <- function(res, site = "Plasma") res[res$Site == site, c("Time", "Y")]

# Trapezoidal AUC between two times, in concentration x hours
pdAUC <- function(res, from, to, site = "Plasma")
{
  s <- pdSite(res, site)
  s <- s[s$Time >= from & s$Time <= to, ]
  sum(diff(s$Time) * (utils::head(s$Y, -1) + utils::tail(s$Y, -1)) / 2) / 60
}

# Peak time (hours) and concentration
pdPeak <- function(res, site = "Plasma")
{
  s <- pdSite(res, site)
  c(t = s$Time[which.max(s$Y)] / 60, C = max(s$Y))
}

# The terminal half-life (hours) of a drug at a patient
pdTerminalHalfLife <- function(drug, ...)
{
  pk <- getDrugPK(drug, ...)$PK[[1]]
  l <- c(pk$lambda_1, pk$lambda_2, pk$lambda_3)
  log(2) / min(l[l > 0]) / 60
}
