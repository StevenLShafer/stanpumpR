# Smoke test for the Windows binary built by .github/workflows/windows-binary.yaml.
# Runs against the installed zip, not the source tree: the package must load
# from the binary and a simulation through the API must finish.  The step that
# runs this has a timeout, so a simulation that hangs fails the build.

suppressPackageStartupMessages(library(stanpumpR))
cat("stanpumpR", as.character(utils::packageVersion("stanpumpR")), "on",
    R.version$platform, R.version.string, "\n")

request <- list(
  patient  = list(weight = 70, height = 170, age = 50, sex = "male"),
  duration = 120,
  doses    = data.frame(
    Drug  = c("propofol", "propofol", "fentanyl", "remifentanil"),
    Time  = c(0, 0, 0, 0),
    Dose  = c(2, 100, 100, 0.1),
    Units = c("mg/kg", "mcg/kg/min", "mcg", "mcg/kg/min")),
  output   = list(interval = 1))

elapsed <- system.time(result <- apiSimulate(request))[["elapsed"]]
cat(sprintf("apiSimulate(): %d rows in %.1f s\n", nrow(result$timeseries), elapsed))
stopifnot(nrow(result$timeseries) > 0, all(is.finite(result$timeseries$Plasma)))

json <- apiFormatResult(result, "json")
stopifnot(nchar(json$body) > 0)
cat("OK\n")
