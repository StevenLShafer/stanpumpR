test_that("simulateDrugsWithCovariates passes smoke tests and can generate a plot", {
  library(ggplot2)

  weight <- 70
  height <- 170
  age <- 50
  sex <- "male"
  maximum <- 60
  plotRecovery <- FALSE

  doseTable <- data.frame(
    Drug = c("remifentanil","remifentanil","remifentanil", "propofol","propofol","propofol"),
    Time = c(0, 0, 40, 0, 0, 30),
    Dose = c(60, 0.15, 0, 200, 150, 0),
    Units = c("mcg", "mcg/kg/min", "mcg/kg/min", "mg", "mcg/kg/min", "mcg/kg/min")
  )

  eventTable <- data.frame(Time = double(), Event = character(), Fill = character())

  output <- simulateDrugsWithCovariates(doseTable, eventTable, weight, height, age, sex, maximum, plotRecovery)

  expect_length(output, 2L)
  expect_identical(names(output), c("remifentanil", "propofol"))
  expect_identical(names(output), unname(sapply(output, `[[`, "Drug")))

  for (drug in output) {
    results <- drug[["results"]]
    cpce <- results[results$Site == "Plasma" | results$Site == "Effect Site", ]
    g <- ggplot() +
      geom_line(data = cpce, aes(x=Time, y=Y, group=Site, color=Site)) +
      xlab('time (minutes)') + ylab(paste0(drug$Drug,' concentration (', drug$Concentration.Units, ')'))
    expect_no_error(print(g))
  }
})
