test_that("a ggplot2 object is created with correct data names", {

library(ggplot2)

maximum <- 60
plotRecovery <- FALSE

doseTable <- data.frame(
  Drug = c("remifentanil", "remifentanil", "remifentanil"),
  Time = c(0, 0, 30),
  Dose = c(60, 0.15, 0),
  Units = c("mcg", "mcg/kg/min", "mcg/kg/min")
)

eventTable <- data.frame(Time = double(), Event = character(), Fill = character())

PK <- getDrugPK(
    drug = "remifentanil",
    weight = 70,
    height = 170,
    age = 50,
    sex = "male",
    getDrugDefaults("remifentanil")
)

output <- simCpCe(doseTable, eventTable, PK, maximum, plotRecovery)
results <- output["results"][["results"]]
cpce <- results[results$Site == "Plasma" | results$Site == "Effect Site", ]

g <- ggplot(cpce, aes(x=Time, y=Y, group=Site, color=Site)) +
            geom_line() + xlab('Time (minutes)') + ylab('Concentration (ng/ml)')

expect_equal(names(g$data),c("Drug","Time","Site","Y"))

})
