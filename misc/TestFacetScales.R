library(ggplot2)
library(scales)
library(facetscales)
mydf <- data.frame(
  Subject = rep(c("A", "B", "C", "D"), each = 4),
  Magnitude = rep(c("SomeValue", "Percent", "Scientific", "LogScale"), times = 4),
  Value = c(c(170, 0.60, 2.7E-4, 10000),
            c(180, 0.80, 2.5E-4, 100),
            c(160, 0.71, 3.2E-4, 1000),
            c(159, 0.62, 3.0E-4, 10)))

scales_y <- list(
  Percent = scale_y_continuous(labels = percent_format()),
  SomeValue = scale_y_continuous(),
  Scientific = scale_y_continuous(labels = scientific_format()),
  LogScale = scale_y_log10()
)

scales_y1 <- sapply(as.character(unique(mydf$Magnitude)), function(x) x = scale_y_continuous())


ggplot(mydf) +
  geom_point(aes(x = Subject, y = Value)) +
  facet_grid_sc(Magnitude ~ ., scales = list(y = scales_y1))
