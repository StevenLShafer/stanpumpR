library(ggplot2)
library(scales)
library(facetscales)
mydf <- data.frame(
  Subject = rep(c("A", "B", "C", "D"), each = 3),
  Magnitude = rep(c("SomeValue", "Percent", "Scientific"), times = 4),
  Value = c(c(170,0.6,2.7E-4),
            c(180, 0.8, 2.5E-4),
            c(160, 0.71, 3.2E-4),
            c(159, 0.62, 3E-4))
)
X <- c("SomeValue", "Percent", "Scientific")

scales_y <- sapply(as.character(unique(mydf$Magnitude)), function(x) x = scale_y_continuous())
scales_y$Percent <- scale_y_continuous(labels = NULL)



scales_y <- list(
  Percent = scale_y_continuous(labels = percent_format()),
  SomeValue = scale_y_continuous(),
  Scientific = scale_y_continuous(labels = NULL)
)
print(scales_y)

X <- ggplot(mydf) +
  geom_point(aes(x = Subject, y = Value)) +
  facet_grid_sc(rows = vars(Magnitude), scales = "free_y")

Y <- ggplot_build(X)
str(Y$scales)

layer_scales(Y)
layer_scales(X)
plot(X)
str(X$facet$super)
unlist(X$scales$scales)
