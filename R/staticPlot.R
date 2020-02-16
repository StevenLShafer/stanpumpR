# Create static plot for the plot output space
staticPlot <- function(text)
  {
  plot <- ggplot() +
  theme_void() +
  annotate(
    "text",
    label = text,
    x = 0.5,
    y = 0.5,
    size = 7
  )
  plot
}
