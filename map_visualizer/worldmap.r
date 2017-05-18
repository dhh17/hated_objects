library(maps)
library(dplyr)
library(ggplot2)
library(plotly)
map_data("world") %>%
  plot_geo(x = ~long, y = ~lat) %>%
  add_markers(x = 60, y = 60, size = 60, hoverinfo = "text", color = toRGB("red"), text = "Tea")
  layout(title = "Tea consumption in the Soviet Union")