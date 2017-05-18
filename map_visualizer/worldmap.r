library(leaflet)

coffee_df <- read.csv("./map_visualizer/ports_of_coffee_coordinates.csv")
tea_df <- read.csv("./map_visualizer/ports_of_coffee_coordinates.csv")

# Offset for tea latitude
tea_df$longitude <- rowSums(cbind(tea_df$longitude, rep.int(1, length(tea_df$longitude))))

# ICONS for tea, coffee and wine

greenLeafIcon <- makeIcon(
  iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94,
  shadowUrl = "http//leafletjs.com/examples/custom-icons/leaf-shadow.png",
  shadowWidth = 50, shadowHeight = 64,
  shadowAnchorX = 4, shadowAnchorY = 62
)

coffeeIcon <- makeIcon(
  iconUrl = "http://image.flaticon.com/icons/png/128/63/63132.png",
  iconWidth = 20, iconHeight = 20,
  iconAnchorX = 0, iconAnchorY = 0
)

# wineIcon <- makeIcon()

# tobaccoIcon <- makeIcon

# Scaling for circle size
circleminsize <- 150000
circlemaxsize <- 750000
tableminvalue <- min(coffee_df$frequency)
tablemaxvalue <- max(coffee_df$frequency)

f_to_r <- function(tableval, tablemin, tablemax) {
  scaling <- (circlemaxsize-circleminsize)/(tablemax-tablemin)
  return(scaling*(tableval-tablemin)+circleminsize)
}

coffee_df$radius <- sapply(coffee_df$frequency, FUN = f_to_r, tableminvalue, tablemaxvalue)
tea_df$radius <- sapply(tea_df$frequency, FUN = f_to_r, tableminvalue, tablemaxvalue)


# MAP

worldmap <- leaflet(coffee_df) %>%
  #addTiles() %>%
  addProviderTiles(providers$Stamen.Watercolor) %>%  # Stamen watercolor for pseudohistorical look
  addCircles(lat = ~latitude, lng = ~longitude, radius = ~radius) %>%
  #addMarkers(~longitude, ~latitude, icon = coffeeIcon)
  addCircles(lng = tea_df$longitude, lat = tea_df$latitude, radius = tea_df$radius, color = "#FFA500")

worldmap # Prints the map