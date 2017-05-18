library(leaflet)
# library(mapview)

get_df_subset_by_freqn_minhits <- function(df, freqn, minhits) {
  df <- df[,c("port", "country", paste0("freq",as.character(freqn)),
              "latitude", "longitude")]
  colnames(df) <- c("port", "country", "frequency", "latitude", "longitude")
  df <- subset(df, frequency > minhits)
  return(df)
}


# coffee_df <- read.csv("./map_visualizer/ports_of_coffee_coordinates.csv")
tea_df <- read.csv("./map_visualizer/ports_of_tea_cat_coordinates.csv")
tea_df$freq1 <- tea_df$freq1 + tea_df$freq0 
tea_df <- subset(tea_df, port != "London")
coffee_df <- read.csv("./map_visualizer/ports_of_coffee_cat_coordinates.csv")
coffee_df$freq1 <- coffee_df$freq1 + coffee_df$freq0 
coffee_df <- subset(coffee_df, port != "London")
wine_df <- read.csv("./map_visualizer/ports_of_wine_cat_coordinates.csv")
wine_df$freq1 <- wine_df$freq1 + wine_df$freq0 
wine_df <- subset(wine_df, port != "London")
opium_df <- read.csv("./map_visualizer/ports_of_opium_cat_coordinates.csv")
opium_df$freq1 <- opium_df$freq1 + opium_df$freq0 
opium_df <- subset(opium_df, port != "London")

tea_df <- get_df_subset_by_freqn_minhits(tea_df, 1, 1)
coffee_df <- get_df_subset_by_freqn_minhits(coffee_df, 1, 0)
wine_df <- get_df_subset_by_freqn_minhits(wine_df, 3, 0)
opium_df <- get_df_subset_by_freqn_minhits(opium_df, 1, 0)

# Offset for tea latitude
tea_df$longitude <- rowSums(cbind(tea_df$longitude, rep.int(2, length(tea_df$longitude))))
opium_df$longitude <- rowSums(cbind(opium_df$longitude, rep.int(2, length(tea_df$longitude))))
# tea_df$longitude <- rowSums(cbind(tea_df$longitude, rep.int(1, length(tea_df$longitude))))


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
circleminsize <- 10000
circlemaxsize <- 100000
tableminvalue <- min(wine_df$frequency)
tablemaxvalue <- max(wine_df$frequency)

f_to_r <- function(tableval, tablemin, tablemax) {
  scaling <- (circlemaxsize - circleminsize) / (tablemax - tablemin)
  return(scaling*(tableval - tablemin) + circleminsize)
}
coffee_df$radius <- sapply(coffee_df$frequency, FUN = f_to_r, tableminvalue, tablemaxvalue)
tea_df$radius <- sapply(tea_df$frequency, FUN = f_to_r, tableminvalue, tablemaxvalue)
wine_df$radius <- sapply(wine_df$frequency, FUN = f_to_r, tableminvalue, tablemaxvalue)
opium_df$radius <- sapply(opium_df$frequency, FUN = f_to_r, tableminvalue, tablemaxvalue)


# MAP

worldmap <- leaflet(wine_df, width = 1600, height = 800) %>%
  #addTiles() %>%
  addProviderTiles(providers$Stamen.Watercolor) %>%  # Stamen watercolor for pseudohistorical look
  addCircles(lat = ~latitude, lng = ~longitude, radius = ~radius, color = "#6d005f") #%>%
  #addMarkers(~longitude, ~latitude, icon = coffeeIcon)
  # addCircles(lng = tea_df$longitude, lat = tea_df$latitude, radius = tea_df$radius, color = "#FFA500") %>%
  # addCircles(lng = wine_df$longitude, lat = wine_df$latitude, radius = wine_df$radius, color = "#6d005f") %>%
  # addCircles(lng = opium_df$longitude, lat = opium_df$latitude, radius = opium_df$radius, color = "#3a0433")

worldmap # Prints the map
# mapshot(worldmap, file = "./Rplot.png")
