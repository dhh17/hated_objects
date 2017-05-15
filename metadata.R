library(jsonlite)

estc_metadata <- read.csv("../data/estc_processed_080517.csv")
ecco_metadata <- fromJSON("../data/api3dump.json")

querydata <- fromJSON("https://vm0542.kaj.pouta.csc.fi/ecco_octavo_api/search?query=king&pretty=&timeout=-1&field=ESTCID")$results$docs

querydata$year <-
  estc_metadata[match(querydata$ESTCID, estc_metadata$id),"publication_year"]

query <- "https://vm0542.kaj.pouta.csc.fi/ecco_octavo_api/search?query=<DOCUMENT§china§DOCUMENT>&pretty=&timeout=-1&field=ESTCID"

geo_names <- fromJSON("../data/tgn_7000874.json")
g2 <- geo_names$results$bindings

cities <- read.csv("../data/worldcitiespop.csv")
cities_matti <- read.csv("../data/dictionary_geo_entry.csv")
cities_matti_unique <- unique(cities_matti)
