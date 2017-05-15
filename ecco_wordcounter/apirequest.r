library(jsonlite)
word <- "china"
mydata <- fromJSON(paste("https://vm0542.kaj.pouta.csc.fi/ecco_octavo_api/search?query=", word, "&limit=-1&field=ESTCID", sep=""))$results$docs
mydata