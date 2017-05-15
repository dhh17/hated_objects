library(jsonlite)
library(ggplot2)

mycsv <- read.csv("estc_processed_080517.csv")

cleaner <- function(id) {
  return(gsub("^([A-Z])0*([1-9][0-9]*)$","\\1\\2",id))
}

counter <- function(word) {
  mydata <- fromJSON(paste("https://vm0542.kaj.pouta.csc.fi/ecco_octavo_api/search?query=", word, "&limit=-1&field=ESTCID&field=documentLength", sep=""))$results$docs
  publication_year <- mycsv[match(sapply(mydata$ESTCID, cleaner), mycsv$id), 'publication_year']
  return(cbind(mydata, publication_year))
}

finaldata <- counter("china")
finaldatalimited <- subset(finaldata, publication_year >= 1700 & publication_year <= 1800)
ggplot(data=finaldatalimited, aes(publication_year, score)) + geom_bar(stat="identity")