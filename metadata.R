library(jsonlite)

cleaner <- function(id) {
  return(gsub("^([A-Z])0*([1-9][0-9]*)$","\\1\\2",id))
}

estc_metadata <- read.csv("../data/estc_processed_080517.csv")
ecco_metadata <- fromJSON("../data/api3dump.json")


ecco_metadata$pub_year <-
  estc_metadata[match(cleaner(ecco_metadata$ESTCID),
                      estc_metadata$id),"publication_year"]


retdata <- data.frame(year = numeric(0), pages = numeric(0),
                      paragraphs = numeric(0), characters = numeric(0))

for (qyear in 1700:1799) {
  # print(year)
  year_subset <- subset(ecco_metadata, ecco_metadata$pub_year == qyear)
  pages_sum = sum(year_subset$totalPages)
  para_sum = sum(year_subset$totalParagraphs)
  char_sum = sum(year_subset$documentLength)

  year_df <- data.frame(year = qyear, pages = pages_sum,
                        paragraphs = para_sum, characters = char_sum)
  retdata <- rbind(retdata, year_df)
    
}

write.csv(retdata, "yearly_ecco_summaries.csv", row.names = F)

