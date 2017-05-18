library(jsonlite)

worddf <- read.csv("./ecco_worddiscovery/preliminary_words.csv", header = F)
wordlist <- as.list(worddf$V1)

collocator <- function(word, n) {
  print(word)
  jsondata <- fromJSON(paste("https://vm0542.kaj.pouta.csc.fi/ecco_octavo_api/collocations?query=",
                             word,"&limit=",n,"&maxDocs=-1&localScaling=ABSOLUTE&maxTotalTermFreq=5000000&minTotalTermFreq=5000&timeout=-1", sep = ""))
  jsondata <- jsondata$results$terms
  colnames(jsondata) <- c('term', 'score')
  return(jsondata)
}

n = 21

collocations_master <- collocator(wordlist[[length(wordlist)]], n)
wordlist[length(wordlist)] <- NULL

update_master <- function(collocations_master, word, n) {
  collocations <- collocator(word, n)
  collocations_master <- merge(collocations_master, collocations, by.x = 'term', by.y = 'term', all = T)
  collocations_master$score <- rowSums(collocations_master[, c('score.x', 'score.y')], na.rm = T)
  collocations_master$score.x = NULL
  collocations_master$score.y = NULL
  return(collocations_master)
}

for (word in wordlist) {
  collocations_master <- update_master(collocations_master, as.character(word), n)
}

csv_file <- collocations_master[order(-collocations_master$score), ]
write.csv(csv_file, "./ecco_worddiscovery/medicalterms.csv", row.names = F)