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
summary <- read.csv("C:/Users/Iiro/Documents/Hackaton/yearly_ecco_summaries.csv")
qantity_per_year <- summary$characters


# finaldata$freq <- finaldata$score / finaldata$documentLength 
finaldatalimited <- subset(finaldata, publication_year >= 1700 & publication_year <= 1800)
ggplot(data=finaldatalimited, aes(publication_year, freq)) + geom_bar(stat="identity")
#Kaikkivuonnajulkaistu <- aggregate(data = to
finaldatalimited2 <- subset(finaldata, publication_year>=1700 & publication_year<1800)
scoreperyear = aggregate(data=finaldatalimited2, score~publication_year, FUN=sum)
relativescore <- c(scoreperyear$score)/c(summary$characters)
years <- seq(from=1700, to=1799, by=1)
together <- data.frame(relativescore,years)

visualizer <- function(basedata, searchedterm){
  searchedterm <- as.character(searchedterm)
  basedatalimited <- subset(basedata, basedata$publication_year >=1700 & basedata$publication_year < 1800)
  scorepery <- aggregate(data=basedatalimited, score~publication_year, FUN=sum)
  
  scorepery
  info1 <- c("Relative occurence of")
  info2 <- searchedterm
  info3 <- c("from 1700 to 1799")
  relativesco <- (c(scorepery$score)/c(summary$characters))*1000000
  
  
  barplot(relativesco, main = paste(info1,info2,info3,sep=" "),xlab= "years starting from 1700", ylab="relative score")
 
  
}

Count_and_visualize <- function(term){
  term = as.character(term)
  basedata <- counter(term)
  visualizer(basedata, term)
  
}

Experimental_visualizer <- function(term){
  scorestotal <- integer(100)
  basedata <- counter(term)
  for(i in 1700:1799){
    scoresofyear <- c(basedata$score[basedata$publication_year==i])
    biggest_value <- max(scoresofyear)
    scorestotal[i-1699] <- sum(scoresofyear,na.rm=TRUE)-biggest_value
  }
  relartivescore <- (scorestotal/(summary$characters))*1000000
  info1 <- c("Relative occurence of")
  info2 <- term
  info3 <- c("from 1700 to 1799, biggest scores removed")
  barplot(relativescore,main=paste(info1,info2,info3,sep=" "),xlab="years starting from 1700" )
  }


sumwithoutmax <- function(vector){
  
  bigestvalue <- max(vector)
  sumofvalues <- sum(vector)
  return(sumofvalues-bigestavalue)
}
scorperywithoutmax <- aggregate(data=basedatalimited, score~publication_year, FUN=sumwithoutmax)
barplot(relativescowithoutmax, main = paste(info1,info2,info3,sep=" "),xlab="years")
relativescowithoutmax <- (c(scorperywithoutmax$score)/c(summary$characters))*1000000    

