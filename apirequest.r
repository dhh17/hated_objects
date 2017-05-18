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
  yearcount <- seq(from=1700,to=1799,by=1)
  aswords <- as.character(yearcount)
  
  barplot(relativesco, main = paste(info1,info2,info3,sep=" "),names.arg=aswords, ylab="relative score")
 
  
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
    subdata <- subset(x=basedata, publication_year==i)
    scores <- c(subdata$score)
    maxscore <- max(scores)
    sumofall <- sum(scores)
    scorestotal[i-1699] <- sumofall-maxscore
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

countbyx <- function(data,x){
  integer <- trunc(length(data)/x)
  if(length(data)-integer*x>0){
    v=integer(integer+1)  
  }else{
    v=integer(integer)  
  }
  a <- c(data)
  for(i in 0:integer-1){
    start <- i*x+1
    end <- (i+1)*x
    stacked <- sum(a[start:end])   
    v[i+1] <- stacked
  }
  if(length(data)-integer*x>0){
    v[integer+1] <- sum(a[integer+1:length(data)])  
  }
  return(v)
}

visualizer_stackbars <- function(term,x=10){
  basedata <- counter(term)
  basedatalimited <- subset(basedata, basedata$publication_year >=1700 & basedata$publication_year < 1800)
  scorepery <- aggregate(data=basedatalimited, score~publication_year, FUN=sum)
  info1 <- c("Relative occurence of")
  info2 <- term
  info3 <- c("from 1700 to 1799")
  relativesco <- (c(scorepery$score)/c(summary$characters))*1000000
  h <- countbyx(relativesco,10)
  xlabexpla <- paste(c("1700-1799. Bar="),x,"year",by="")
  barplot(h, main = paste(info1,info2,info3,sep=" "),names.arg=xlabexpla, ylab="relative score")
  
  
}

Visualizer_comparingtopages <- function(term, x=1){
term = as.character(term)
basedata <- counter(term)
scorestotal <- integer(100)
basedatalimited <- subset(basedata, basedata$publication_year >=1700 & basedata$publication_year < 1800)
for(i in 1700:1799){
  subdata <- subset(x=basedata, publication_year==i)
  scores <- c(subdata$score)
  sumofall <- sum(scores)
  scorestotal[i-1699] <- sumofall
}
totalpages <- summary$pages
info1 <- c("Relative occurence of")
info2 <- searchedterm
info3 <- c("from 1700 to 1799")
relativesco <- scorestotal/totalpages
stackedbars <- countbyx(relativesco)
yearcount <- seq(from=1700,to=1799,by=1)
aswords <- as.character(yearcount)
barplot()
}

Csvcreator <- function(term){
  data <- counter(term)
  write.csv(data,paste(term,".csv",by=""))
}


counterv2 <- function(word,word2) {
  mydata <- fromJSON(paste("https://vm0542.kaj.pouta.csc.fi/ecco_octavo_api/search?query=%2B", word,"%20%2B",word2, "&limit=-1&field=ESTCID&field=documentLength", sep=""))$results$docs
  publication_year <- mycsv[match(sapply(mydata$ESTCID, cleaner), mycsv$id), 'publication_year']
  return(cbind(mydata, publication_year))
}

Measureofcloseness <- function(term1,term2){
  
  firstword <- as.character(term1)
  secondword <- as.character(term2)
  basedata1 <- counter(firstword)
  basedata2 <- counter(secondword)
  basedatacombined <- counterv2(firstword,secondword)
  paragraphs1 <- length(basedata1$score)
  paragraphs2 <- length(basedata2$score)
  paragraphsboth <- length(basedatacombined$score)
  hypotheticalmax <- min(paragraphs1,paragraphs2)
  Proportion_of_max <- paragraphsboth/hypotheticalmax
  return(Proportion_of_max)
  }

Closenessdata <- function(term1,term2){
  firstword <- as.character(term1)
  secondword <- as.character(term2)
  basedata1 <- counter(firstword)
  basedata2 <- counter(secondword)
  basedatacombined <- counterv2(firstword,secondword)
  paragraphs1 <- length(basedata1$score)
  paragraphs2 <- length(basedata2$score)
  paragraphsboth <- length(basedatacombined$score)    
  values <- c(paragraphs1,paragraphs2,paragraphsboth)
  return(values)
  
  }
Alldatacreator <- function(term1,term2){
  firstword <- as.character(term1)
  secondword <- as.character(term2)
  basedata1 <- counter(firstword)
  basedata2 <- counter(secondword)
  basedatacombined <- counterv2(firstword,secondword)
  write.csv(basedata1,paste(firsword,".csv",by=""))
  write.csv(basedata2,paste(secondword,".csv",by=""))
  write.csv(basedatacombined,paste(term1,"and",term2,"occurring together",".csv",by=""))
  
}

counterv3 <- function(term1,term2,term3){
  mydata <- fromJSON(paste("https://vm0542.kaj.pouta.csc.fi/ecco_octavo_api/search?query=","%2B","%22",term1,"%20",term2,"%22","%20%2B",term3, "&limit=-1&field=ESTCID&field=documentLength", sep=""))$results$docs    
  publication_year <- mycsv[match(sapply(mydata$ESTCID, cleaner), mycsv$id), 'publication_year']
  return(cbind(mydata, publication_year))
}
counterforcombinedwords <- function(term1,term2){
  mydata <- fromJSON(paste("https://vm0542.kaj.pouta.csc.fi/ecco_octavo_api/search?query=","%22",term1,"%20",term2,"%22","&limit=-1&field=ESTCID&field=documentLength", sep=""))$results$docs      
  publication_year <- mycsv[match(sapply(mydata$ESTCID, cleaner), mycsv$id), 'publication_year']
  return(cbind(mydata, publication_year))
}
Measurev2 <- function(term1,term2,term3){
  data1 <- counterv3(term1,term2,term3)
  data2 <- counterforcombinedwords(term1,term2)
  data3 <- counter(term3)
  paragraphsboth <- length(data1$score)
  paragraphs1 <- length(data3$score)
  paragraphs2 <- length(data2$score)
  hypotheticalmax <- min(paragraphs1,paragraphs2)
  Proportion_of_max <- paragraphsboth/hypotheticalmax
  return(Proportion_of_max)
}
Measurev3 <- function(term1,term2,term3){
  data1 <- counterv3(term1,term2,term3)
  data2 <- counterforcombinedwords(term1,term2)
  data3 <- counter(term3)
  paragraphsboth <- length(data1$score)
  paragraphs1 <- length(data3$score)
  paragraphs2 <- length(data2$score)
  hypotheticalmax <- min(paragraphs1,paragraphs2)
  Proportion_of_max <- paragraphsboth/hypotheticalmax
  write.csv(data3,paste(term3,".csv",by=""))
  write.csv(data2,paste(term1," ",term2,".csv",by=""))
  write.csv(data1,paste(term1," ",term2,"and ",term3,"occurring together",".csv",by=""))
  return(Proportion_of_max)
}

Visualizer_for_emotions <- function(data,word){
  datarightyears <- subset(data,data$publication_year >=1700 & data$publication_year<1800)  
  paragraphsperyr <- integer(100)
  for(i in 1700:1799){
    rightparagrahphs <- subset(data, data$publication_year==i)
    paragraphsperyr[i-1699] <- length(rightparagrahphs$score)  
    }
  
  emotionsperyear <- aggregate(data=datarightyears,emotionality~publication_year,FUN=sum)
  emotionalityv <- emotionsperyear$emotionality
  relativescore <- emotionalityv/paragraphsperyr
  relative_stacked <- countbyx(relativescore,10)
  relative_lowest <- min(relative_stacked)
  smallestaszero <- relative_stacked - relative_lowest
  yearcount <- seq(from=1700,to=1799,by=10)
  aswords <- as.character(yearcount)
  barplot(height=smallestaszero,names.arg=aswords) 
}

Createdatawithyears <- function(data){
  publication_year <- publication_year <- mycsv[match(sapply(data$ESTCID, cleaner), mycsv$id), 'publication_year']  
  return(cbind(data,publication_year))
}

Absoluteparagraphs <- function(data,term){
  datarightyears <- subset(data,data$publication_year >=1700 & data$publication_year<1800)  
  paragraphsperyr <- integer(100)
  for(i in 1700:1799){
    rightparagrahphs <- subset(data, data$publication_year==i)
    paragraphsperyr[i-1699] <- length(rightparagrahphs$score)  
  }
  stacked <- countbyx(paragraphsperyr,10)
  barplot(stacked)

  
}

Giverelativescore <- function(data){
  datarightyears <- subset(data,data$publication_year >=1700 & data$publication_year<1800)  
  paragraphsperyr <- integer(100)
  emotionsperyear <- integer(100)
  for(i in 1700:1799){
    rightparagrahphs <- subset(datarightyears, datarightyears$publication_year==i)
    paragraphsperyr[i-1699] <- length(rightparagrahphs$score)  
    emotionsfromtheyear <- rightparagrahphs$emotionality
    emotionsperyear[i-1699] <- sum(emotionsfromtheyear)
    }
  
  return(emotionsperyear/paragraphsperyr)
}

Combinedplot <- function(data1,data2){
  combined_data <- data.frame(data1,data2,date=seq(as.Date("1700-01-01"),by="1 year",length.out=100))  
  data1name <- as.character(data1)
  data2name <- as.character(data2)
  ggplot(combined_data, aes(date)) + 
    geom_line(aes(y = data1, colour = "blue")) + 
    geom_line(aes(y = data2, colour = "green"))
}

Giverelativescore_csv <- function(data,name){
  datarightyears <- subset(data,data$publication_year >=1700 & data$publication_year<1800)  
  paragraphsperyr <- integer(100)
  emotionsperyear <- integer(100)
  for(i in 1700:1799){
    rightparagrahphs <- subset(datarightyears, datarightyears$publication_year==i)
    paragraphsperyr[i-1699] <- length(rightparagrahphs$score)  
    emotionsfromtheyear <- rightparagrahphs$emotionality
    emotionsperyear[i-1699] <- sum(emotionsfromtheyear)
  }
  years <- seq(from=1700, to=1799,by=1)
  relativescore <- emotionsperyear/paragraphsperyr
  dataforcsv <- data.frame(relativescore,years)
  write.csv(x=dataforcsv,file=name)
}

Combinedplot2 <- function(data1,data2){
  combined_data <- data.frame(data1,data2,date=seq(as.Date("1700-01-01"),by="1 year",length.out=100))    
  long_version <- melt(combined_data,id="date")
  ggplot(data=long_version,
         aes(x=date, y=value, colour=variable)) +
    geom_line()
}

Combinedplot3 <- function(data1,data2){
  combined_data <- data.frame(data1,data2,date=seq(as.Date("1700-01-01"),by="1 year",length.out=100))          
  stacked <- with(combined_data,
                  data.frame(value = c(data1, data2),
                             variable = factor(rep(c("Var0","Var1"),
                                                   each = NROW(combined_data))),
                             Dates = rep(Dates, 2)))
  require(ggplot2)
  p <- ggplot(stacked, aes(Dates, value, colour = variable))
  p + geom_line()
  
}