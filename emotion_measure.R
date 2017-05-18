library(jsonlite)


get_search_results_for_term <- function(search_term) {
  api_request <- paste0("https://vm0542.kaj.pouta.csc.fi/ecco_octavo_api/search?query=",
                        search_term, "&pretty&limit=-1&field=ESTCID&field=content&timeout=-1")
  
  search_results <- jsonlite::fromJSON(api_request)$results$docs
  print(paste0("search_results_length: ", length(search_results$content)))
  return(search_results)
}


get_tokenlist <- function(search_results) {
  tokenlist <- list()
  i <- 1
  for (result_row in search_results$content) {
    # print(result_row)
    result_row <- as.character(result_row)
    result_row <- gsub("\\.", " ", result_row)
    result_row <- gsub(",", " ", result_row)
    result_row <- gsub("coffee", "", result_row)
    result_row <- gsub("tea", "", result_row)
    result_row <- gsub("wine", "", result_row)
    # result_row <- gsub("wine", "", result_row)
    result_row <- gsub("  ", " ", result_row)
    tokens <- strsplit(result_row, " ")
    
    tokens <- lapply(tokens, tolower)
    tokens <- unlist(tokens)
    
    if (length(tokens) < 6) {
      # print(result_row)
      print(i)
      # break()
      tokens <- list("")
    } else {
      tokens <- list(tokens)
    }
    tokenlist[i] <- tokens
    i <- i + 1
    if (i %% 1000 == 0) {
      print(paste0("tokenized ", i))
    }
  }
  print(length(tokenlist))
  return(tokenlist)
}


get_termlist <- function(datafile = "data/emotions.csv") {
  termlist <- read.csv(datafile)
  termlist$emotion <- as.character(termlist$term)
  termlist$strength_abs <- abs(termlist$score)
  return(termlist)
}


get_wordhitlist <- function(tokenlist, termlist) {
  wordhitlist <- list()
  i <- 1
  for (tokengroup in tokenlist) {
    wordhits <- 0
    for (token in tokengroup) {
      if (token %in% termlist$emotion) {
        strength <- (subset(termlist, emotion == token))$strength_abs[[1]]
        wordhits <- wordhits + strength
        ntokens <- length(unlist(tokengroup))
        wordhits <- (wordhits / ntokens) * 10
        if (ntokens < 20) {
          wordhits <- wordhits / 2
        }
      }
    }
    wordhitlist[i] <- wordhits
    i <- i + 1
    if (i %% 1000 == 0) {
      print(paste0("emotionalized ", i))
      print(ntokens)
    }
  }
  return(wordhitlist)
}


# emotion_list <- read.csv("../data/medicalterms.csv")
# emotion_list$emotion <- as.character(emotion_list$term)
# emotion_list$strength_abs <- abs(emotion_list$score)

query_word <- "tobacco"
print(paste0("querying for: ", query_word))
search_results <- get_search_results_for_term(query_word)
tokenlist <- get_tokenlist(search_results)
print(paste0("querying for: ", query_word))
termlist <- get_termlist(datafile = "data/medicalterms.csv")
print(paste0("querying for: ", query_word))
wordhitlist <- get_wordhitlist(tokenlist, termlist)
print(paste0("querying for: ", query_word))

paragraph_weights <- search_results
paragraph_weights$hits <- wordhitlist
paragraph_weights$hits <- as.numeric(paragraph_weights$hits) 
paragraph_weights$content <- as.character(paragraph_weights$content)
paragraph_weights_ordered <- paragraph_weights[with(paragraph_weights, order(-hits)), ]

write.csv(x = paragraph_weights_ordered,
          file = paste0("output/", query_word, "_medicality.csv"),
          row.names = FALSE)
