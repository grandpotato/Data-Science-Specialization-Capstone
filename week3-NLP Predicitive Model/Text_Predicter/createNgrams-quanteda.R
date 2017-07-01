library(quanteda)
library(dplyr)
library(tidyr)
library(stringi)
library(stringr)
library(RCurl)
library(jsonlite)

source("Text_Predicter/helpers.R")

#Well this stuff should go within the MakePredictMatrix function
# data_location <- "../data/"
# export_location <- "Text_Predicter/"
# cleaned_corpus <- create_cleaned_corpus(data_location, fraction = 0.1)
# 
# 
# 
# #This should be a class of a list of Matricies...I wouldn't really know how to do that....especially dynamic assignment of memory sizing or depth. questions for another time.
# 
# ngram <- create_ngram(cleaned_corpus, bigram_tokenizer)
# export_ngram(ngram, export_location)
# 
# ngram <- create_ngram(cleaned_corpus, trigram_tokenizer)
# export_ngram(ngram, export_location)
# 
# ngram <- create_ngram(cleaned_corpus, quadgram_tokenizer)
# export_ngram(ngram, export_location)

#functions Below


create_corpusQ <- function(data_location, fraction = 0.0001) {
  #This R function takes a directory of documents and returns a plaintext document map of 1-grams with profanity and filtering. It 
  
  #variables
  profanity_location <- "https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
  raw_profanity_list <- getURL(profanity_location)
  profanity_list <- stri_split(raw_profanity_list, regex = "\\n")[[1]]
  profanity_list <- head(profanity_list,-2)
  
  #Create corpus
  directory_source <- dir(data_location)
  directory_source <- directory_source[grepl(".*.txt",directory_source)] # remove the non-text files
  
  for (i in directory_source) {
    conn <- file(paste(data_location, i, sep = ""),"r")
    filebuffer <- readLines(conn, encoding="UTF-8", skipNul = TRUE, warn = FALSE)
    close(conn)
    
    set.seed(3413)
    sampled_buffer <- sample(filebuffer, size = round(length(filebuffer) * fraction, digits = 0))
    sample_corpus <- corpus(sampled_buffer)
    if (!exists("clean_corpus")) {
      clean_corpus <- sample_corpus
    } else {
      clean_corpus <- clean_corpus + sample_corpus
    }
    
  }


  
  # removeFeatures()
  return(clean_corpus)
}


bigram_tokenizer <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

trigram_tokenizer <- function(x) unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)

quadgram_tokenizer <- function(x) unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), use.names = FALSE)

ngram_tokenizer <- function(x,n) unlist(lapply(ngrams(words(x), n), paste, collapse = " "), use.names = FALSE)

create_ngram <- function(a_corpus, n_of_tokens) {
  tokenized_corpus <- tokens(a_corpus,
                         remove_numbers = TRUE,
                         remove_punct = TRUE,
                         remove_symbols = TRUE,
                         remove_separators = TRUE,
                         remove_twitter = TRUE,
                         remove_url = TRUE,
                         ngrams = n_of_tokens
  )
  dfm <- dfm(a_corpus, tolower = TRUE)
  
  
  
  ngram <- tbl_df(data.frame(Words = dfm$dimnames$Terms, Frequency = colSums(as.matrix(dfm)))) %>% 
    arrange(Words, desc(Frequency)) %>% 
    mutate(Frequency = Frequency/sum(Frequency)) %>%
    extract(Words,into = c("Input", "Predict"), '(.*)\\s+([^ ]+)$')
  return(ngram)
}

export_ngram <- function(ngram, destination) {
  library("jsonlite")
  
  json <- toJSON(ngram, digits = NA)
  validate(json)
  
  filename <- paste(destination, CountWords(head(ngram)$Input[1]) + 1,"-gram.json", sep ="")
  
  #write to file
  write(json, file = filename)
  
}
