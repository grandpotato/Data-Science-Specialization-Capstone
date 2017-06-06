library(tm)
library(dplyr)
library(tidyr)
library(stringi)
library(stringr)
library(RCurl)
library(jsonlite)

#Well this stuff should go within the MakePredictMatrix function
data_location <- "../data/sampled/"
export_location <- "Text_Predicter/"
cleaned_corpus <- create_cleaned_corpus(data_location)

#This should be a class of a list of Matricies...I wouldn't really know how to do that....especially dynamic assignment of memory sizing or depth. questions for another time.
num_ngrams <- 4

ngram <- create_ngram(cleaned_corpus, bigram_tokenizer)
export_ngram(ngram, export_location)

ngram <- create_ngram(cleaned_corpus, trigram_tokenizer)
export_ngram(ngram, export_location)

ngram <- create_ngram(cleaned_corpus, quadgram_tokenizer)
export_ngram(ngram, export_location)

CreateSampledFile <- function(input.file, output.file, fraction = 0.001) {
  #This function randomly samples lines from an inputFile and then exports it into an outputFile
  
  conn <- file(input.file, "r")
  file.contents <- readLines(conn)
  nlines <- length(file.contents )
  close(conn)
  
  conn <- file(output.file, "w")
  selection <- rbinom(nlines, 1, fraction)
  for(i in  1: nlines) {
    if (selection[i]==1) {cat(file.contents [i], file=conn, sep = "\n")}
  }
  close(conn)
  
  paste("Saved", sum(selection), "lines to file.", output.file)
}

create_cleaned_corpus <- function(data_location) {
  #This R function takes a directory of documents and returns a plaintext document map of 1-grams with profanity and filtering. It 
  
  #variables
  profanity_location <- "https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
  raw_profanity_list <- getURL(profanity_location)
  profanity_list <- stri_split(raw_profanity_list, regex = "\\n")[[1]]
  profanity_list <- head(profanity_list,-2)
  
  #Create corpus
  directory_source <- DirSource(data_location, encoding = "UTF-8")
  clean_corpus <- VCorpus(directory_source)   
  
  #clean corpus
  clean_corpus <- tm_map(clean_corpus, removePunctuation)
  clean_corpus <- tm_map(clean_corpus, content_transformer(function(x) iconv(x, to="ASCII", sub=" ")))
  clean_corpus <- tm_map(clean_corpus, removeNumbers)   
  clean_corpus <- tm_map(clean_corpus, content_transformer(tolower))   
  clean_corpus <- tm_map(clean_corpus, removeWords, profanity_list)   
  clean_corpus <- tm_map(clean_corpus, stripWhitespace)  
  
  #stemming corpora
  clean_corpus <- tm_map(clean_corpus, stemDocument) 
  
  return(clean_corpus)
}


bigram_tokenizer <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

trigram_tokenizer <- function(x) unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)

quadgram_tokenizer <- function(x) unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), use.names = FALSE)

ngram_tokenizer <- function(x,n) unlist(lapply(ngrams(words(x), n), paste, collapse = " "), use.names = FALSE)

create_ngram <- function(a_corpus,tokenizer_function) {
  dtm <- DocumentTermMatrix(a_corpus, control = list(tokenize = tokenizer_function))
  
  ngram <- tbl_df(data.frame(Words = dtm$dimnames$Terms, Frequency = colSums(as.matrix(dtm)))) %>% 
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
