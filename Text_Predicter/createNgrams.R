library(quanteda)
library(readtext)
library(dplyr)
library(tidyr)
library(stringi)
library(stringr)
library(RCurl)
source("helpers.R")

main <- function() {
  
  start_time <- Sys.time()
  
  print(paste0("Time started: ", start_time))
  #Well this stuff should go within the MakePredictMatrix function
  raw_data_location <- "../data/"
  corpus_data_location <- "../data/samples/"
  export_location <- "./"
  
  ngram_indexes <- seq(2,6,1)
  
  create_sample_files(raw_data_location, fraction = 0.1)
  a_corpus <- create_corpus(corpus_data_location)
  
  
  #This should be a class of a list of Matricies...I wouldn't really know how to do that....especially dynamic assignment of memory sizing or depth. questions for another time.
  

  for(i in ngram_indexes) {
    ngram <- create_ngram(a_corpus, i)
    export_ngram(ngram, export_location)
  }
  
  end_time <- Sys.time()
  print(paste0("Time ended: ", end_time))
  print(paste0("Total time: ", difftime(end_time, start_time)))
  
}


create_sample_files <- function(data_location, fraction = 0.001) {

  directory_source <- dir(data_location, ".*txt")
  for (i in directory_source) {
    full_path <- paste0(data_location, i)
    filebuffer <- read.table(full_path, 
                           header = FALSE, 
                           sep = "\n",
                           stringsAsFactors = FALSE, 
                           encoding = "UTF-8",
                           quote = "",
                           skipNul = TRUE
                           
                           )
    
    set.seed(3413)
    number_of_lines <- nrow(filebuffer)
    sample_size <- round(number_of_lines * fraction)
    filebuffer <- filebuffer[sample(number_of_lines, sample_size),]

    sample_location <- paste0(data_location, "samples/sample_", i)
    
    conn <- file(sample_location, "w")
    write.table(filebuffer, 
              file = conn,
              quote = FALSE,
              sep = "\n",
              row.names = FALSE,
              col.names = FALSE,
              fileEncoding = "UTF-8"
              )
    close(conn)
  }
  
}


create_corpus <- function(data_location) {
  require(readtext)
  require(quanteda)
  #This R function takes a directory of documents and returns a corpus
    #Create corpus
  directory_source <- paste(data_location, dir(data_location, ".*txt"), sep = "")
  
  text_corpus <- corpus(readtext(directory_source, encoding = "latin1"))
  
  return(text_corpus)
}

create_ngram <- function(a_corpus, n_of_tokens) {
  require(quanteda)
  #variables
  profanity_location <- "https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
  raw_profanity_list <- getURL(profanity_location)
  profanity_list <- stri_split(raw_profanity_list, regex = "\\n")[[1]]
  profanity_list <- head(profanity_list,-2)
  
  #write tokens to file
  tokenized_corpus <- tokens(a_corpus,
                         remove_numbers = TRUE,
                         remove_punct = TRUE,
                         remove_symbols = TRUE,
                         remove_separators = TRUE,
                         remove_twitter = TRUE,
                         remove_url = TRUE,
                         ngrams = n_of_tokens,
                         concatenator = "_"
  )
  

  tokenized_corpus <- removeFeatures(tokenized_corpus, profanity_list)
  
  
  #write dfm to file
  dfm_corpus <- dfm(tokenized_corpus, tolower = TRUE)
  #dfm <- dfm_trim(dfm, min_count = 4)
  
  
  ngram <- tbl_df(data.frame(Words = dfm_corpus@Dimnames$features, Frequency = colSums(as.matrix(dfm_corpus)), stringsAsFactors = FALSE)) %>%
     filter(Frequency > 4) %>%
     arrange(Words, desc(Frequency)) %>%
     mutate(Frequency = Frequency/sum(Frequency)) %>%
     extract(Words,into = c("Input", "Predict"), '(.*)_([^ ]+)$') %>%
     mutate(Input = stri_replace_all(Input, " ", regex = "_"))
  return(ngram)
}

#CSV implementation
export_ngram <- function(ngram, destination) {
  
  
  filename <- paste(destination, CountWords(head(ngram)$Input[1]) + 1,"-gram.csv", sep ="")
  
  conn <- file(filename, "w")
  #write to file
  write.csv(ngram, file = filename, row.names = FALSE)
  close(conn)
}

