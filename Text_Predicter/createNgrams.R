library(quanteda)
library(readtext)
library(dplyr)
library(tidyr)
library(stringi)
library(stringr)
library(RCurl)
source("helpers.R")

main <- function(percentageOfData = 0.01) {
  
  logfile <- "data.log"
  start_time <- Sys.time()
  log_message(logfile, "START")
  
  #Well this stuff should go within the MakePredictMatrix function
  raw_data_location <- "../data/"
  corpus_data_location <- "../data/joined_samples/"
  export_location <- "./"
  ngram_indexes <- seq(2,6,1)
  
  log_message(logfile, "Percentage of data", percentageOfData)

  create_sample_files(raw_data_location, fraction = percentageOfData)
  read_corpus_files(raw_data_location, fraction = percentageOfData)
  log_message(logfile, "Sample files created in ", corpus_data_location)
  
  a_corpus <- create_corpus(corpus_data_location)
  log_message(logfile, "Corpus Created")

  for(i in ngram_indexes) {
    ngram <- create_ngram(a_corpus, i)
    export_ngram(ngram, export_location)
    log_message(logfile, "Created Ngram size:", i)
  }
  
  
  end_time <- Sys.time()
  log_message(logfile, "END")
  log_message(logfile, "Total time: ", format(difftime(end_time, start_time), units = "auto") )
  
}

log_message <- function(logfile, ...) {
  arguments <- paste(list(...), collapse = " ")
  message <- paste(Sys.time(), " -- ", arguments)
  write(message, file = logfile, append = TRUE)
  print(message)
}


#No longer used in this iteration
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

#reads in and creates the sample files all in one
read_corpus_files <- function(data_location, fraction = 0.01){
  require(readr)
sample_location <- paste0(data_location, "joined_samples/joined_sampled.txt")
  
  if(file.exists(sample_location)) file.remove(sample_location)
  
  directory_source <- dir(data_location, ".*txt")
  for (i in directory_source) {
    full_path <- paste0(data_location, i)
    
    print(paste("Loading", full_path))
    filebuffer <- read_lines(full_path)
    
    #make a sample
    set.seed(3413)
    number_of_lines <- length(filebuffer)
    sample_size <- round(number_of_lines * fraction)
    filebuffer <- filebuffer[sample(number_of_lines, sample_size)]

    #write sample    
    write_lines(filebuffer, sample_location, append = TRUE)
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
  require(RCurl)
  require(stringi)
  require(stringr)
  require(data.table)
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
                         concatenator = " "
  )
  

  tokenized_corpus <- removeFeatures(tokenized_corpus, profanity_list)
  
  
  #write dfm to file
  minThresh <- round((10 - n_of_tokens)/2, digits = 0)
  dfm_corpus <- dfm(tokenized_corpus, tolower = TRUE)
  dfm_corpus <- dfm_trim(dfm_corpus, min_count = minThresh)
  
  rm(tokenized_corpus)
  ngram <- data.table(Input = word(dfm_corpus@Dimnames$features,end = -2), 
                      Predict = word(dfm_corpus@Dimnames$features, start = -1),
                      Frequency = dfm_corpus@x)
  
  ngram <- ngram[,head(.SD, 1), by = Input]
  
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

