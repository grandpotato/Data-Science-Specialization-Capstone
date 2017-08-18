

init_predictive_model_env <- function()
{
  library(dplyr)
  src_location <- "./"
  Sys.setlocale("LC_ALL", "C")
  
  #write a function that sets up the corpus in a way that it is sized so it can perform in the chosen environment
  conn <- file(paste(src_location, "2-gram.csv", sep = ""), "r")
  file.contents <- read.csv(conn, header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  bigram <- tbl_df(file.contents)
  close(conn)
  
  conn <- file(paste(src_location, "3-gram.csv", sep = ""), "r")
  file.contents <- read.csv(conn, header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  trigram <- tbl_df(file.contents)
  close(conn)
  
  conn <- file(paste(src_location, "4-gram.csv", sep = ""), "r")
  file.contents <- read.csv(conn, header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  quadgram <- tbl_df(file.contents)
  close(conn)
  
  conn <- file(paste(src_location, "5-gram.csv", sep = ""), "r")
  file.contents <- read.csv(conn, header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  quingram <- tbl_df(file.contents)
  close(conn)
  
  conn <- file(paste(src_location, "6-gram.csv", sep = ""), "r")
  file.contents <- read.csv(conn, header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  sexgram <- tbl_df(file.contents)
  close(conn)
  
  return(list(sexgram, quingram, quadgram, trigram, bigram))
}


predict_next_words <- function(list_of_words, predict_environment)
{

  for(ngram in predict_environment) {
    next_words <- ngram %>%
              filter(Input == list_of_words) %>%
              arrange(desc(Frequency)) %>%
              select(Predict) 
    list_of_words <- gsub(".*? (.+)", "\\1", list_of_words)
    if (dim(next_words)[1] != 0) break 
  }
  if(dim(next_words)[[1]] == 0) { 
      next_words <- predict_environment[[length(predict_environment)]] %>% 
        top_n(1, Frequency) %>%
        select(Predict)
      list_of_words <- "!!!NOT MATCHED!!!"
  
  }
  return(list(next_words, list_of_words))
}


