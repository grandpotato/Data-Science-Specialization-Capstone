





init_predictive_model_env <- function()
{
  library(dplyr)
  src_location <- "./"
  
  #write a function that sets up the corpus in a way that it is sized so it can perform in the chosen environment
  conn <- file(paste(src_location, "2-gram.csv", sep = ""), "r")
  file.contents <- read.csv(conn, stringsAsFactors = FALSE)
  bigram <- tbl_df(file.contents)
  close(conn)
  
  conn <- file(paste(src_location, "3-gram.csv", sep = ""), "r")
  file.contents <- read.csv(conn, stringsAsFactors = FALSE)
  trigram <- tbl_df(file.contents)
  close(conn)
  
  conn <- file(paste(src_location, "4-gram.csv", sep = ""), "r")
  file.contents <- read.csv(conn, stringsAsFactors = FALSE)
  quadgram <- tbl_df(file.contents)
  close(conn)
  
  conn <- file(paste(src_location, "5-gram.csv", sep = ""), "r")
  file.contents <- read.csv(conn, stringsAsFactors = FALSE)
  quingram <- tbl_df(file.contents)
  close(conn)
  
  conn <- file(paste(src_location, "6-gram.csv", sep = ""), "r")
  file.contents <- read.csv(conn, stringsAsFactors = FALSE)
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
  

  return(list(next_words, list_of_words))
}


