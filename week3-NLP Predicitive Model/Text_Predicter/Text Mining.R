





init_Predictive_Model_Env <- function()
{
  library(dplyr)
  src.location <- "./"
  
  #write a function that sets up the corpus in a way that it is sized so it can perform in the chosen environment
  conn <- file(paste(src.location, "2-gram.csv", sep = ""), "r")
  file.contents <- read.csv(conn, stringsAsFactors = FALSE)
  bigram <- tbl_df(file.contents)
  close(conn)
  
  conn <- file(paste(src.location, "3-gram.csv", sep = ""), "r")
  file.contents <- read.csv(conn, stringsAsFactors = FALSE)
  trigram <- tbl_df(file.contents)
  close(conn)
  
  conn <- file(paste(src.location, "4-gram.csv", sep = ""), "r")
  file.contents <- read.csv(conn, stringsAsFactors = FALSE)
  quadgram <- tbl_df(file.contents)
  close(conn)
  
  return(list(quadgram, trigram, bigram))
}


predictNextWords <- function(list.of.words, predict.environment)
{
  #Given an existing list of words (1 to n) this predict function calculates the 3 next most likely words based on an existing corpus.
  #I should really bundle the corpus into a class and run predict off of that
  
  #select correct ngram
  next.words <- predict.environment[[1]] %>%
            filter(Input == list.of.words) %>% 
            arrange(desc(Frequency)) %>%
            top_n(n = 3) %>%
            select(Predict) %>%
            top_n(n = 3)    

  if(dim(next.words)[1] == 0) {
    list.of.words <- gsub(".*? (.+)", "\\1", list.of.words)
    next.words <- predict.environment[[2]] %>%
      filter(Input == list.of.words) %>% 
      arrange(desc(Frequency)) %>%
      top_n(n = 3) %>%
      select(Predict) %>%
      top_n(n = 3)   
  }
  
  if(dim(next.words)[1] == 0) {
    list.of.words <- gsub(".*? (.+)", "\\1", list.of.words)
    next.words <- predict.environment[[3]] %>%
      filter(Input == list.of.words) %>% 
      arrange(desc(Frequency)) %>%
      top_n(n = 3) %>%
      select(Predict) %>%
      top_n(n = 3) 
  }
  
  return(list(next.words, list.of.words))
}


