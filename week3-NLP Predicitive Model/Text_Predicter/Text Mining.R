





init_Predictive_Model_Env <- function(size, coverage = NULL)
{
  #write a function that sets up the corpus in a way that it is sized so it can perform in the chosen environment
}


predictNextWords <- function(list.of.words, predict.environment)
{
  #Given an existing list of words (1 to n) this predict function calculates the 3 next most likely words based on an existing corpus.
  #I should really bundle the corpus into a class and run predict off of that
  
  #select correct ngram
  next.words <- tbl_df(predict.environment) %>%
            filter(Input == list.of.words) %>% 
            arrange(desc(Frequency)) %>%
            top_n(n = 3, wt = Frequency) %>%
            select(Predict)
                     
  return(next.words)
}


