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


MakeTMMap <- function(dataLoc) {
    #This R function takes a directory of documents and returns a plaintext document map of 1-grams with profanity and filtering. It 
    
    #LOAD PACKAGES
    #Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
    #install.packages(Needed, dependencies=TRUE)   
    #install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")    
    
    # setvariables
    cname <- file.path(dataLoc)   
    
    #LOAD DATA
    library(tm)   
    docs <- VCorpus(DirSource(cname))   
    
    profanity <- readLines("../../data/reference/profanity.txt", encoding = "UTF-8")

    #PROCESSING
    docs <- tm_map(docs, removePunctuation)
    docs <- tm_map(docs, removeNumbers)   
    docs <- tm_map(docs, content_transformer(tolower))   
    docs <- tm_map(docs, removeWords, profanity)   
    docs <- tm_map(docs, stripWhitespace)  
    
    #stemming corpora
    docs <- tm_map(docs, stemDocument) 
    
    docs
}

# require(RWeka)
# BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
# TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

BigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

TrigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)


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


