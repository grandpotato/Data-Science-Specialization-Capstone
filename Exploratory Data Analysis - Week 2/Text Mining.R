
create.sampled.file <- function(inputFile, outputFile, fraction = 0.001) {
    #This function randomly samples lines from an inputFile and then exports it into an outputFile
    
    conn <- file(inputFile, "r")
    fileContents <- readLines(conn)
    nlines <- length(fileContents)
    close(conn)
    
    conn <- file(outputFile, "w")
    selection <- rbinom(nlines, 1, fraction)
    for(i in  1: nlines) {
        if (selection[i]==1) {cat(fileContents[i], file=conn, sep = "\n")}
    }
    close(conn)
    
    paste("Saved", sum(selection), "lines to file.", outputFile)
}


make.TMMap <- function(dataLoc) {
    #This R function takes a directory of documents and returns a plaintext document map of 1-grams with profanity and filtering. It 
        
    #LOAD PACKAGES
    #Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
    #install.packages(Needed, dependencies=TRUE)   
    #install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")    
    
    # setvariables
    cname <- file.path(dataLoc)   
    
    #LOAD DATA
    library(tm)   
    docs <- Corpus(DirSource(cname))   
    
    profanity <- readLines("C:/Users/Kevin/Documents/GitHub/Temp/profanity.txt")
    #Confirm all files are available
    
    # PREPROCESSING
    #I'm not here to predict punctuation. Although it is possible.
    docs <- tm_map(docs, removePunctuation) 
    
    #remove common characters in emails
    for(j in seq(docs))   
        {   
            docs[[j]] <- gsub("/", " ", docs[[j]])   
            docs[[j]] <- gsub("@", " ", docs[[j]])   
            docs[[j]] <- gsub("\\|", " ", docs[[j]])   
        }   
    
    #cuz w3'd h8 to be predictn 1337 5P34K which would just ruin things. Also it kinda forces language to drift slower. More inertia.
    docs <- tm_map(docs, removeNumbers)   
    
    #We don't want words to be separated just because they're capitalized. Although the 'US' (America) becoming 'us' is a problem
    docs <- tm_map(docs, tolower)   
    
    #Its true that this might work against me in text prediction. But ... meh I can take this out later. It'll be better for context analysis
    docs <- tm_map(docs, removeWords, stopwords("english")) 
    
    #remove swearing. 
    docs <- tm_map(docs, removeWords, profanity)   
    
    #stemming corpora
    docs <- tm_map(docs, stemDocument) 
    
    #remove whitespace
    docs <- tm_map(docs, stripWhitespace)  
    
    #Return a plain text document
    tm_map(docs, PlainTextDocument)   
}
