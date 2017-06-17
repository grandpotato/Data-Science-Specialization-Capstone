source("Text_Predicter/createNgrams.R")
source("Text_Predicter/createNgrams-quanteda.R")

library(lineprof)
data_location <- "../data/"
export_location <- "Text_Predicter/"

sample.size <- 0.0005

tm.run <- lineprof(create_cleaned_corpus(data_location, fraction = sample.size))

quanteda.run <- lineprof(create_cleaned_corpusQ(data_location, fraction = sample.size))

shine(tm.run)
shine(quanteda.run)
alarm()

system.time(create_cleaned_corpusQ(data_location, fraction = sample.size))
system.time(create_cleaned_corpus(data_location, fraction = sample.size))

cleaned_corpus <- create_cleaned_corpus(data_location, fraction = sample.size)

ngram <- create_ngram(cleaned_corpus, quadgram_tokenizer)
export_ngram(ngram, export_location)

ngramq <-create_cleaned_corpusQ(data_location, fraction = sample.size)
ngramq
