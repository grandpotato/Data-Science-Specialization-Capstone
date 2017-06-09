source("Text_Predicter/createNgrams.R")

library(lineprof)
data_location <- "../data/"
export_location <- "Text_Predicter/"

l001 <- lineprof(create_cleaned_corpus(data_location, fraction = 0.01))

l0001 <- lineprof(create_cleaned_corpus(data_location, fraction = 0.001))
