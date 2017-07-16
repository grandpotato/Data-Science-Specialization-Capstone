print("Running")

data_location <- "../data/"
fraction <- 0.01
n_of_tokens <- 2

a_corpus <- create_corpusQ(data_location, fraction = fraction)

tokenized_corpus <- tokens(a_corpus,
                           remove_numbers = TRUE,
                           remove_punct = TRUE,
                           remove_symbols = TRUE,
                           remove_separators = TRUE,
                           remove_twitter = TRUE,
                           remove_url = TRUE,
                           ngrams = n_of_tokens
)




dfm <- dfm(tokenized_corpus, tolower = TRUE)
dfm <- dfm_trim(dfm, min_count = 4)
dfm <- dfm_weight(dfm, type = c("frequency"))


rm(tokenized_corpus)

gc()

ngram <- tbl_df(data.frame(Words = dfm@Dimnames$features, Frequency = colSums(as.matrix(dfm)))) %>% 
  arrange(Words, desc(Frequency)) %>% 
  mutate(Frequency = Frequency/sum(Frequency)) %>%
  extract(Words,into = c("Input", "Predict"), '(.*)_([^ ]+)$')


data_location

readLines()

sample()
  #combine samples from each of the texts into a single file.

write.csv()

readtext()

corpus()

dfm
make dfm
