size_corpus = object.size(a_corpus) / 1024 / 1024
# size_tokenized_corpus = object.size(tokenized_corpus) / 1024 /1024
size_dfm = object.size(dfm) / 1024 / 1024
size_ngram = object.size(ngram) / 1024 / 1024

ratio_growth = (size_dfm + size_ngram + size_corpus) / size_corpus



print(paste("Tokens:", n_of_tokens))
print(paste("Percentage of Corpus:", fraction * 100, "%"))
print(paste("a_corpus size:", size_corpus))
# print(paste("tokenized_corpus size:", size_tokenized_corpus))
print(paste("dfm size:", size_dfm))
print(paste("ngram size:", size_ngram))
print(paste("growth ratio:",ratio_growth))


gc(verbose = T)
