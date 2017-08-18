


CountWords <- function(phrase) {
    phrase <- str_trim(phrase)
    stri_count(phrase,regex = "\\W+") + 1
}

GetLastNWords <- function(phrase, numWords) {
    stringr::word(phrase, -1 * numWords, -1)
}
