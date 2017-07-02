setClass("predictionCorpus",
         slots = list(source = "character",
                      corpus = "Corpus"
                      ),
         validity = function(object)
             {
              if(is.null(object@source) || is.null(object@corpus))
                 {
                     return("Values required for these parameters")
              }
                 
             }
         )