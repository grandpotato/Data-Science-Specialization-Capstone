
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
# This function has been sourced from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/ 15/03/17

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL, title = NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
      
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        
        pushViewport(viewport(layout = grid.layout(nrow(layout) + 1, ncol(layout), heights = c(0.5, rep(5,nrow(layout))))))
        
        grid.text(title,vp = viewport(layout.pos.row = 1, layout.pos.col = seq(1:ncol(layout))),gp=gpar(fontsize=20))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row + 1,
                                            layout.pos.col = matchidx$col))
        }
    }
}

MakePredictMatrix <- function(docs.object,tokenizer.function) {
    dtm.ngram <- DocumentTermMatrix(docs.object, control = list(tokenize = tokenizer.function))
    
    dtm.freq <- tbl_df(data.frame(Words = dtm.ngram$dimnames$Terms, Frequency = colSums(as.matrix(dtm.ngram)))) %>% 
        arrange(Words, desc(Frequency)) %>% 
        mutate(cumsum = cumsum(Frequency)) %>%
        mutate(coverage = cumsum(Frequency)/sum(Frequency)) %>%
        extract(Words,into = c("Input", "Predict"),'(.*)\\s+([^ ]+)$')
#        group_by(Input, Predict) %>% 
#        summarise(max(coverage))
    return(dtm.freq)
}


CountWords <- function(phrase) {
    phrase <- str_trim(phrase)
    stri_count(phrase,regex = "\\W+") + 1
}

GetLastNWords <- function(phrase, numWords) {
    stringr::word(phrase, -1 * numWords, -1)
}
