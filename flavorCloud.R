suppressPackageStartupMessages(library(tm))
suppressPackageStartupMessages(library(qdap))
suppressPackageStartupMessages(library(stopwords))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(tidytext))
suppressPackageStartupMessages(library(wordcloud2))


clean_Corpus <- function(corpus) {
    wine_stopwords = c(data_stopwords_smart$en, 'wine', 
                       'palate', 'finish', 'drink', 'notes', 
                       'nose', 'flavors', 'tannins', 'fruit', 'aromas')
    corpus = tm_map(corpus, content_transformer(replace_contraction))
    print('replaced contractions')
    corpus = tm_map(corpus, removePunctuation)
    print('removed punctuation')
    corpus = tm_map(corpus, removeNumbers)
    print('removed numbers')
    corpus = tm_map(corpus, content_transformer(tolower))
    print('changed to lower case')
    corpus = tm_map(corpus, removeWords, wine_stopwords)
    print('removed stop words')
    print('cleaning complete')
    return(corpus)
}

analyze_text <- function(reviews) {
    corpora = VCorpus(VectorSource(reviews))
    corpora = clean_Corpus(corpora)
    
    textDF = sort(rowSums(as.matrix(TermDocumentMatrix(corpora))), decreasing = TRUE)
    textDF = data.frame(word = names(textDF), freq = textDF)
    rownames(textDF) = NULL
    return(textDF)
}


wines = read.csv('./winemag-data-130k-v2.csv')

affordable_wines = wines[wines$price <= 25, ]
top_affordable = subset(affordable_wines, points >= quantile(affordable_wines$points, 0.75, na.rm = TRUE))
aff_reviews = top_affordable[sample(nrow(affordable_wines), 1000), ]$description

expensive_wines = wines[wines$price >= 100, ]
top_expensive = subset(expensive_wines, points >= quantile(expensive_wines$points, 0.75, na.rm = TRUE))
exp_reviews = top_expensive[sample(nrow(top_expensive), 1000), ]$description



# Affordable Wines NLP
affDF = analyze_text(aff_reviews)

# Affordable Wines Word Cloud
aff_colors = c('#E6E6FA', '#D8BFD8', '#DDA0DD', '#EE82EE', '#DA70D6', 
           '#BA55D3', '#9370DB', '#8A2BE2', '#9400D3', '#9932CC', '#800080', '#4B0082')

aff_plot = wordcloud2(affDF, fontFamily = 'Avenir', 
                      color = rep_len(aff_colors, length.out = nrow(affDF)),
                      shape = 'rectangle')




# Expensive Wines NLP
expDF = analyze_text(exp_reviews)

# Expensive Wines Word Cloud
exp_colors = rev(c('#F4D166', '#F7BF5A', '#F8AD4E', '#F69C3F', '#F38C30', 
           '#F17921', '#E8691D', '#DD5C1F', '#D05022', '#C14823', '#AF4123', '#9E3A26'))

exp_plot = wordcloud2(expDF, fontFamily = 'Avenir', 
                      color = rep_len(exp_colors, length.out = nrow(expDF)), 
                      shape = 'Rectangle')


par(mfrow= c(1, 2))
aff_plot
exp_plot