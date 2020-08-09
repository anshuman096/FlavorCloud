suppressPackageStartupMessages(library(tm))
suppressPackageStartupMessages(library(qdap))
suppressPackageStartupMessages(library(stopwords))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(tidytext))
suppressPackageStartupMessages(library(wordcloud2))


clean_Corpus <- function(corpus) {
    wine_stopwords = c(data_stopwords_smart$en, 'wine', 'palate', 'finish', 'drink')
    corpus = tm_map(corpus, content_transformer(replace_contraction))
    print('replaced contractions')
    corpus = tm_map(corpus, removePunctuation)
    print('removed punctuation')
    corpus = tm_map(corpus, content_transformer(tolower))
    print('changed to lower case')
    corpus = tm_map(corpus, removeWords, wine_stopwords)
    print('removed stop words')
    print('cleaning complete')
    return(corpus)
}


wines = read.csv('./winemag-data-130k-v2.csv')

affordable_wines = wines[wines$price <= 25, ]
top_affordable = subset(affordable_wines, points >= quantile(affordable_wines$points, 0.75, na.rm = TRUE))
aff_reviews = top_affordable[sample(nrow(affordable_wines), 1000), ]$description

expensive_wines = wines[wines$price >= 100, ]
top_expensive = subset(expensive_wines, points >= quantile(expensive_wines$points, 0.75, na.rm = TRUE))
exp_reviews = top_expensive[sample(nrow(top_expensive), 1000), ]$description



# Affordable Wines NLP

aff_corpus = VCorpus(VectorSource(aff_reviews))
aff_corpus = clean_Corpus(aff_corpus)

affTDM = TermDocumentMatrix(aff_corpus)
affTDMm = as.matrix(affTDM)
affTDMv = sort(rowSums(affTDMm), decreasing = TRUE)
affDF = data.frame(word = names(affTDMv), freq = affTDMv)
rownames(affDF) = NULL

colors = c('#F4D166', '#F7BF5A', '#F8AD4E', '#F69C3F', '#F38C30', 
           '#F17921', '#E8691D', '#DD5C1F', '#D05022', '#C14823', '#AF4123', '#9E3A26')

aff_plot = wordcloud2(affDF, fontFamily = 'Avenir', 
                      color = rep_len(colors, length.out = nrow(affDF)),
                      shape = 'rectangle')




# Expensive Wines NLP

exp_corpus = VCorpus(VectorSource(exp_reviews))
exp_corpus = clean_Corpus(exp_corpus)

expTDM = TermDocumentMatrix(exp_corpus)
expTDMm = as.matrix(expTDM)
expTDMv = sort(rowSums(expTDMm), decreasing = TRUE)
expDF = data.frame(word = names(expTDMv), freq = expTDMv)
rownames(expDF) = NULL

colors = c('#F4D166', '#F7BF5A', '#F8AD4E', '#F69C3F', '#F38C30', 
           '#F17921', '#E8691D', '#DD5C1F', '#D05022', '#C14823', '#AF4123', '#9E3A26')

exp_plot = wordcloud2(expDF, fontFamily = 'Avenir', 
                      color = rep_len(colors, length.out = nrow(expDF)), 
                      shape = 'Rectangle')


par(mfrow= c(1, 2))
aff_plot
exp_plot