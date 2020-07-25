library(tidyverse)
library(ggplot2)
library(tidytext)
library(wordcloud2)




wines = read.csv('./winemag-data-130k-v2.csv')

affordable_wines = wines[wines$price <= 25, ]
top_affordable = subset(affordable_wines, points >= quantile(affordable_wines$points, 0.75, na.rm = TRUE))
aff_reviews = top_affordable[sample(nrow(affordable_wines), 1000), ]$description

expensive_wines = wines[wines$price >= 100, ]
top_expensive = subset(expensive_wines, points >= quantile(expensive_wines$points, 0.75, na.rm = TRUE))
exp_reviews = top_expensive[sample(nrow(top_expensive), 1000), ]$description


phrases = c()

'%!in%' = function(x,y)!('%in%'(x,y))
for (review in aff_reviews) {
    review_df = tibble(line = 1:1, text = review)
    review_draft = review_df %>% unnest_tokens(word, text) %>% left_join(parts_of_speech)
    review_draft$is_included = review_draft$pos %in% c('Adjective','Adverb', 'Noun') & review_draft$word %!in% c('The','the', 'Is', 'is','And','and','Or','or','Isn’t','isn’t', 'A','a', 'This','this','That','that','While','while', 'Still','still','are','Are', 'Out','out', 'Be', 'be', 'It’s', 'it’s', 'It', 'it', 'Already', 'already', 'Will', 'will', 'certainly', 'At', 'at', 'in', 'In', 'Of', 'of', 'Off', 'off', 'to', 'To', 'By', 'by', 'On', 'on', 'For', 'for', 'As', 'as', 'If', 'if', 'an', 'An', 'Through', 'through', 'but', 'But', 'more', 'More', 'Just', 'just', 'also')
    word_var = ''
    phrase = ''
    for (i in 1:nrow(review_draft)) {
        if ((review_draft$is_included[i] == TRUE) & ((review_draft$word[i] != word_var) | (review_draft$word[i] == 'bodied'))) {
            phrase = paste(phrase, review_draft$word[i], sep = ' ')
            word_var = review_draft$word[i]
            if (i == nrow(review_draft)) {
                phrase = substring(phrase, 2)
                append(phrases, phrase)
                phrase = ''
            }
        } else if(phrase != '') {
            phrase = substring(phrase, 2)
            phrases = append(phrases, phrase)
            phrase = ''
        }
    }
}

phrases_df = tibble(line = 1:length(phrases), text = phrases)
aff_freq = data.frame(table(unlist(strsplit(tolower(phrases_df$text), " "))))
colnames(aff_freq) = c("word", "freq")
write.csv(aff_freq, 'aff_reviews_wordcount.csv')



aff_freq = aff_freq[ aff_freq$freq >= 25 &
                         aff_freq$word != 'wine' & 
                         aff_freq$word != 'it\'s' &
                         aff_freq$word != 'not' & 
                         aff_freq$word !=  'drink' & 
                         aff_freq$word != 'bodied' & 
                         aff_freq$word != 'there\'s' &
                         aff_freq$word != 'there' &
                         aff_freq$word != 'that\'s' & 
                         aff_freq$word != 'yet' & 
                         aff_freq$word != 'palate' &
                         aff_freq$word != 'acidity' &
                         aff_freq$word != 'so' & 
                         aff_freq$word != 'you' & 
                         aff_freq$word != 'very' & 
                         aff_freq$word != 'make' & 
                         aff_freq$word != 'almost' & 
                         aff_freq$word != 'all' &
                         aff_freq$word != 'finish' & 
                         aff_freq$word != 'give' & 
                         aff_freq$word != 'black', ]

colors = c('#F4D166', '#F7BF5A', '#F8AD4E', '#F69C3F', '#F38C30', 
           '#F17921', '#E8691D', '#DD5C1F', '#D05022', '#C14823', '#AF4123', '#9E3A26')

aff_plot = wordcloud2(aff_freq, fontFamily = 'Avenir', 
                      color = rep_len(colors, length.out = nrow(aff_freq)), 
                      figPath = 'wineBottle.jpg')



phrases = c()

for (review in exp_reviews) {
    review_df = tibble(line = 1:1, text = review)
    review_draft = review_df %>% unnest_tokens(word, text) %>% left_join(parts_of_speech)
    review_draft$is_included = review_draft$pos %in% c('Adjective','Adverb', 'Noun') & review_draft$word %!in% c('The','the', 'Is', 'is','And','and','Or','or','Isn’t','isn’t', 'A','a', 'This','this','That','that','While','while', 'Still','still','are','Are', 'Out','out', 'Be', 'be', 'It’s', 'it’s', 'It', 'it', 'Already', 'already', 'Will', 'will', 'certainly', 'At', 'at', 'in', 'In', 'Of', 'of', 'Off', 'off', 'to', 'To', 'By', 'by', 'On', 'on', 'For', 'for', 'As', 'as', 'If', 'if', 'an', 'An', 'Through', 'through', 'but', 'But', 'more', 'More', 'Just', 'just', 'also')
    word_var = ''
    phrase = ''
    for (i in 1:nrow(review_draft)) {
        if ((review_draft$is_included[i] == TRUE) & ((review_draft$word[i] != word_var) | (review_draft$word[i] == 'bodied'))) {
            phrase = paste(phrase, review_draft$word[i], sep = ' ')
            word_var = review_draft$word[i]
            if (i == nrow(review_draft)) {
                phrase = substring(phrase, 2)
                append(phrases, phrase)
                phrase = ''
            }
        } else if(phrase != '') {
            phrase = substring(phrase, 2)
            phrases = append(phrases, phrase)
            phrase = ''
        }
    }
}

phrases_df = tibble(line = 1:length(phrases), text = phrases)
exp_freq = data.frame(table(unlist(strsplit(tolower(phrases_df$text), " "))))
colnames(exp_freq) = c("word", "freq")
write.csv(exp_freq, 'exp_reviews_wordcount.csv')


exp_freq = exp_freq[exp_freq$freq > 25 & 
                        exp_freq$word != 'wine' & 
                        exp_freq$word != 'it\'s' &
                        exp_freq$word != 'not' & 
                        exp_freq$word !=  'drink' & 
                        exp_freq$word != 'bodied' & 
                        exp_freq$word != 'there\'s' &
                        exp_freq$word != 'there' &
                        exp_freq$word != 'that\'s' & 
                        exp_freq$word != 'yet' & 
                        exp_freq$word != 'palate' &
                        exp_freq$word != 'acidity' &
                        exp_freq$word != 'so' & 
                        exp_freq$word != 'you' & 
                        exp_freq$word != 'very' & 
                        exp_freq$word != 'make' & 
                        exp_freq$word != 'almost' & 
                        exp_freq$word != 'all' &
                        exp_freq$word != 'finish' & 
                        exp_freq$word != 'give' & 
                        exp_freq$word != 'now' & 
                        exp_freq$word != 'de', ]

colors = c('#F4D166', '#F7BF5A', '#F8AD4E', '#F69C3F', '#F38C30', 
           '#F17921', '#E8691D', '#DD5C1F', '#D05022', '#C14823', '#AF4123', '#9E3A26')

exp_plot = wordcloud2(exp_freq, fontFamily = 'Avenir', 
                      color = rep_len(colors, length.out = nrow(exp_freq)), 
                      figPath = 'wineAndGlass.png')


par(mfrow= c(1, 2))
aff_plot
exp_plot