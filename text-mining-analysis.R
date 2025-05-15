
### Włączyć poniższe przed odpaleniem kodu
library(pdftools)
library(tidyverse)
library(tm)
library(tidytext)
library(stopwords)
library(textstem)
library(hunspell)
library(wordcloud)
library(wordcloud2)
library(plotrix)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(scales)
library(qdap)
library(topicmodels)
library(dendextend)
library(dbscan)
library(clValid)
library(lsa)
library(factoextra)
library(skmeans)
library(fpc)
library(clue)
library(lemon)
library(ggdendro)

### Zamiana pdfów na pliki .txt ###

docs_list <- dir('pdfs/')

docs <- list()

for(i in docs_list){
  docs[[i]] <- pdf_text(sprintf('pdfs/%s', i))
  print(str_interp("Plik ${i} wczytany"))
}

docs <- lapply(docs, function(x) paste(x, collapse = ' '))
docs <- lapply(docs, strsplit, split = '\n', fixed = T)

docs2 <- lapply(docs, function(x) x[[1]])

docs <- lapply(docs2, str_trim)
docs[[2]]

for(i in seq_along(docs)) {
  file_name <- paste0("txts/", names(docs)[i], ".txt")
  writeLines(docs[[i]], file_name)
}


### Czyszczenie danych tekstowych ###

df_corpus <- VCorpus(DirSource("txts/", pattern = ".txt", encoding = "UTF-8"))
df_corpus
inspect(df_corpus[1])

getTransformations()

names(df_corpus) <- c("Dexter-1x01", "Dexter-1x02", "Dexter-1x03", "Dexter-1x04", "Dexter-1x06", "Dexter-1x08", "Dexter-1x10", "Dexter-1x11", "Dexter-1x12", "Dexter-2x02")

df_corpus <- tm_map(df_corpus, content_transformer(tolower))

df_corpus <- tm_map(df_corpus, content_transformer(function(x) {
  x <- gsub("’", "'", x)  # zamiana smart quote na normalny apostrof
  x <- gsub("\\s*'\\s*", "'", x)  # usuwa spacje obok apostrofów
  return(x)
}))
df_corpus <- tm_map(df_corpus, content_transformer(function(x) {
  x <- gsub("\\bsgt\\b", "sergeant", x)
  return(x)
}))
# poniżej zamiana skróconych form czasowników z apostrofem na pełne słowa
df_corpus <- tm_map(df_corpus, content_transformer(function(x) {
  x <- gsub("\\b(can)'t\\b", "cannot", x)
  x <- gsub("n't\\b", " not", x)
  x <- gsub("'re\\b", " are", x)
  x <- gsub("'ll\\b", " will", x)
  x <- gsub("'ve\\b", " have", x)
  x <- gsub("'m\\b", " am", x)
  x <- gsub("'d\\b", " would", x)
  return(x)
}))

df_corpus <- tm_map(df_corpus, removeNumbers)

stopwords_mod <- setdiff(tm::stopwords("en"), c("not")) # do bigramów
df_corpus <- tm_map(df_corpus, removeWords, stopwords_mod)
#df_corpus <- tm_map(df_corpus, removeWords, tm::stopwords('en'))

df_corpus <- tm_map(df_corpus, stripWhitespace)
df_corpus <- tm_map(df_corpus, removePunctuation)
df_corpus <- tm_map(df_corpus, content_transformer(function(x) {
  gsub("\\b\\w*[ãõñóÓÑÕÃẼŨ]+\\w*\\b", "", x)
}))
df_corpus_lem <- tm_map(df_corpus, content_transformer(lemmatize_strings))
inspect(df_corpus[1])

df <- data.frame(id = names(df_corpus_lem),
                 text = sapply(df_corpus_lem, function(x) paste(x$content, collapse = " ")),
                 stringsAsFactors = F)

tokens <- df %>%
  unnest_tokens(word, text, drop = FALSE)

# sprawdzenie, jakie są słowa 1- lub 2-literowe
tokens %>% 
  filter(nchar(word) <= 2) %>% 
  distinct(word) %>%
  arrange(word)

# tokenizacja razem z usunięciem cyfr, słów krótszych niż 3 litery i kilku technicznych słów takich różne wariancje "continued" lub "ext" od
# "external" (zewnętrza, na których były kręcone sceny), czy nawet samo "continue", które nie było w ogóle wypowiadane przez aktorów

technical_terms <- c("contd", "cont", "contõd", "continue", "ext")

word_tokens <- df %>% 
  unnest_tokens(word, text, drop = FALSE) %>% 
  select(-text) %>% 
  filter(!str_detect(word, '\\d')) %>% 
  filter(nchar(word) > 2) %>% 
  filter(!(word %in% technical_terms))


### Macierz DTM (Document Term Matrix)

dtm <- df %>%
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  count(id, word) %>% 
  spread(word, n, fill = 0)
View(dtm)

# policzenie wag tf-idf
dtm2 <- df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  count(id, word, sort = TRUE) %>% 
  bind_tf_idf(word, id, n) %>%
  select(id, word, tf_idf) %>% 
  spread(word, tf_idf, fill = 0)
View(dtm2)


### Chmury słów ###

word_count <- word_tokens %>%
  count(word)

par(mar = c(0, 0, 0, 0))

wordcloud_1 <- wordcloud(word_count$word,
              word_count$n, 
              max.words = 200,
              colors = brewer.pal(9, 'Reds'),
              random.color=TRUE,
              family = 'mono',
              font = 1,
              size = 3
              )
wordcloud_1


#word_count$orig_freq = word_count$freq
#word_count$freq = log(word_count$orig_freq)

# tu średnio
wordcloud2_1_data <- word_count %>% 
  arrange(desc(n)) %>% 
  slice_head(n = 100) %>% 
  mutate(orig_freq = n,
         freq = log(orig_freq + 1),
         freq = freq / max(freq))

# to nawet niezłe
wordcloud2_1_data <- word_count %>% 
  arrange(desc(n)) %>% 
  slice_head(n = 100) %>% 
  mutate(orig_freq = n,
         freq = (orig_freq)^0.3)

wordcloud2_1_palette = c(
               "#b56576",
               "#e56b6f",
               "#eaac8b",
               "#355070")


wordcloud2_1 <- wordcloud2(
  wordcloud2_1_data, 
  fontFamily = 'serif',
  size = 1.5, # lub size = 1 jak się nie zmieści na stronie
  color = rep_len(wordcloud2_1_palette, nrow(word_count)),
  shape = 'triangle',
  )
wordcloud2_1


### Związki słów ###

dtm_assoc <- word_tokens %>%
  count(id, word, sort = T) %>% 
  spread(word, n, fill = 0) %>% 
  tibble::column_to_rownames('id') %>% 
  tm::as.DocumentTermMatrix(weighting = weightTfIdf)
View(dtm_assoc)

assoc <- findAssocs(dtm_assoc, terms = 'biney', corlimit = .3)
assoc

assoc_df <- as.data.frame(assoc)

assoc_df$terms <- row.names(assoc_df)
assoc_df$terms <- factor(assoc_df$terms, levels = assoc_df$terms)
assoc_df$terms

# ten wykres to test bez podziału scenariuszy na fragmenty nie ma sensu
ggplot(data = assoc_df, aes(y = terms, x = biney)) +
  geom_point(size = 2) +
  theme_minimal() +
  geom_text(aes(label = biney),
            color = 'darkred', vjust = -.5,
            size = 3) +
  theme(text = element_text(size = 10),
        axis.title.y = element_blank()) +
  scale_x_reverse()

# podział scenariuszy na fragmenty po n (100) tokenów
n <- 100

tokenized_df_100 <- df %>% 
  unnest_tokens(word, text, drop = FALSE) %>% 
  filter(!str_detect(word, '\\d')) %>% 
  filter(nchar(word) > 2) %>% 
  group_by(id) %>% 
  mutate(token_id = row_number(),
         fragment = (token_id - 1) %/% n + 1,
         frag_id = paste0(id, "_", fragment)) %>% 
  ungroup()
View(tokenized_df_100)

dtm_assoc_split <- tokenized_df_100 %>% 
  count(frag_id, word) %>% 
  pivot_wider(names_from = word, values_from = n, values_fill = 0) %>% 
  column_to_rownames('frag_id') %>% 
  as.matrix() %>% 
  as.DocumentTermMatrix(weighting = weightTfIdf)

assoc <- findAssocs(dtm_assoc_split, terms = "biney", corlimit = 0.3)

assoc_df <- as.data.frame(assoc)

assoc_df$terms <- row.names(assoc_df)
assoc_df$terms <- factor(assoc_df$terms, levels = assoc_df$terms)
assoc_df$terms

ggplot(data = assoc_df, aes(y = terms, x = biney)) +
  geom_point(size = 2) +
  theme_minimal() +
  geom_text(aes(label = biney),
            color = 'darkred', vjust = -.5,
            size = 3) +
  theme(text = element_text(size = 10),
        axis.title.y = element_blank()) +
  scale_x_reverse()


### Słupkowe wykresy częstości

font_add(family = "Courier New", regular = "/fonts/cour.ttf")

word_top <- word_tokens %>% 
  count(id, word, sort = TRUE) %>% 
  group_by(id) %>% 
  top_n(9, n) %>% 
  ungroup() %>% 
  mutate(word = reorder_within(word, n, id)) %>% 
  mutate(id = str_remove(id, "\\.pdf\\.txt$"))

ggplot(word_top, aes(x = word, y = n, fill = id)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~id, scales = "free_y") +
  scale_x_reordered() +
  labs(x = "word", y = "number of occurences") +
  theme(text = element_text(size = 12, face = 'bold', family = 'serif'),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = 'gray90'),
        panel.grid.major = element_line(color = 'gray80'))


### Porównanie korpusów

tdm <- TermDocumentMatrix(df_corpus_lem)
tdm_m <- as.matrix(tdm)
View(tdm_m)
pal <- brewer.pal(8, "Greens")
pal <- pal[-(1:4)]

# Chmura wspólnych słów
par(mfrow = c(1, 2), mar = c(0, 0, 0, 0))
commonality.cloud(tdm_m,
                  max.words = 200,
                  random.order = FALSE,
                  colors = pal,
                  family = 'serif',
                  font = 2,
                  scale = c(2, 1))

# Chmura porównawcza (różnice)
comparison.cloud(tdm_m,
                 max.words = 300,
                 random.order = FALSE,
                 title.size = 1.5,
                 colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E",
                            "#E6AB02", "#A6761D", "#666666", "#A6CEE3", "#FB9A99"),
                 family = 'serif',
                 font = 2,
                 scale = c(3, 0.5))


### Wykres piramidowy

common_words <- subset(tdm_m, tdm_m[, 1] > 0 & tdm_m[, 2] > 0)
head(common_words) 
tail(common_words)

diff <- abs(common_words[, 1] - common_words[, 2])
common_words <- cbind(common_words, diff)
common_words <- common_words[order(common_words[, 3], decreasing = TRUE), ]


df_common_words <- data.frame(x = common_words[1:25, 1],
                              y = common_words[1:25, 2],
                              labels = rownames(common_words)[1:25]
                              )

colors <- colorRampPalette(brewer.pal(9, 'Reds'))(25)

par(mfrow = c(1,1), mar = c(0, 0, 0, 0))

pyramid.plot(df_common_words$x, df_common_words$y,
             labels = df_common_words$labels,
             gap = 100,
             top.labels = c("Dexter 1x01", "words", "Dexter 1x02"),
             lxcol = colors, rxcol = colors,
             laxlab = NULL, raxlab = NULL, unit = NULL)


### Analiza sentymentu

# load('sentiments_lexicons.RData')

bing %>% 
  filter(word == 'debt')

nrc %>% filter(sentiment %in% c('positive', 'negative'))
nrc %>% filter(!sentiment %in% c('positive', 'negative'))

loughran <- get_sentiments("loughran"); loughran

bing %>% 
  filter(sentiment %in% c('positive', 'negative')) %>% 
  count(sentiment)

nrc %>% 
  filter(sentiment %in% c('positive', 'negative')) %>% 
  count(sentiment)

nrc %>% 
  filter(!sentiment %in% c('positive', 'negative')) %>% 
  count(sentiment)

afinn %>% 
  mutate(sentiment = ifelse(value < 0, 'negative', 'positive')) %>% 
  count(sentiment)

loughran %>% 
  count(sentiment)

# Leksykon bing
word_sent <- word_tokens %>% 
  inner_join(bing) %>% 
  count(word, sentiment, sort = T)

## Pozytywne / negatywne

# 12 najpopularniejszych pozytywnych i negatywnych słów
word_sent_top <- word_sent %>%
  group_by(sentiment) %>% 
  top_n(12, n) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n))

ggplot(word_sent_top, aes(x = word, y = n, fill = sentiment)) +
  geom_col(show.legend = F) +
  facet_grid(~sentiment, scales = 'free_x') +
  labs(y = "number of occurences") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Leksykon nrc
nrc_polarity <- nrc %>% 
  filter(sentiment %in% c('positive', 'negative'))

word_sent2 <- word_tokens %>% 
  inner_join(nrc_polarity, relationship = "many-to-many") %>% 
  count(word, sentiment, sort = T)

# 12 najpopularniejszych pozytywnych i negatywnych słów
word_sent_top2 <- word_sent2 %>% 
  group_by(sentiment) %>% 
  top_n(12, n) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n))

ggplot(word_sent_top2, aes(x = word, y = n, fill = sentiment)) +
  geom_col(show.legend = F) +
  facet_grid(~sentiment, scales = 'free_x') +
  labs(y = "number of occurences") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
## Emocje

word_emotions <- word_tokens %>% 
  inner_join(loughran %>% filter(!sentiment %in% c('positive', 'negative'))) %>%
  count(word, sentiment)

word_emotions_top <- word_emotions %>% 
  group_by(sentiment) %>% 
  top_n(12, n) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n))
 
ggplot(word_emotions_top, aes(x = word, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +
  coord_flip() +
  theme_minimal() +
  labs(x = "word", y = "number of occurences") +
  theme(axis.text.y = element_text(size = 9, family = 'serif'))

# alternatywnie heatmapa
heatmap_data <- word_emotions %>% 
  group_by(sentiment) %>% 
  top_n(12, n) %>% 
  spread(sentiment, n, fill = 0)

heatmap_long <- heatmap_data %>% 
  gather(sentiment, count, -word)

ggplot(heatmap_long, aes(x = sentiment, y = reorder(word, count), fill = count)) +
  geom_tile(color = 'white') +
  scale_fill_gradient(low = 'white', high = 'darkred') + 
  theme_minimal() +
  labs(x = 'emotion', y = 'word') +
  theme(axis.text.y = element_text(size = 11, family = 'serif'))
  

## n-gramy w analizie sentymentu
bigrams <- df %>% 
  unnest_tokens(bigram, text, token = 'ngrams', n = 2) %>% 
  separate(bigram, c('c1', 'c2'), sep = " ")

not_words <- bigrams %>% 
  filter(c1 == 'not') %>% 
  inner_join(afinn, by = c(c2 = 'word')) %>% 
  count(c2, value, sort = TRUE) %>% 
  ungroup()

top_not_words <- not_words %>% 
  mutate(contribution = n * value) %>% # wpływ słowa = liczba wystąpień * wartość sentymentu
  arrange(desc(abs(contribution))) %>% # sortuje dane malejąco wg. bezwzględnej wartości wpływu: 
  # nieważne, czy sentyment był silnie pozytywny czy negatywny – pokazujemy po prostu te słowa, które miały największy wpływ, w którąkolwiek stronę
  head(20) %>% 
  mutate(c2 = reorder(c2, contribution))
  
ggplot(top_not_words, aes(c2, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) + 
  xlab("words preceded by \"not\"") + 
  ylab("sentiment value * number of word occurences") +
  theme(axis.text.y = element_text(size = 11, family = 'serif')) +
  coord_flip() 

# więcej słów negujących
negation_words <- c('not', 'no', 'never', 'nobody', 'without') 

negated_words <- bigrams %>% 
  filter(c1 %in% negation_words) %>% 
  inner_join(afinn, by = c('c2' = 'word')) %>% 
  count(c1, c2, value, sort = T) %>% 
  ungroup()

# słowa najbardziej przyczyniające się do zakłamania sentymentu
top_neg_words <- negated_words %>% 
  mutate(contribution = n * value) %>% # wpływ = liczba wystąpień * wartość sentymentu
  arrange(desc(abs(contribution))) %>% 
  group_by(c1) %>% 
  top_n(10, abs(contribution)) %>% 
  ungroup() %>% 
  mutate(c2 = reorder(c2, contribution))

ggplot(top_neg_words, aes(c2, n * value, fill = n * value > 0)) +
  geom_col(show.legend = F) +
  xlab("words preceded by negation") +
  ylab("sentiment value * number of word occurrences") +
  theme(axis.text.y = element_text(size = 11, family = 'serif')) +
  facet_wrap(~c1, ncol = 2, scales = 'free') + # podział wg. zmiennej c1
  coord_flip()


## qdap

# import pliku do korpusu jeszcze raz, ponieważ należy podzielić go na zdania, zamiast zwykłej tokenizacji na słowa
setwd("C:/Users/borys/SynologyDrive/Zdalne cdv 2024&2025/4 semestr/modul text mining/projekt")

df_corpus2 <- VCorpus(DirSource("txts/", pattern = ".txt", encoding = "UTF-8"))
df_corpus2
inspect(df_corpus2[1])

getTransformations()

names(df_corpus2) <- c("Dexter-1x01", "Dexter-1x02", "Dexter-1x03", "Dexter-1x04", "Dexter-1x06", "Dexter-1x08", "Dexter-1x10", "Dexter-1x11", "Dexter-1x12", "Dexter-2x02")

df_corpus2 <- tm_map(df_corpus, content_transformer(tolower))
df_corpus2 <- tm_map(df_corpus2, content_transformer(function(x) {
  x <- gsub("’", "'", x)
  x <- gsub("\\s*'\\s*", "'", x)
  return(x)
}))
df_corpus2 <- tm_map(df_corpus2, content_transformer(function(x) {
  x <- gsub("\\b(can)'t\\b", "cannot", x)
  x <- gsub("n't\\b", " not", x)
  x <- gsub("'re\\b", " are", x)
  x <- gsub("'ll\\b", " will", x)
  x <- gsub("'ve\\b", " have", x)
  x <- gsub("'m\\b", " am", x)
  x <- gsub("'d\\b", " would", x)
  return(x)
}))
df_corpus2 <- tm_map(df_corpus, removeNumbers)

stopwords_mod <- setdiff(tm::stopwords("en"), c("not"))
df_corpus2 <- tm_map(df_corpus2, removeWords, stopwords_mod)

df_corpus2 <- tm_map(df_corpus2, stripWhitespace)
df_corpus2 <- tm_map(df_corpus2, removePunctuation)
df_corpus2 <- tm_map(df_corpus2, content_transformer(function(x) {
  gsub("\\b\\w*[ãõñóÓÑÕÃẼŨ]+\\w*\\b", "", x)
}))
df_corpus_lem2 <- tm_map(df_corpus2, content_transformer(lemmatize_strings))
inspect(df_corpus2[1])

df2 <- data.frame(id = names(df_corpus_lem2),
                 text = sapply(df_corpus_lem2, function(x) paste(x$content, collapse = " ")),
                 stringsAsFactors = F)

# podział korpusu na zdania
sentence_token <- df2 %>% 
  unnest_tokens(sentence, text, token = 'regex', pattern = '\\.') %>%
  slice_head(n = 500)

wyniki <- polarity(sentence_token$sentence, sentence_token$id)
wyniki

ggplot(wyniki$all, aes(x = polarity)) +
  geom_histogram(bins = 20, fill = 'cyan4', color = 'gray') +
  theme_minimal()

# polarity pierwszych 500 zdań scenariuszy
ggplot(wyniki$all, aes(x = polarity)) +
  theme_minimal() +
  geom_histogram(fill = 'cyan4',
                 color = 'gray') +
  theme(strip.text = element_text(size = 11, family = 'serif')) +
  facet_rep_grid(vars(id), scales = "free"
                 , switch = "both", space = "free",
                 repeat.tick.labels = T)

ggplot(wyniki$all, aes(x = polarity)) +
  geom_histogram(bins = 20, fill = 'cyan4', color = 'gray') +
  facet_wrap(~id) +
  theme_minimal()

# wordcloud sentymentu
sentence_token$polarity <- scale(wyniki$all$polarity)

pos.sentences <- subset(sentence_token$sentence, sentence_token$polarity>0)
neg.sentences <- subset(sentence_token$sentence, sentence_token$polarity<0)

pos <- paste(pos.sentences, collapse = " ")
neg <- paste(neg.sentences, collapse = " ")
all <- c(pos, neg)
all_corpus <- VCorpus(VectorSource(all))

all_tdm <- TermDocumentMatrix(all_corpus,
                              control = list(
                                weighting = weightTfIdf,
                                removePunctuation = T,
                                stopwords = stopwords()
                              ))

all_tdm_m <- as.matrix(all_tdm)
colnames(all_tdm_m) <- c('positive', 'negative')

comparison.cloud(all_tdm_m, 
                 max.words = 120,
                 colors = c('cyan4', 'coral3'))


### Analiza tematu

# załadowanie korpusu jeszcze raz, ponieważ oryginalny jest w tym samym pliku, a jego zmiany doprowadziłyby do zmian w innych częściach analizy

df_corpus3 <- VCorpus(DirSource("txts/", pattern = ".txt", encoding = "UTF-8"))
df_corpus3
inspect(df_corpus3[1])

getTransformations()

names(df_corpus3) <- c("Dexter-1x01", "Dexter-1x02", "Dexter-1x03", "Dexter-1x04", "Dexter-1x06", "Dexter-1x08", "Dexter-1x010", "Dexter-1x011", "Dexter-1x12", "Dexter-2x02")

df_corpus3 <- tm_map(df_corpus3, content_transformer(tolower))
df_corpus3 <- tm_map(df_corpus3, content_transformer(function(x) {
  x <- gsub("’", "'", x)
  x <- gsub("\\s*'\\s*", "'", x)
  return(x)
}))
df_corpus3 <- tm_map(df_corpus3, content_transformer(function(x) {
  x <- gsub("\\bsgt\\b", "sergeant", x)
  return(x)
}))
df_corpus3 <- tm_map(df_corpus3, content_transformer(function(x) {
  x <- gsub("\\b(can)'t\\b", "cannot", x)
  x <- gsub("n't\\b", " not", x)
  x <- gsub("'re\\b", " are", x)
  x <- gsub("'ll\\b", " will", x)
  x <- gsub("'ve\\b", " have", x)
  x <- gsub("'m\\b", " am", x)
  x <- gsub("'d\\b", " would", x)
  return(x)
}))
df_corpus3 <- tm_map(df_corpus3, removeNumbers)
stopwords_mod <- setdiff(tm::stopwords("en"), c("not"))
df_corpus3 <- tm_map(df_corpus3, removeWords, stopwords_mod)
df_corpus3 <- tm_map(df_corpus3, stripWhitespace)
df_corpus3 <- tm_map(df_corpus3, removePunctuation)
df_corpus3 <- tm_map(df_corpus3, content_transformer(function(x) {
  gsub("\\b\\w*[ãõñóÓÑÕÃẼŨ]+\\w*\\b", "", x)
}))
df_corpus_lem3 <- tm_map(df_corpus3, content_transformer(lemmatize_strings))
inspect(df_corpus3[1])

df3 <- data.frame(id = names(df_corpus_lem3),
                 text = sapply(df_corpus_lem3, function(x) paste(x$content, collapse = " ")),
                 stringsAsFactors = F)

stopwords3 <- df3 %>%
  unnest_tokens(word, text) %>% 
  distinct(id, word) %>% 
  count(word, sort = T) %>% 
  filter(n > .95*nrow(df3))
  
df_corpus_lem3 <- tm_map(df_corpus_lem3, removeWords, stopwords3$word)

dtm3 <- DocumentTermMatrix(df_corpus_lem3,
                          control = list(
                            weighting = weightTf
                          ))
View(as.matrix(dtm3))
## Model LDA (Latent Dirichlet Allocation)

# policzenie modelu
topic_minutes <- LDA(dtm3,
                     k = 3,
                     control = list(seed = 1234))
topic_minutes

# prawdopodobieństwa słowo-temat (prawdopodobieństwo "beta", że dane słowo jest powiązane z danym tematem)
mi_topics <- tidy(topic_minutes, matrix = 'beta')
mi_topics

# wizualizacja 10-sięciu najbardziej związanych z każdym tematem słów  
top_terms <- mi_topics %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta) # malejąco
top_terms  
  
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = F) +
  theme(axis.text.y = element_text(size = 11, family = 'serif')) +
  facet_wrap(~ topic, scales = 'free') +
  coord_flip() +
  scale_x_reordered()

## największa różnica w betach pomiędzy tematami. Dzięki temu różnica między tematami jest symetryczna  
beta_spread <- mi_topics %>% 
  mutate(topic = paste0('topic', topic)) %>% 
  spread(topic, beta) %>% 
  filter(topic1 > .001 | topic2 >  .001) %>% 
  mutate(log_ratio = log2(topic2 / topic1))
beta_spread  

top_beta_spread <- beta_spread %>% 
  arrange(desc(abs(log_ratio))) %>% 
  group_by(log_ratio) %>% 
  head(20) %>% 
  ungroup() %>% 
  mutate(term = reorder(term, log_ratio))
  
ggplot(data = top_beta_spread, aes(x = term, y = log_ratio, fill = log_ratio > 0)) +
  geom_col(show.legend = F) +
  xlab("word") +
  ylab("logarithm beta ratio") +
  theme(axis.text.y = element_text(size = 11, family = 'serif')) +
  coord_flip()

## prawdopodobieństwa dotyczące dokumentu i klasyfikacja scenariuszy
mi_class <- tidy(topic_minutes, matrix = 'gamma')
mi_class

# sortowanie po scenariuszach
mi_class %>% 
  arrange(document, topic)

# sortowanie po gamma
mi_class %>% 
  arrange(topic, desc(gamma))


### Segmentacja

dtm3_m <- as.matrix(dtm3)
# View(dtm3_m)

for(i in 1:dim(dtm3_m)[1]){
  dtm3_m[i, ] = as.matrix(dtm3_m[i, ])/norm(as.matrix(dtm3_m[i,]), type = 'F')
}

## klastrowanie hierarchiczne odległością euklidesową (czy normą Forbeniusa)

# macierz odległości
dist_uni = dist(dtm3_m, method = 'euclidean')

# klastrowanie metodą 'ward.D'
hc_uni = hclust(dist_uni, method = 'ward.D')
hc_uni_d = as.dendrogram(hc_uni)

# przykładowy dendrogram
plot(hc_uni_d, main = 'Method Ward', col.main = 'dodgerblue',
     cex = 1, leaflab = 'perpendicular')
  
k = 10
wyniki_k <- numeric(k-1)  
for(i in 2:k){
  members = cutree(hc_uni, i)
  dunn_index = dunn(clusters = members, Data = dist_uni)
  wyniki_k[i-1] <- dunn_index
}

ggplot(data = NULL, aes(x = 2:10, y = wyniki_k)) +
  geom_line() +
  scale_x_continuous(breaks = 2:10) +
  geom_vline(xintercept = which.min(wyniki_k) + 1, linewidth = 1, linetype = 'dashed', color = 'red') +
  ylab("dunn index value") +
  xlab("clusters count (k)")
# wychodzą m.in. dwie klastry, max 10 (which.max(wyniki_k) + 1)

# plot hcd
plot(hc_uni_d, main = 'Ward Method', col.main = 'dodgerblue')
rect.dendrogram(hc_uni_d, k = 2, border = 'blue', xpd = F, lower_rect = 0) # k = 4 testowo (mamy w sumie 4 klastry, które są podzielone pod względem
# podobieństw między scenariuszami, gdzie tylko jeden scenariusz nie jest bardziej podobny do innego niż reszta, ale to też może wynikać z tego, że
# jest po prostu liczba dokumentów niepodzielna przez 4: 10:3=3 1 reszty)

# plot hcd v2
hc_data <- as.dendrogram(hc_uni)
dendro_data <- dendro_data(hc_data)

ggplot() +
  geom_segment(data = dendro_data$segments,
               aes(x = x, y = y, xend = xend, yend = yend),
               linewidth = 0.8, color = "steelblue") +
  theme_minimal() +
  labs(title = "", x = hc_uni_d, y = "distance") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

dend <- as.dendrogram(hc_uni)

dend <- color_branches(dend, k = 2)

plot(dend, main = "")
rect.dendrogram(dend, k = 2, border = 4:7, lty = 2, lwd = 2)

wyniki_klastry <- as.data.frame(cutree(hc_uni_d, 2))
names(wyniki_klastry) <- 'clusters'

wyniki_klastry <- wyniki_klastry %>% 
  rownames_to_column('id')

ggplot(data = wyniki_klastry, aes(x = factor(clusters), fill = factor(clusters))) +
  geom_bar(show.legend = F) +
  theme(strip.text = element_text(size = 11, family = 'serif')) +
  facet_wrap(~id)

## Algorytm kmeans

dist_matrix = proxy::dist(dtm3_m, method = 'euclidean')
class(dist_matrix)

k2 = 9

# dunn index
wyniki_klastry2 <- numeric(k2-1)

for(i in 2:k2){
  members = kmeans(dtm3_m, i, nstart = 25)$cluster
  dunn_index = dunn(clusters = members, Data = dist_matrix)
  wyniki_klastry2[i-1] <- dunn_index
}

ggplot(data = NULL, aes(x = 2:k2, y = wyniki_klastry2)) +
  geom_line() +
  scale_x_continuous(breaks = 2:k2) +
  geom_vline(xintercept = which.max(wyniki_klastry2) + 1, linewidth = 1, linetype = 'dashed', color = 'red') +
  xlab("number of clusters") +
  ylab("dunn index for k-means")

# silhouette
wyniki_sil <- numeric(k2-1)

for(i in 2:k2){
  mod <- kmeans(dtm3_m, i, nstart = 25)
  sil <- silhouette(mod$cluster, dist_matrix)
  wyniki_sil[i-1] <- mean(sil[, 3])
}

ggplot(data = NULL, aes(x = 2:k2, y = wyniki_sil)) +
  geom_line() +
  scale_x_continuous(breaks = 2:k2) +
  geom_vline(xintercept = which.max(wyniki_sil) + 1, linewidth = 1, linetype = 'dashed', color = 'red') +
  xlab("number of clusters") +
  ylab("dunn index for k-means")

# wersja 2D
mod_km <- kmeans(dtm3_m, 2)
points <- cmdscale(dist_matrix, k = 2) 
members = as.factor(mod_km$cluster)

ggplot(data = NULL, aes(x = points[,1], y = points[,2])) +
  geom_point(aes(color = members)) +
  xlab('Dim 1 (MDS)') +
  ylab('Dim 2 (MDS)')

# PCA
fviz_cluster(mod_km, data = dtm3_m,
             labelsize = 0,
             ggtheme = theme_minimal(),
             main = ""
)

dtm4 <- removeSparseTerms(dtm3, 0.7)
dtm4_m <- as.matrix(dtm4)

pca_res <- prcomp(dtm4_m, scale. = FALSE)  # już znormalizowane, więc bez scale.
fviz_pca_ind(pca_res, geom.ind = "point", col.ind = as.factor(mod_km2$cluster),
             palette = "jco", title = '')

for(i in 1:dim(dtm4_m)[1]){
  dtm4_m[i,] = as.matrix(dtm4_m[i,])/norm(as.matrix(dtm4_m[i,]), type = 'F')
}

mod_km2 <- kmeans(dtm4_m, 2)

fviz_cluster(mod_km2, data = dtm4_m,
             labelsize = 0,
             ggtheme = theme_minimal(),
             main = '')

## Klastrowanie sferyczne (spherical clustering)
# to niżej na potrzeby wykresu silhouette
dtm3_m_names <- c("1x01", "1x02", "1x03", "1x04", "1x06", "1x08", "1x10", "1x11", "1x12", "2x02")
rownames(dtm3_m) <- dtm3_m_names

sk_part <- skmeans(dtm3_m, 2,
                   m = 1.2,
                   control = list(nruns = 5, verbose = T))

barplot(table(sk_part$cluster), main = "", col = c('cadetblue3', 'darkorange'))

plotcluster(cmdscale(dist(dtm3_m)), 
            sk_part$cluster,
            col = c('cadetblue3', 'darkorange')[sk_part$cluster])

plot(silhouette(sk_part), main = "")

s_cludtm3_ms_clues_proto <- t(cl_prototypes(sk_part))

comparison.cloud(s_cludtm3_ms_clues_proto, max.words = 350,
                 scale = c(2, .5),
                 family = 'serif',
                 colors = c('cadetblue3', 'darkorange'))

# 5 najbardziej reprezentatywnych słów dla klastrów 1 i 2
sort(s_cludtm3_ms_clues_proto[, 1], decreasing = T)[1:5]
sort(s_cludtm3_ms_clues_proto[, 2], decreasing = T)[1:5]

## Klastrowanie k-medoidalne (k-medoid clustering)

# kryterium avg silhouette
pam_clus <- pamk(dtm3_m, krange = 2:9, critout = T)
dissimilarity_m <- dist(dtm3_m)
plot(silhouette(pam_clus$pamobject$clustering, dissimilarity_m), main = "")

# avg silhouette calinski-harabasz
pam_clus <- pamk(dtm3_m, krange=2:9, critout = T, criterion = 'ch')
dissimilarity_m <- dist(dtm3_m)
plot(silhouette(pam_clus$pamobject$clustering, dissimilarity_m), main = "")

# wszystkie kryteria
results <- cluster.stats(dist(dtm3_m), pam_clus$pamobject$clustering)
results



















