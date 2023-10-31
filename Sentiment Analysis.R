
#PENGERTIAN
#sentiment analysis adalah mengekstrak data untuk mendapatkan informasi sentimen yang bernilai
#                          positif atau negatif (pd umumnya dibagi menjadi 7)
#pengelompokan berdasarkan sentimen dengan skoring
#stopword tidak ada skor (ex:yang, di, dll), nilainya 0
#untuk sentimen positif nilainya +1, untuk sentimen negatif nilainya -1
#sentiment analysisi menghitung per documentnya

#contoh
df <- data.frame(ID=rep(c(1,2,3)),
                 text=rep(c("jangan lari","lari pagi", "lari menjauh")))
head(df)

###############################################################33
#Analysis Sentiment data tweetter
#data 3 periode

#Packages
library(tm)
library(stringr)
library(dplyr)
library(textdata)
library(tidytext)

#read data
library(readxl)
Tiga_Periode <- read_excel("E:/SEMESTER 6/MDTT/3 Periode.xlsx", 
                         sheet = "3 Periode -filterretweets -filt")
head(Tiga_Periode)

# Cleaning data
head(Tiga_Periode)
Tiga_Periode <- gsub("RT", replacement = "", Tiga_Periode) # Menghilangkan RT
Tiga_Periode <- gsub("@\\w+\\:", replacement="",Tiga_Periode) # Menghilangkan Username
Tiga_Periode <- gsub("@\\w+", replacement="", Tiga_Periode) # menghilangkan username mention
Tiga_Periode <- gsub("\\:", replacement="", Tiga_Periode) # menghilangkan :
Tiga_Periode <- gsub("\\,", replacement="", Tiga_Periode) # menghilangkan ,
Tiga_Periode <- gsub("\\.", replacement="", Tiga_Periode) # menghilangkan .
Tiga_Periode <- gsub("\\s\\s", replacement=" ", Tiga_Periode) # menghilangkan double space

# menghapus angka
Tiga_Periode <- gsub("[[:digit:]]", "", Tiga_Periode)


# 
Tibble_Tiga_Periode <- tibble(line = 1:100, 
                              text=Tiga_Periode)

Token_Tiga_Periode <- Tibble_Tiga_Periode %>%
  unnest_tokens(output=word, input=text) %>%
  count(line, word, sort=T)

Token_Tiga_Periode <- Token_Tiga_Periode %>%
  arrange(line)

stopword_indonesia <- read.delim('stopword_indonesia.txt')

stopword_indonesia <- read.csv("E:/SEMESTER 6/MDTT/stopword_indonesia.txt", sep="", header = F)
Token_Tiga_Periode <- Token_Tiga_Periode %>%
  anti_join(stopword_indonesia)

#set as dictionary
setwd("E:/SEMESTER 6/MDTT")
stop_words_ind <- read.delim("stopword_indonesia.txt", header=F)

#untuk menambah kata pada stopword
stop_words_ind <- rbind(stop_words_ind, "periode")
colnames(stop_words_ind) <- "word"
stop_words_ind <- tibble(word = stop_words_ind$word)

#error
for (j in 1:100){
  for (i in length(Token_Tiga_Periode[Token_Tiga_Periode$line = j,])){
    Token_Tiga_Periode[Token_Tiga_Periode = j,] 
  }
}

dtm_Tiga_Periode <- Token_Tiga_Periode %>%
  cast_dtm(line, word, n)

df_Tiga_Periode <- data.frame(as.matrix(dtm_Tiga_Periode))
View(df_Tiga_Periode)
  

positif <- read.delim("positive.txt", header = F)
negatif <- read.delim("negative.txt", header = F)
list_positif <- NULL
list_negatif <- NULL


# Membuat list positif
for (i in 1:1717){
  list_positif[i] <- token_tiga_periode[i,2] %in% positif$V1
}
list_positif
# Membuat list negatif

for (i in 1:1717){
  list_negatif[i] <- token_tiga_periode[i,2] %in% negatif$V1
}

list_negatif

score = NULL
for (i in 1:1717){
  if (list_negatif[[i]] == TRUE){
    score[i] = -1*token_tiga_periode$n[i]
  } else {
    if (list_positif[[i]] == TRUE)
    { score[i] = 1*token_tiga_periode$n[i]
    } else {
      score[i] = 0
    }
  }
}
score
token_tiga_periode$score <- score
token_tiga_periode$score

hasil_sentiment <- token_tiga_periode %>%
  group_by(line) %>%
  summarize(sum(score))

write.csv(hasil_sentiment, "hasil_sentiment.csv")

#############################3
#Pemodelan naive bayes (model testing)
library("e1071")





# casefolding
gi <- gsub("[[:digit:]]", "", Tiga_Periode$'Tweet Text') #menghilangkan angka
pc <- gsub("[[:punct:]]","", gi)
pc
tl <- tolower(gi)
rh <- remove.hashtag(tl)
rm <- remove.mention(rh)
ra <- str_remove(rm, "[<,]")
gs <- gsub("+0001f602|+0001f64f|+0001f97a|+0001f643|0001f308|+0001f92a", "", ra)
ga <- gsub("\n", "", gs)
gb <- gsub("<|>|?|!|-|&|=|:|", "", ga)
gc <- str_remove_all(gb, "[+]")
gc










#############################Sentiment analysis di buku###################################

# 2.1 The sentiments datasets
library(tidytext)
library(textdata)
get_sentiments("afinn") #kamus dengan score
summary(get_sentiments("afinn")$value)

get_sentiments("bing") #kamus (2 kategori/simpel)
summary(get_sentiments("bing"))

get_sentiments("nrc") #kamus sentimen analisis (7 perasaan)
summary(get_sentiments("nrc")$'sentiment')

# 2.2 Sentiment analysis with inner join
library(janeaustenr)
library(dplyr)
library(stringr)

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

library(tidyr)

jane_austen_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

library(ggplot2)

ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

# 2.3 Comparing the three sentiment dictionaries
pride_prejudice <- tidy_books %>% 
  filter(book == "Pride & Prejudice")

pride_prejudice

afinn <- pride_prejudice %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(
  pride_prejudice %>% 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  pride_prejudice %>% 
    inner_join(get_sentiments("nrc") %>% 
                 filter(sentiment %in% c("positive", 
                                         "negative"))
    ) %>%
    mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", "negative")) %>% 
  count(sentiment) #jumlah kata negatif dan positif

get_sentiments("bing") %>% 
  count(sentiment)

# 2.4 Most common positive and negative words
bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

custom_stop_words <- bind_rows(tibble(word = c("miss"),  
                                      lexicon = c("custom")), 
                               stop_words)

View(custom_stop_words)

# 2.5 Wordclouds
library(wordcloud)

tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

library(reshape2)

tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

# 2.6 Looking at units beyond just words
p_and_p_sentences <- tibble(text = prideprejudice) %>% 
  unnest_tokens(sentence, text, token = "sentences")
## Let's look at just one.
p_and_p_sentences$sentence[2]

austen_chapters <- austen_books() %>%
  group_by(book) %>%
  unnest_tokens(chapter, text, token = "regex", 
                pattern = "Chapter|CHAPTER [\\dIVXLC]") %>%
  ungroup()

austen_chapters %>% 
  group_by(book) %>% 
  summarise(chapters = n())

bingnegative <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")

wordcounts <- tidy_books %>%
  group_by(book, chapter) %>%
  summarize(words = n())

tidy_books %>%
  semi_join(bingnegative) %>%
  group_by(book, chapter) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts, by = c("book", "chapter")) %>%
  mutate(ratio = negativewords/words) %>%
  filter(chapter != 0) %>%
  slice_max(ratio, n = 1) %>% 
  ungroup()



