# Import libraries
library(tm)
library(NLP)
library(caret)
library(dplyr)
library(parallel)
library(tau)
library(stopwords)
library(tidytext)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(tidyverse)
library(SnowballC) 


#import data (per 100 data)

####### Data11 ######

library(readr)
data <- read.csv("data_sindy.csv")
data = data$Judul
View(data)
str(data)



# merubah xlsx menjadi corpus
data = Corpus(VectorSource(data))
inspect(data[1:3000]) #jumlah data menyesuaikan


# Case folding
data = tm_map(data,content_transformer(tolower))
inspect(data[1:3000])


# menghapus retweet
removeRT <- function(x){
  gsub("(rt|via)((?:\\b\\W*@\\w+)+)", "", x)
}
data = tm_map(data,content_transformer(removeRT))
inspect(data[1:3000])

# Menghapus hastag
removeHashtag <- function(x){
  gsub("#\\S+", "", x)
}
data = tm_map(data,content_transformer(removeHashtag))
inspect(data[1:3000])

# Menghapus URL
removeURL <- function(x){
  gsub("http[^[:space:]]*", "", x)
}
data = tm_map(data,content_transformer(removeURL))
inspect(data[1:3000])

# Menghapus Mention
removeMention <- function(x){
  gsub("@\\w+", "", x)
}
data = tm_map(data,content_transformer(removeMention))
inspect(data[1:3000])

# Mengahpus /r dan /n
removeCarriage <- function(x){
  gsub("[\r\n]", "", x)
}
data = tm_map(data,content_transformer(removeCarriage))
inspect(data[1:3000])

# Menghapus Emoji
removeEmoticon <- function(x){
  gsub("[^\x01-\x7F]", "", x)
}
data = tm_map(data,content_transformer(removeEmoticon))
inspect(data[1:3000])

# Invoice removal
removeInvoice <- function(x){
  gsub("inv/[0-9]+/+[xvi]+/[xvi]+/[0-9]+", "", x, ignore.case = T)
}
data = tm_map(data,content_transformer(removeInvoice))
inspect(data[1:3000])

# Menghapus tambahan
toSpace <- content_transformer(function(x, pattern){
  gsub(pattern, " ", x)
})
# tanda baca
data = tm_map(data,toSpace,"[[:punct:]]")
inspect(data[1:3000])
# Angka
data = tm_map(data,toSpace,"[[:digit:]]")
inspect(data[1:3000])
# Spasi lebih %>% 
data = tm_map(data,stripWhitespace)
inspect(data[1:3000])

data = data.frame(text = sapply(data,as.character),
                       tringsAsFactors = FALSE)
View(data)

#menyimpan data cleaned
write.csv(data,file="data_baru.csv")

