library(tm)
library(ggplot2)
library(NLP)
library(caret)

library(readr)
setwd("/cloud/project")
data <- read.csv("train_fix.csv")
data
str(data)
data$Judul
data$kategori

data$kategori = factor(data$kategori)
datacorpus = VCorpus(VectorSource(data$Judul))
datacorpus
str(datacorpus)

dataDTM = DocumentTermMatrix(datacorpus,control=list(tolower=TRUE,
                                                        removeNumbers=TRUE,
                                                        stopwords=TRUE,
                                                        removePunctuation=TRUE,
                                                        stemming=TRUE))
t = removeSparseTerms(dataDTM, sparse = 0.98)
m = as.matrix(t)
View(m)

convert_counts<-function(x){x<-ifelse(x>0,"fakta","hoax")}
train = apply(m, MARGIN = 2, convert_counts)
View(train)

#random forest
model = randomForest(data$kategori~., data = train, ntree = 100)
model

# testing random forest
library(readr)
setwd("/cloud/project")
uji <- read.csv("test_fix.csv")
ujicorpus = VCorpus(VectorSource(uji$Judul))
ujicorpus
str(ujicorpus)

ujiDTM = DocumentTermMatrix(ujicorpus,control=list(tolower=TRUE,
                                                     removeNumbers=TRUE,
                                                     stopwords=TRUE,
                                                     removePunctuation=TRUE,
                                                     stemming=TRUE))
ujiDTM
ujifreq = findFreqTerms(ujiDTM,1)
ujifreq

ujiDTMfreq = ujiDTM[,ujifreq]
ujiDTMfreq




uji_t = removeSparseTerms(ujiDTM, sparse = 0.98)
uji_m = as.matrix(uji_t)
View(uji_m)

convert_count<-function(y){y<-ifelse(y>0,"fakta","hoax")}
test = apply(ujiDTMfreq, MARGIN = 2, convert_count)
View(test)

hasil = predict(model,test,type="class")
hasil = predict(model,test,type="raw")
hasil = confusionMatrix(hasil, reference = factor(uji$kategori))
hasil

#rf prediksi
set.seed(1234)
caret::confusionMatrix(
  data = predict(model, test)
)
pred = predict(model,test)


dataDTM
inspect(dataDTM)
str(dataDTM)
dataDTM$dimnames$Terms

datafreq = findFreqTerms(dataDTM,1)
datafreq

dataDTMfreq = dataDTM[, datafreq]
dataDTMfreq

View(inspect(dataDTMfreq))

convert_counts<-function(x){x<-ifelse(x>0,"hoax","fakta")}
train = apply(dataDTMfreq, MARGIN = 2, convert_counts)
train

#random forest
install.packages("randomForest")
install.packages("reprtree")
library(reprtree)
library(randomForest)



kategori = data$kategori
View(kategori)

library(readr)
baru <- read.csv("data_sindy.csv")
baru
barucorpus = VCorpus(VectorSource(baru$Judul))
barucorpus
str(barucorpus)

baruDTM = DocumentTermMatrix(barucorpus,control=list(tolower=TRUE,
                                                     removeNumbers=TRUE,
                                                     stopwords=TRUE,
                                                     removePunctuation=TRUE,
                                                     stemming=TRUE))
t_baru = removeSparseTerms(baruDTM, sparse = 0.98)
m_baru = as.matrix(t_baru)
View(m_baru)





text = baru$text
View(text)

Tibble_baru <- tibble(line = 1:3000, 
                            text=text)

Token_baru <- Tibble_baru %>%
  unnest_tokens(output=word, input=text) %>%
  count(line, word, sort=T)
Token_baru

Token_baru <- Token_baru %>%
  arrange(line)

View(Token_baru)

dtm_tweet<- Token_baru %>%
  cast_dtm(line, word, n)
dtm_df <- data.frame(as.matrix(dtm_tweet))
View(dtm_df)

#buat data frame
olah = data.frame(text, kategori)
View(olah)
str(olah)

#membuat corpus
corpus = Corpus(VectorSource(olah))
length(corpus)

myStopwords = c(stopwords(),"akan")

tdm = TermDocumentMatrix(corpus,
                         control = list(weighting = weightTfIdf,
                                        stopwords = myStopwords,
                                        removePunctuation = T,
                                        removeNumbers = T,
                                        stemming = T))
tdm

#TermFrequensi
corpusdatabersih4 <- Corpus(VectorSource(DataSentimen$text))
tdm <- TermDocumentMatrix(corpusdatabersih4, control =
                            list(minWordLength=c(1,Inf)))
tdm

freq=rowSums(as.matrix(tdm))
View(freq)
x = (tdm)
head(freq,10)
tail(freq,10)

plot(sort(freq, decreasing = T),col="blue",main="Word TF-IDF frequencies", xlab="TF-IDF-based rank", ylab = "TF-IDF")

tail(sort(freq),n=10)

high.freq=tail(sort(freq),n=10)
hfp.df=as.data.frame(sort(high.freq))
hfp.df$names <- rownames(hfp.df) 
View((hfp.df$names))

ggplot(hfp.df, aes(reorder(names,high.freq), high.freq)) +
  geom_bar(stat="identity") + coord_flip() + 
  xlab("Terms") + ylab("Frequency") +
  ggtitle("Term frequencies")

tail(hfp.df, n=6759)
