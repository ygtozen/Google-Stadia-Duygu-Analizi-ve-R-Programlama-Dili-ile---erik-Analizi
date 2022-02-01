
#ingilizce kelime analizi icin
install.packages("tidytext")
library(tidytext)

install.packages("textdata")
library(textdata)

install.packages("dplyr")
library(dplyr)

#sozluk 1
get_sentiments("bing")
#bu hazir paket kelimeleri pozitif ve negatif olarak ayirmis yaklasik 6786 kelime

#sozluk 2
get_sentiments("afinn")
#bu sozlukte kelimelere sayi verilmis

#sozluk 3
get_sentiments("nrc")
#bu sozlukte kelimler duygusal olarak nitelendirilmistir 13 900 kelime


#bingde pozitif negatif kelimeleri sayalim
get_sentiments("bing") %>%
  count(sentiment)
#4781 negatif 2005 pozitif kelime var

#metin madenciligi paketi veri setini temizlemek icin
install.packages("tm")
#kutuphaneden cagirilir
library(tm)

install.packages("wordcloud")
install.packages("ggplot2")
install.packages("stringr")
install.packages("plyr")
install.packages("e1071")
install.packages("RColorBrewer")
install.packages("readxl")
install.packages("SnowballC")
install.packages("rpart.plot")
install.packages("randomForest")
install.packages("caTools")


library(wordcloud)
library(ggplot2)
library(e1071)
library(stringr)
library(plyr)
library(RColorBrewer)
library(readxl)
library(SnowballC)
library(rpart.plot)
library(randomForest)
library(caTools)


library(readxl)
tweets <- read_excel("C:/Users/yigit/Desktop/stadia.xlsx")
tweets.df <-as.data.frame(tweets)
colnames(tweets.df)
#text kolonunda tweetlerin iÃ§eriÄŸi, screenName kolonunda ise tweeti atan kiÅŸiye iliÅŸkin bilgiye ulaÅŸÄ±labilmektedir. 
#Bu iki kolonun iÃ§erisinde yer alan bilgiler sosyal aÄŸ iliÅŸkilerini gÃ¶stermek aÃ§Ä±sÄ±ndan yeterli iÃ§erik saÄŸlamaktadÄ±r.

library(stringi)
library(dplyr)
library(tibble)
library(qdapTools)

library(openssl)
library(httpuv)
library(stringr)

tweet_clean <- tweets.df
tweet_clean$text <- stri_enc_toutf8(tweet_clean$text)

#RT ifadelerinin kaldÄ±rÄ±lmasÄ±
tweet_clean$text <- ifelse(str_sub(tweet_clean$text,1,2) == "RT",
                           substring(tweet_clean$text,3),
                           tweet_clean$text)
#URL linklerinin temizlenmesi
tweet_clean$text <- str_replace_all(tweet_clean$text, "http[^[:space:]]*", "")


#Hashtag "#" ve "@" iþaretlerinin kaldýrýlmasý
tweet_clean$text <- str_replace_all(tweet_clean$text, "#\\S+", "")
tweet_clean$text <- str_replace_all(tweet_clean$text, "@\\S+", "")

#Noktalama iþaretlerinin temizlenmesi
tweet_clean$text <- str_replace_all(tweet_clean$text, "[[:punct:][:blank:]]+", " ")

#Tüm harflerin küçük harfe dönüþtürülmesi
tweet_clean$text  <- str_to_lower(tweet_clean$text, "en")

#Rakamlarýn temizlenmesi
tweet_clean$text <- removeNumbers(tweet_clean$text)

#ASCII formatýna uymayan karakterlerin temizlenmesi
tweet_clean$text <- str_replace_all(tweet_clean$text, "[<].*[>]", "")
tweet_clean$text <- gsub("\uFFFD", "", tweet_clean$text, fixed =  TRUE)
tweet_clean$text <- gsub("\n", "", tweet_clean$text, fixed =  TRUE)

#Alfabetik olmayan karakterlerin temizlenmesi
tweet_clean$text <- str_replace_all(tweet_clean$text, "[^[:alnum:]]"," " )

#RDS Dosyaslarýný lokalden okuyoruz
library(readxl)
Turkish_Stopwords <- read_excel(file.choose(),col_names =T)
head(Turkish_Stopwords)

install.packages("tidytext")
library(tidytext)
library(dplyr)
library(ggplot2)
library(srvyr)


tweets.df <- twListToDF(tweets)
colnames(tweets.df)


doc.corpus <- Corpus(VectorSource(tweets.df$text))
doc.corpus <- tm_map(doc.corpus, content_transformer(tolower))
doc.corpus <- tm_map(doc.corpus, content_transformer(removePunctuation))
doc.corpus <- tm_map(doc.corpus,content_transformer(removeNumbers))

removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
myCorpus <- tm_map(doc.corpus, removeURL)
myCorpus <- tm_map(myCorpus, stripWhitespace)#bosluklarin
tdm <- TermDocumentMatrix(myCorpus)
findFreqTerms(tdm, lowfreq = 500)

wordcloud(tweet_clean$text,min.freq = 2, scale = c(2,0,5),colors = brewer.pal(8,"Dark2"),
          random.color = TRUE, random.order = FALSE, max.words = 500)
