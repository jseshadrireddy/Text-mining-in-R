library(selectr)
library(rvest)
library(XML)
library(magrittr)
url="https://www.amazon.in/Redmi-Note-Neptune-Blue-Storage/product-reviews/B07X1KT6LD/ref=cm_cr_arp_d_paging_btm_next_2?ie=UTF8&reviewerType=all_reviews&pageNumber=2"
N_pages=5
a=NULL
for(j in 1:N_pages){
  pant<-read_html(paste0(url, j))
  b<-cbind(pant%>% html_nodes(".review-text")%>%  html_text()   )
  a<-rbind(a,b)
}
write.table(a, "Rreview.text", row.names=F)
getwd()
library(SnowballC)
library(wordcloud)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(httr)
library(reshape2)
library(sentimentr)
library(scales)
library(RCurl)
library(syuzhet)
library(tm)
library(slam)
library(topicmodels)
x=readLines("C:\\Users\\j seshadri reddy\\Documents\\Rreview.text")
View(x)
tail(x)
# cleaning the text data
x_corpus=Corpus(VectorSource(x))
removeURL=function(x) gsub('http[[:alnum:]]*','',x)
x_corpus=tm_map(x_corpus, content_transformer(removeURL))
tospace=function(x, pattern) gsub(pattern," ", x)
x_corpus=tm_map(x_corpus, tospace, "/")
x_corpus=tm_map(x_corpus, tospace, "@")
x_corpus=tm_map(x_corpus, tospace, "\\|")
x_corpus=tm_map(x_corpus,tolower)
x_corpus=tm_map(x_corpus, removePunctuation)
x_corpus=tm_map(x_corpus, removeWords, stopwords('English'))
x_corpus=tm_map(x_corpus, removeWords, c("with", "not","veri","note",
                                          "are","who","that", "but","for","will","you",
                                          "is","to","if","the", "use","and","this","it"
                                          ,"i","from","all","after","was","a","also"))
x_corpus=tm_map(x_corpus, removeNumbers)
x_corpus=tm_map(x_corpus, stripWhitespace)
x_corpus=tm_map(x_corpus, stemDocument)
inspect(x_corpus[76:79])
## wordcloud
wordcloud(x_corpus)
### term document matrix
tdm=TermDocumentMatrix(x_corpus)
tdm
dim(tdm)
tdm1=as.matrix(tdm)
a=sort(rowSums(as.matrix(tdm1)), decreasing = TRUE)
d=data.frame(word=names(a), freq=a)
library(wordcloud)
wordcloud(words=d$word, freq=d$freq, min.freq=10, max.words=500, 
          random.order = FALSE, rot.per = 0.3, 
          colors=brewer.pal(1,"Dark2"))
## most frequent words
barplot(d$freq[1:10], las=2, names.arg=d$word[1:10], col="lightblue", 
        main="Most frequent words", ylab="word frequencies")
## Sentiment analysis
text=readLines("C:\\Users\\j seshadri reddy\\Documents\\Rreview.text")
##removing html nodes
t=gsub('http[[:alnum:]]*','',text)
t=iconv(t)
text1=get_sentences(t)
sentiment_vector=get_sentiment(text1, method="bing")
s_afinn=get_sentiment(text1, method="afinn")
sum(sentiment_vector)
summary(sentiment_vector)
negative=text1[which.min(sentiment_vector)]
negative
positive=text1[which.max(sentiment_vector)]
positive
plot(sentiment_vector, type="l", main="plot Trajectory", xlab="narrative", ylab="emotional valence")
s_nrc=get_nrc_sentiment(text1)
s_nrc[1:20, ]
### graphical representation of sentiment score
barplot(colSums(s_nrc), las=2, col=rainbow(10), ylab='count', main="sentiment score")
library(tokenizers)
words=tokenize_words(text1)
words1=unlist(words)
sw=get_nrc_sentiment(words1)
negative=which(sw$negative>0)
words1[negative[50:70]]
positive=which(sw$positive>0)
words1[positive[1:30]]
percent_val=get_percentage_values(sentiment_vector)
plot(percent_val, type='l', main="percentage_based_meand",
                          xlab="narrative time ", ylab='emotional valence', col="red")
ft_values=get_transformed_values(sentiment_vector, low_pass_size=3, x_reverse_len = 100, 
                                  scale_vals = TRUE, scale_range = FALSE)
plot(ft_values, type="h", main="Transformed values", xlab="narrative time",
     ylab="emotional valence", col="blue")
