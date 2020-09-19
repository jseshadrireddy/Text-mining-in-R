library(selectr)
library(rvest)
library(XML)
library(magrittr)
library(httr)
url="https://www.imdb.com/title/tt0468569/reviews?ref_=tt_ov_rt"
N_pages=5
a=NULL
for (i in 1:10){
  murl = read_html(as.character(paste(url,i,sep="=")))
  rev = murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  a = c(a,rev)
}
length(a)
View(a)
write.table(a, "moviereview.text", row.names=F)
getwd()
text=read.delim("C:\\Users\\j seshadri reddy\\Documents\\moviereview.text")
str(text)
View(text)
head(text,1)
library(tm)
corpus=text[-1,]
### creating corpus
corpus=Corpus(VectorSource(corpus))
inspect(corpus[1:2])
## corpus cleaning
corpus=tm_map(corpus, tolower)
corpus=tm_map(corpus, removePunctuation)
corpus=tm_map(corpus, removeNumbers)
corpus=tm_map(corpus, stripWhitespace)
corpus=tm_map(corpus, removeWords, stopwords("english"))
removeURL = function(x) gsub('http[[:alnum:]]*','',x)
cleanset= tm_map(corpus, content_transformer(removeURL))
inspect(cleanset[1:2])
cleanset=tm_map(cleanset, removeWords, c('one','just','see',
                                          'can','also','even','yes','film','movie','will'))
cleanset=tm_map(cleanset, stemDocument)
## term document matrix
tdm=TermDocumentMatrix(cleanset)
str(tdm)
tdm=as.matrix(tdm)
dim(tdm)
## word clould
a=sort(rowSums(tdm), decreasing = TRUE)
d=data.frame(word=names(a), freq=a)
library(wordcloud)
wordcloud(words=d$word, freq=d$freq, min.freq=10, max.words=100, random.order = FALSE, 
          rot.per = 0.3, colors=brewer.pal(1,"Dark2"))
## most freqent words
barplot(d$freq[1:8], las=2, names.arg=d$word[1:8], col="lightblue",
        main="Most frequent words", ylab="word frequencies")
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
## sentiment analysis
test=read.delim("C:\\Users\\j seshadri reddy\\Documents\\moviereview.text")
text1=as.character(test[-1,])
class(text1)
removeURL = gsub('http[[:alnum:]]*','',text1)
text2=iconv(removeURL)
s_v=get_sentences(text2)
sentiment_vector<-get_sentiment(s_v, method="bing")
sum(sentiment_vector)
summary(sentiment_vector)
### most negative
negative=s_v[which.min(sentiment_vector)]
negative
### most positive
positive=s_v[which.max(sentiment_vector)]
positive
plot(sentiment_vector, type="l", main="plot Trajectory",
     xlab="narrative", ylab="emotional valence")
#### using urc method
library(tokenizers)
words=tokenize_words(s_v)
words=unlist(words)
s_nrc=get_nrc_sentiment(s_v)
head(s_nrc)
barplot(colSums(prop.table(s_nrc[1:10])), las=1,xlab="persentage",
        main="Emotions", horiz=T, col=1:8)
ft_values=get_transformed_values(sentiment_vector, low_pass_size=3,
                                  x_reverse_len = 100, scale_vals = TRUE, scale_range = FALSE)
plot(ft_values, type="h", main="Transformed values", 
     xlab="narrative time", ylab="emotional valence", col="blue")
