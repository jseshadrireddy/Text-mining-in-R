library(httr)
library(twitteR)
library(ROAuth)
library(base64enc)
library(httpuv)
cred=OAuthFactory$new( consumerKey="o3B3rdVR2CECg6pdB7HbtS4EG",
                       consumerSecret="B1RHiIvCVnO2PQqunhkAgrxpIS2x83PxwGnKDtmxDqTNBJFBCZ",
                       requestURL="1214726496369012741-Aj3cY0ZuwmyOkIUsl0oE9WLWaRIEId",
                       accessURL="5kHhnc6ErbuS66tvDzORMwYdyafqEXuZz6nk9YOIFOHA0",
                       authURL='https://api.twitter.com/oauth/authorize'  )

save(cred, file="twitter authentication.Rdata")
load("twitter authentication.Rdata")

setup_twitter_oauth("o3B3rdVR2CECg6pdB7HbtS4EG",
                    "B1RHiIvCVnO2PQqunhkAgrxpIS2x83PxwGnKDtmxDqTNBJFBCZ",  
                    "1214726496369012741-Aj3cY0ZuwmyOkIUsl0oE9WLWaRIEId", 
                    "5kHhnc6ErbuS66tvDzORMwYdyafqEXuZz6nk9YOIFOHA0"       )
tweets=userTimeline('@narandramodi', n=1000, includeRts = T)
tweetsdf=twListToDF(tweets)

write.csv(tweetsdf, "tweets(3).csv", row.names=F)
getwd()
head(tweets)
library(tm)
library(slam)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
text=readLines('C://Users//j seshadri reddy//Desktop//ds assignments//text mining//tweets(3).csv')
View(text)

docs=Corpus(VectorSource(text))
removeURL = function(x) gsub('http[[:alnum:]]*','',x)
docs= tm_map(docs, content_transformer(removeURL))
docs=tm_map(docs, tolower)
docs=tm_map(docs, removeNumbers)
docs=tm_map(docs, removeWords, stopwords("english"))
docs=tm_map(docs, removeWords,c("uuu","will","see","if","uuueu","efalsenana",
                                "uuufuuf","uuuub","uuuu","one",
                                "yes","much","see","even","just","ufeeuffufduffauff"))
docs=tm_map(docs, removePunctuation)
docs=tm_map(docs, stripWhitespace)
docs=tm_map(docs, stemDocument)
inspect(docs[1:2])
## term document matrix
tdm=TermDocumentMatrix(docs)
a=as.matrix(tdm)
dim(a)
### wordcloud
b=sort(rowSums(a), decreasing=TRUE)
d=data.frame(word=names(b), freq=b)
wordcloud(word=d$word, freq=d$freq, min.freq=20, max.words=100,
          rot.per=0.35, color=brewer.pal(1, "Dark2" ))
## most freqent words
barplot(d$freq[1:9], las=2, names.arg=d$word[1:9], col="lightblue", 
        main="most frequent words", ylab="word frequences")
### sentiment analysis
library(syuzhet)
my_text=readLines('C://Users//j seshadri reddy//Desktop//ds assignments//text mining//tweets(3).csv')

t=gsub('http[[:alnum:]]*','',my_text)
t1=iconv(t)
data1=gsub('\"', "", t1, fixed = TRUE)


sv=get_sentences(data1)
sentiment_vec<-get_sentiment(data1, method="bing")
summary(sentiment_vec)
sum(sentiment_vec)
afinn_sv=get_sentiment(data1, method="afinn")
nrc_sv=get_sentiment(data1,method="nrc")
plot(sentiment_vec, type="l", main="plot Trejoctory",
     xlab="Narrative", ylab="Emotional Valence")
nrc_sv=get_nrc_sentiment(data1)
head(nrc_sv)
barplot(sort(colSums(prop.table(nrc_sv[, 1:10]))),  cex.names=0.7, 
        las=1, main="emotions", xlab="percentages", col=1:10)
ft_values=get_transformed_values(sentiment_vec, low_pass_size=2,
                                 x_reverse_len = 100, scale_vals = TRUE, 
                                 scale_range = FALSE)
plot(ft_values, type="h", main="Transformed values", xlab="narrative time",
     ylab="emotional valence", col="blue")
