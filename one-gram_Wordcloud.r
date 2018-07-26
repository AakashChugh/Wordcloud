rm(list = ls())

install.packages(c("twitteR","ROAuth","base64enc",
                   "httpuv","tm","SnowballC","wordcloud","RColorBrewer"))
library(twitteR)
library(ROAuth)
library(base64enc)
library(httpuv)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)


cred <- OAuthFactory$new(consumerKey='xxxxx',
                         consumerSecret='xxxxx',
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')



## Usage of the following function 
## setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

setup_twitter_oauth("xxxxxxx", 
                    "xxxxxx",
                    "xxxx-xxx",
                    "xx")




## to extract tweets based on a particular word
zuckerberg_tweets = searchTwitter('zuckerberg', n=1500, lang="en", resultType = "recent")
class(zuckerberg_tweets)
str(zuckerberg_tweets)
zuckerberg_tweets[1:5]


## to get text from the tweets
zuckerberg_text = sapply(zuckerberg_tweets, function(x) x$getText())
zuckerberg_text[1]

zuckerberg_text<- phase1_text$text
# create a corpus
zuckerberg_Corpus = Corpus(VectorSource(zuckerberg_text))
zuckerberg_Corpus
inspect(myCorpus[1])



                     ######### Data prep ###########

## remove hashtags 
removeHashTags <- function(x) gsub("#\\S+", "", x)
zuckerberg_Corpus <- tm_map(zuckerberg_Corpus, content_transformer(removeHashTags))

## remove twitter handles
removeTwitterHandles <- function(x) gsub("@\\S+", "", x)
zuckerberg_Corpus <- tm_map(zuckerberg_Corpus, content_transformer(removeTwitterHandles))


## remove emojis

zuckerberg_Corpus = tm_map(zuckerberg_Corpus, content_transformer(gsub), pattern="\\W",replace=" ") 

## remove URL 
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
zuckerberg_Corpus <- tm_map(zuckerberg_Corpus, content_transformer(removeURL))

## to lower
zuckerberg_Corpus <- tm_map(zuckerberg_Corpus, content_transformer(tolower)) 

## remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
zuckerberg_Corpus <- tm_map(zuckerberg_Corpus, content_transformer(removeNumPunct))

## remove punctuation 
zuckerberg_Corpus = tm_map(zuckerberg_Corpus, removePunctuation)

## remove numbers 
zuckerberg_Corpus = tm_map(zuckerberg_Corpus, removeNumbers)

## remove stopwords 
zuckerberg_Corpus = tm_map(zuckerberg_Corpus, removeWords, stopwords("english"))

## strip whitespace
zuckerberg_Corpus = tm_map(zuckerberg_Corpus, stripWhitespace)

## clean frquent words based on the your case
zuckerberg_Corpus<- tm_map(zuckerberg_Corpus, removeWords, c("wir","bckrst"))

## term document matrix: important to create wordcloud

zuckerberg_dtm<- TermDocumentMatrix(zuckerberg_Corpus)

zuckerberg_matrix <- as.matrix(zuckerberg_dtm)
v <- sort(rowSums(zuckerberg_matrix),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)



## wordcloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=FALSE, 
          colors=brewer.pal(8, "Paired"))





         
     
