library(twitteR)
library(httr)

access.token<-"#token#"
api.secret<-"#secret#"
api.key<-"#key#"
token.secret<-"#secret#"




setup_twitter_oauth(api.key,api.secret,access.token,token.secret)

trump.tweet24<-searchTwitter("@realDonaldTrump", n = 500, since = '2018-1-24', until = '2018-1-25')#state of the union Jan 30th, 9pm E/T





library(tm)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(tidytext)
library(scales)
library(stringr)

tweet.df<-twListToDF(unclean.tweet)#twitterlist to data frame

tot.tweet <-paste(tweet.df$text, collapse = " ")#make into a string

tweet.df$text <- sapply(tweet.df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))#removes emojis/problem Characters


corp.tweet<-Corpus(VectorSource(tweet.df$text))#to Corpus

clean.corp<-tm_map(corp.tweet, content_transformer(tolower))
removeURL<- function(x) gsub("http[^[ : space: ]]*", " ", x)
clean.corp<-tm_map(clean.corp, content_transformer(removeURL))
clean.corp<-tm_map(clean.corp, removeNumbers)
clean.corp<-tm_map(clean.corp, removePunctuation)
clean.corp<-tm_map(clean.corp, removeWords, stopwords('english'))
clean.corp<-tm_map(clean.corp, removeWords, c("realdonaldtrump"))#had to remove, dominated the word frequency charts
clean.corp<-tm_map(clean.corp, stripWhitespace)

#cleaning process



trump.dtm<-DocumentTermMatrix(clean.corp)#corp to documenttermmatrix

tidy.trump<-tidy(trump.dtm)#coercing into a tiddle


tidy.trump%>%
  count(term, sort = TRUE)%>%
  filter(n > 20) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n)) + 
  geom_col()+
  coord_flip()#freq count and visulization

cloud.tweet<-tidy.trump%>%
  count(term, sort = TRUE)#word clouds are fun

wordcloud(cloud.tweet$term, cloud.tweet$n, min.freq = 20)# the Donald is too big for this. 


tidy.trump1<-tweet.df%>%
  unnest_tokens(word, text)%>%
  count(word, sort = TRUE) %>%
  ungroup()

total.count<-tidy.trump1 %>%
  mutate(total = sum(n))#adding a total column

Trump_tf<-total.count %>%
  mutate(rank = row_number(), 'tf' = n/total)#rank and term frequency column

Trump_tf %>%
  ggplot(aes(rank, tf)) +
  geom_line() +
  scale_x_log10() +
  scale_y_log10()
#zipf

trump.sentiment<-tidy.trump %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))
#added bing sentiment to tweets

trump.sentiment %>%
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n >= 5) %>% #low frequency required to pick out more terms
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n , fill = sentiment)) +
  ylab("Sentiment") +
  geom_bar(stat = "identity") +
  coord_flip()
#visulizations of scores