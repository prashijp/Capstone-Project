data_dir <- "/Users/jeevarehakrishnaraj/Desktop/Springboard/Capstone Project/Capstone" 
twitter_airline <- read.csv(file.path(data_dir,"tweets.csv"),header = TRUE)

# Replace twitter handle with blank 
twitter_airline$text <- gsub("@VirginAmerica","",gsub("@AmericanAir","",gsub("@JetBlue ","",gsub("@SouthwestAir","",gsub("@united","",gsub("@USAirways","",twitter_airline$text))))))

#Build a corpus, and specify the source to be character vectors
tweets_corpus <- Corpus(VectorSource(twitter_airline$text))

#Inspect Corpus
inspect(tweets_corpus[1:2])

# Remove punctuations from corpus
tweets_corpus <- tm_map(tweets_corpus,removePunctuation)

# Remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
tweets_corpus <- tm_map(tweets_corpus,content_transformer(removeURL))

# Remove anything expect English and Space
remove_others <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
tweets_corpus <- tm_map(tweets_corpus,content_transformer(remove_others))

# Remove Stopwords
tweets_stopwords <- c(setdiff(stopwords('english'), c("r", "big")),"use", "see", "used", "via", "amp")
tweets_corpus <- tm_map(tweets_corpus,removeWords,tweets_stopwords)

# Remove extra whitespace
tweets_corpus <- tm_map(tweets_corpus,stripWhitespace)

# Convert the corpus to lowercase 
tweets_corpus <- tm_map(tweets_corpus,content_transformer(tolower))


# Make a copy of the corpus
tweets_corpus_copy <- tweets_corpus

#Inspect the corpus
inspect(tweets_corpus[4])

#Stemming 
tweets_corpus <- tm_map(tweets_corpus,stemDocument)

#Inspect the corpus
inspect(tweets_corpus[4])

# Stem Completion
tweets_corpus <- tm_map(tweets_corpus,content_transformer(stemCompletion), dictionary = tweets_corpus_copy)

# Creating Term Document Matrix
tweets_tdm <- TermDocumentMatrix(tweets_corpus, control = list(wordLengths = c(1,Inf)))

for(i in 1:5) { 
  cat(paste("[[",i,"]] ",sep = "")) 
  writeLines(as.character(tweets_corpus[[i]]))
  }

# Create Term Document Martix
tweet_tdm <- TermDocumentMatrix(tweets_corpus, control = list(wordLengths = c(1,Inf)))
tweet_tdm

# find the frequency of the word
#which(apply(tweet_tdm,1,sum)>2)
#freq.terms <- findFreqTerms(tweet_tdm,lowfreq = 2)
#freterm.freq <- rowSums(as.matrix(tweet_tdm))
#tweets_df <- data.frame(term = names(term.freq), freq = term.freq)
#ggplot(tweets_df,aes( x= term, y = freq))+ geom_bar(stat = "identity") + xlab("Terms")+ ylab("count") +coord_flip()
#wordcloud(tweets_corpus, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.25, use.r.layout=FALSE, colors=brewer.pal(8, 'Dark2'))

# alternate method
m  <- as.matrix(tweet_tdm)
v <- sort(rowSums(m),decreasing = TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

freq.terms <- findFreqTerms(tweet_tdm, lowfreq = 1)
term.freq <- rowSums(as.matrix(tweet_tdm))
df <- data.frame(term = names(term.freq), freq = term.freq)
library(sentiment)
sentiments <- sentiment(twitter_airline$text)

sentiments$score <- 0
sentiments$score[sentiments$polarity == "positive"] <- 1
sentiments$score[sentiments$polarity == "negative"] <- -1
#sentiments$date <- as.IDate(probius_twitter$created)
t = table(sentiments$polarity)
ggplot(as.data.frame(t), aes(Var1, Freq)) + geom_bar(stat = 'identity', fill = 'lightblue', color = 'black') + xlab("Polarity") + ylab("Tweets Count")


# Retrieve Data for Delta airline
delta <- subset(twitter_airline,airline == "Delta")
delta.txt  <- delta$text
delta_sentiment <- sentiment(delta.txt)
delta_sentiment$score <- 0
delta_sentiment$score[delta_sentiment$polarity == "positive"] <- 1
delta_sentiment$score[delta_sentiment$polarity == "negative"] <- -1
delta_table <- table(delta_sentiment$polarity)
ggplot(delta_sentiment, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +geom_text(stat='count',aes(label=..count..),vjust=-0.2)+
  scale_fill_brewer(palette="Dark2") +
  labs(x="Polarity", y="Number of Tweets") +
  ggtitle("Twitter Sentiment Analysis of Delta Airlines")+
  theme(plot.title = element_text(hjust = 0.5))
delta_sentiment$airline = 'Delta'
delta_sentiment$code = 'DL'
colnames(delta_sentiment)

# Retrieve Data for American airline
american <- subset(twitter_airline,airline == "American")
american.txt  <- american$text
american_sentiment <- sentiment(american.txt)
american_sentiment$score <- 0
american_sentiment$score[american_sentiment$polarity == "positive"] <- 1
american_sentiment$score[american_sentiment$polarity == "negative"] <- -1
american_table <- table(american_sentiment$polarity)
ggplot(american_sentiment, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +geom_text(stat='count',aes(label=..count..),vjust=-0.2)+
  scale_fill_brewer(palette="Dark2") +
  labs(x="Polarity", y="Number of Tweets") +
  ggtitle("Twitter Sentiment Analysis of American Airlines")+
  theme(plot.title = element_text(hjust = 0.5))
american_sentiment$airline = 'American'
american_sentiment$code = 'AA'
colnames(american_sentiment)

# Retrieve Data for United airline
united <- subset(twitter_airline,airline == "United")
united.txt  <- united$text
united_sentiment <- sentiment(united.txt)
united_sentiment$score <- 0
united_sentiment$score[united_sentiment$polarity == "positive"] <- 1
united_sentiment$score[united_sentiment$polarity == "negative"] <- -1
united_table <- table(united_sentiment$polarity)
ggplot(united_sentiment, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +geom_text(stat='count',aes(label=..count..),vjust=-0.2)+
  scale_fill_brewer(palette="Dark2") +
  labs(x="Polarity", y="Number of Tweets") +
  ggtitle("Twitter Sentiment Analysis of United Airlines")+
  theme(plot.title = element_text(hjust = 0.5))
united_sentiment$airline = 'United'
united_sentiment$code = 'UA'
colnames(united_sentiment)

# Retrieve Data for Southwest airline
southwest <- subset(twitter_airline,airline == "Southwest")
southwest.txt  <- southwest$text
southwest_sentiment <- sentiment(southwest.txt)
southwest_sentiment$score <- 0
southwest_sentiment$score[southwest_sentiment$polarity == "positive"] <- 1
southwest_sentiment$score[southwest_sentiment$polarity == "negative"] <- -1
southwest_table <- table(southwest_sentiment$polarity)
ggplot(southwest_sentiment, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +geom_text(stat='count',aes(label=..count..),vjust=-0.2)+
  scale_fill_brewer(palette="Dark2") +
  labs(x="Polarity", y="Number of Tweets") +
  ggtitle("Twitter Sentiment Analysis of Southwest Airlines")+
  theme(plot.title = element_text(hjust = 0.5))
southwest_sentiment$airline = 'Southwest'
southwest_sentiment$code = 'SW'
colnames(southwest_sentiment)

# Retrieve Data for US Airways
us_airways <- subset(twitter_airline,airline == "US Airways")
us_airways.txt  <- us_airways$text
us_airways_sentiment <- sentiment(us_airways.txt)
us_airways_sentiment$score <- 0
us_airways_sentiment$score[us_airways_sentiment$polarity == "positive"] <- 1
us_airways_sentiment$score[us_airways_sentiment$polarity == "negative"] <- -1
us_airways_table <- table(us_airways_sentiment$polarity)
ggplot(us_airways_sentiment, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +geom_text(stat='count',aes(label=..count..),vjust=-0.2)+
  scale_fill_brewer(palette="Dark2") +
  labs(x="Polarity", y="Number of Tweets") +
  ggtitle("Twitter Sentiment Analysis of Us_airways Airlines")+
  theme(plot.title = element_text(hjust = 0.5))
us_airways_sentiment$airline = 'US Airways'
us_airways_sentiment$code = 'UW'
colnames(us_airways_sentiment)

# Retrieve Data for Virgin America
virgin_america <- subset(twitter_airline,airline == "Virgin America")
virgin_america.txt  <- virgin_america$text
virgin_america_sentiment <- sentiment(virgin_america.txt)
virgin_america_sentiment$score <- 0
virgin_america_sentiment$score[virgin_america_sentiment$polarity == "positive"] <- 1
virgin_america_sentiment$score[virgin_america_sentiment$polarity == "negative"] <- -1
virgin_america_table <- table(virgin_america_sentiment$polarity)
ggplot(virgin_america_sentiment, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +geom_text(stat='count',aes(label=..count..),vjust=-0.2)+
  scale_fill_brewer(palette="Dark2") +
  labs(x="Polarity", y="Number of Tweets") +
  ggtitle("Twitter Sentiment Analysis of Virgin America ")+
  theme(plot.title = element_text(hjust = 0.5))
virgin_america_sentiment$airline = 'Virgin America'
virgin_america_sentiment$code = 'VA'
acolnames(virgin_america_sentiment)

# Combining Data for Virgin America
all.sentiment <- rbind(delta_sentiment,american_sentiment,united_sentiment,southwest_sentiment,virgin_america_sentiment,us_airways_sentiment)
all.sentiment$score <- 0
 all.sentiment$score[all.sentiment$polarity == "Very Positive"] <- 1
 all.sentiment$score[all.sentiment$polarity == "Positive"] <- 0.5
 all.sentiment$score[all.sentiment$polarity == "Negative"] <- -0.5
 all.sentiment$score[all.sentiment$polarity == " Very Negative"] <- -1
 ggplot(all.sentiment, aes(x=polarity)) +
  +     geom_bar(aes(y=..count.., fill=polarity)) +geom_text(stat='count',aes(label=..count..),vjust=-0.2)+
  +     scale_fill_brewer(palette="Dark2") +
  +     labs(x="Polarity", y="Number of Tweets") +
  +     ggtitle("Twitter Sentiment Analysis of US based airlines ")+
  +     theme(plot.title = element_text(hjust = 0.5))
 
 



all.airlines <- rbind(delta,american,united,us_airways,virgin_america,southwest)
all.airlines$updatedsentiment <- all.sentiment$polarity
airlines= all.airlines %>% group_by(airline) %>% dplyr::summarise(count=n())
posNegByAirline <-dcast(all.airlines, airline ~ updatedsentiment)
posNegByAirline

#Timeline of Negative tweets.
negativeTweets <- all.airlines %>% filter(updatedsentiment =="negative")
negativeTweetsByDate <- negativeTweets %>% group_by(tweet_created) %>% dplyr::summarise(count = n())
negativeTweetsByDatePlot = ggplot() + geom_line(data=negativeTweetsByDate, aes(x=tweet_created, y=count, group = 1)) 
negativeTweetsByDatePlot

#Timeline of Negative tweets by Airlines
negativeTweetsByDateByAirline <- negativeTweets %>% group_by(airline,tweet_created) %>% dplyr::summarise(count = n())
negativeTweetsByDateByAirlinePlot = ggplot() + geom_line(data=negativeTweetsByDateByAirline, aes(x=tweet_created, y=count, group =airline , color=airline)) 
negativeTweetsByDateByAirlinePlot
