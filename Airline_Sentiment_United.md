# Airline Sentiment
`r format(Sys.Date())`  
### Introduction
Analyzing the US based airlines performances by analyzing the the tweets of the 
airlines.Identifying the sentiments of tweets and classifying them as nuetral,
negative and positive sentiment for each airlines. Identify the issues  behind
negative sentiments and checking the significance of bigrams, trigrams and 
airlines on the sentiment.

### Description of Data Set
The dataset contains important fields like tweet_id,airline_sentiment, 
airline, name, text, tweet_created, tweet_location which will be widely used
in the sentiment analysis.


```r
data_dir <- "~/Desktop/Springboard/Capstone Project/Capstone" 
twitter_airline <- read.csv(file.path(data_dir,"tweets.csv"),header = TRUE)
dim(twitter_airline)
```

```
## [1] 14640    15
```

```r
colnames(twitter_airline)
```

```
##  [1] "tweet_id"                     "airline_sentiment"           
##  [3] "airline_sentiment_confidence" "negativereason"              
##  [5] "negativereason_confidence"    "airline"                     
##  [7] "airline_sentiment_gold"       "name"                        
##  [9] "negativereason_gold"          "retweet_count"               
## [11] "text"                         "tweet_coord"                 
## [13] "tweet_created"                "tweet_location"              
## [15] "user_timezone"
```

```r
knitr::opts_chunk$set(echo = TRUE)
```

### Load the libraries
Loading the libraries required for sentiment analysis.


```r
options(java.parameters = "-Xmx4g")
options(mc.cores=1)
library(tm)
library(dplyr)
library(plyr)
library(sentiment)
library(twitteR)
library(wordcloud)
library(ggplot2)
library(magrittr)
library(tidytext)
library(ggrepel)
library(stringr)
dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_66.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
require(rJava)
#library(rJava)
library(RWeka)
library(slam)
```

### Structure of the dataset

```r
str(twitter_airline)
```

```
## 'data.frame':	14640 obs. of  15 variables:
##  $ tweet_id                    : num  5.7e+17 5.7e+17 5.7e+17 5.7e+17 5.7e+17 ...
##  $ airline_sentiment           : Factor w/ 3 levels "negative","neutral",..: 2 3 2 1 1 1 3 2 3 3 ...
##  $ airline_sentiment_confidence: num  1 0.349 0.684 1 1 ...
##  $ negativereason              : Factor w/ 11 levels "","Bad Flight",..: 1 1 1 2 3 3 1 1 1 1 ...
##  $ negativereason_confidence   : num  NA 0 NA 0.703 1 ...
##  $ airline                     : Factor w/ 6 levels "American","Delta",..: 6 6 6 6 6 6 6 6 6 6 ...
##  $ airline_sentiment_gold      : Factor w/ 4 levels "","negative",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ name                        : Factor w/ 7701 levels "___the___","__betrayal",..: 1073 3477 7666 3477 3477 3477 1392 5658 1874 7665 ...
##  $ negativereason_gold         : Factor w/ 14 levels "","Bad Flight",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ retweet_count               : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ text                        : Factor w/ 14427 levels "_\xf4ִ RT @JetBlue: Our fleet's on fleek. http://t.co/4KH92mKoTZ",..: 14016 13923 13794 13857 13659 13937 14049 13928 14015 13855 ...
##  $ tweet_coord                 : Factor w/ 833 levels "","[-33.87144962, 151.20821275]",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ tweet_created               : Factor w/ 6804 levels "2/16/15 23:36",..: 6414 6397 6397 6397 6396 6396 6395 6394 6393 6376 ...
##  $ tweet_location              : Factor w/ 3082 levels "","  || san antonio, texas||",..: 1 1 1465 1 1 1 2407 1529 2389 1529 ...
##  $ user_timezone               : Factor w/ 86 levels "","Abu Dhabi",..: 32 64 29 64 64 64 64 64 64 32 ...
```
### Summary  of the dataset


```r
summary(twitter_airline)
```

```
##     tweet_id         airline_sentiment airline_sentiment_confidence
##  Min.   :5.676e+17   negative:9178     Min.   :0.3350              
##  1st Qu.:5.686e+17   neutral :3099     1st Qu.:0.6923              
##  Median :5.695e+17   positive:2363     Median :1.0000              
##  Mean   :5.692e+17                     Mean   :0.9002              
##  3rd Qu.:5.699e+17                     3rd Qu.:1.0000              
##  Max.   :5.703e+17                     Max.   :1.0000              
##                                                                    
##                 negativereason negativereason_confidence
##                        :5462   Min.   :0.000            
##  Customer Service Issue:2910   1st Qu.:0.361            
##  Late Flight           :1665   Median :0.671            
##  Can't Tell            :1190   Mean   :0.638            
##  Cancelled Flight      : 847   3rd Qu.:1.000            
##  Lost Luggage          : 724   Max.   :1.000            
##  (Other)               :1842   NA's   :4118             
##            airline     airline_sentiment_gold          name      
##  American      :2759           :14600         JetBlueNews:   63  
##  Delta         :2222   negative:   32         kbosspotter:   32  
##  Southwest     :2420   neutral :    3         _mhertz    :   29  
##  United        :3822   positive:    5         otisday    :   28  
##  US Airways    :2913                          throthra   :   27  
##  Virgin America: 504                          rossj987   :   23  
##                                               (Other)    :14438  
##                                negativereason_gold retweet_count     
##                                          :14608    Min.   : 0.00000  
##  Customer Service Issue                  :   12    1st Qu.: 0.00000  
##  Late Flight                             :    4    Median : 0.00000  
##  Can't Tell                              :    3    Mean   : 0.08265  
##  Cancelled Flight                        :    3    3rd Qu.: 0.00000  
##  Cancelled Flight\nCustomer Service Issue:    2    Max.   :44.00000  
##  (Other)                                 :    8                      
##                       text                            tweet_coord   
##  @united thanks         :    6                              :13621  
##  @AmericanAir thanks    :    5   [0.0, 0.0]                 :  164  
##  @JetBlue thanks!       :    5   [40.64656067, -73.78334045]:    6  
##  @SouthwestAir sent     :    5   [32.91792297, -97.00367737]:    3  
##  @AmericanAir thank you!:    4   [40.64646912, -73.79133606]:    3  
##  @united thank you!     :    4   [18.22245647, -63.00369733]:    2  
##  (Other)                :14611   (Other)                    :  841  
##        tweet_created          tweet_location
##  2/22/15 14:22:   11                 :4733  
##  2/22/15 17:15:   11   Boston, MA    : 157  
##  2/23/15 11:50:   11   New York, NY  : 156  
##  2/22/15 13:06:   10   Washington, DC: 150  
##  2/22/15 14:11:   10   New York      : 127  
##  2/22/15 17:14:   10   USA           : 126  
##  (Other)      :14577   (Other)       :9191  
##                     user_timezone 
##                            :4820  
##  Eastern Time (US & Canada):3744  
##  Central Time (US & Canada):1931  
##  Pacific Time (US & Canada):1208  
##  Quito                     : 738  
##  Atlantic Time (Canada)    : 497  
##  (Other)                   :1702
```
Dataset contains 14640 observations and 15 variables. There are some new 
variables that will be added to the dataset.


### Replace twitter handle with blank 
The tweets contained the airlines twitter handle. We must first remove the 
twitter handle as they should not be used in the text analysis. 


```r
twitter_airline$text <- gsub("@VirginAmerica","",gsub("@AmericanAir","",
 gsub("@JetBlue ","",gsub("@SouthwestAir","",gsub("@united","",
 gsub("@USAirways","", twitter_airline$text))))))
```


### Subsetting united tweets from the dataset


```r
twitter_united <- filter(twitter_airline, airline =="United")
```

### Build and cleaning the corpus 
Here we convert the text into a word corpus using the function VectorSource. 
A word corpus enables us to eliminate common words using the text mining 
package tm. Removing the corpus specific stopwords  lets us focus on the
important words. 

```r
tweets_corpus <- Corpus(VectorSource(twitter_united$text))


# Inspect Corpus
inspect(tweets_corpus[1:5])
```

```
## <<SimpleCorpus>>
## Metadata:  corpus specific: 1, document level (indexed): 0
## Content:  documents: 5
## 
## [1]  thanks                                                                                                                            
## [2]  Thanks for taking care of that MR!! Happy customer.                                                                               
## [3]  still no refund or word via DM. Please resolve this issue as your Cancelled Flightled flight was useless to my assistant's trip.  
## [4]  Delayed due to lack of crew and now delayed again because there's a long line for deicing... Still need to improve service #united
## [5]  thanks -- we filled it out. How's our luck with this? Is it common?
```

### Clean the corpus

```r
# Remove Punctuations
tweets_corpus <- tm_map(tweets_corpus,removePunctuation)


#Remove URLs
removeURL <- function(x) {
  gsub("http[^[:space:]]*", "", x)
}
tweets_corpus <- tm_map(tweets_corpus,content_transformer(removeURL))

# Remove anything expect English and Space
remove_others <- function(x) {
  gsub("[^[:alpha:][:space:]]*","",x)
}
tweets_corpus <- tm_map(tweets_corpus,content_transformer(remove_others))
inspect(tweets_corpus[1:5])
```

```
## <<SimpleCorpus>>
## Metadata:  corpus specific: 1, document level (indexed): 0
## Content:  documents: 5
## 
## [1]  thanks                                                                                                                       
## [2]  Thanks for taking care of that MR Happy customer                                                                             
## [3]  still no refund or word via DM Please resolve this issue as your Cancelled Flightled flight was useless to my assistants trip
## [4]  Delayed due to lack of crew and now delayed again because theres a long line for deicing Still need to improve service united
## [5]  thanks  we filled it out Hows our luck with this Is it common
```

### Convert the corpus to lowercase 

```r
tweets_corpus <- tm_map(tweets_corpus,content_transformer(tolower))

# Remove Stopwords. 
tweets_stopwords <- c(setdiff(stopwords('english'), c("r", "big","united","united","united","airways","airlines","flight","pilot",
 "virgin","US airways","southwest","a","the","is","and")),"use", "see", 
 "used", "via", "amp","the","a","aa","aaaand","i","a","the",
 "flight","airlines","flights","airway","will", "cant","and","is","can","im")
tweets_corpus <- tm_map(tweets_corpus,removeWords,tweets_stopwords)
inspect(tweets_corpus[1:5])
```

```
## <<SimpleCorpus>>
## Metadata:  corpus specific: 1, document level (indexed): 0
## Content:  documents: 5
## 
## [1]  thanks                                                                                              
## [2]  thanks  taking care   mr happy customer                                                             
## [3]  still  refund  word  dm please resolve  issue   cancelled flightled   useless   assistants trip     
## [4]  delayed due  lack  crew  now delayed   theres  long line  deicing still need  improve service united
## [5]  thanks   filled   hows  luck     common
```

### Remove extra whitespace

```r
tweets_corpus <- tm_map(tweets_corpus,stripWhitespace)
inspect(tweets_corpus[1:5])
```

```
## <<SimpleCorpus>>
## Metadata:  corpus specific: 1, document level (indexed): 0
## Content:  documents: 5
## 
## [1]  thanks                                                                                      
## [2]  thanks taking care mr happy customer                                                        
## [3]  still refund word dm please resolve issue cancelled flightled useless assistants trip       
## [4]  delayed due lack crew now delayed theres long line deicing still need improve service united
## [5]  thanks filled hows luck common
```

### Make a copy of the corpus


```r
tweets_corpus_copy <- tweets_corpus
tweets_corpus_jp <- tweets_corpus
```
### converting corpus to dataframe 

```r
attributes(tweets_corpus_jp)
```

```
## $names
## [1] "content" "meta"    "dmeta"  
## 
## $class
## [1] "SimpleCorpus" "Corpus"
```

```r
united_df <-data.frame(text=unlist(sapply(tweets_corpus, `[`)), stringsAsFactors=F)


united_df$tweet_id <- twitter_united$tweet_id
head(united_df)
```

```
##                                                                                            text
## 1                                                                                        thanks
## 2                                                          thanks taking care mr happy customer
## 3         still refund word dm please resolve issue cancelled flightled useless assistants trip
## 4  delayed due lack crew now delayed theres long line deicing still need improve service united
## 5                                                                thanks filled hows luck common
## 6                                     eriord express connections hugely popular now eriewr hop 
##      tweet_id
## 1 5.70308e+17
## 2 5.70308e+17
## 3 5.70307e+17
## 4 5.70307e+17
## 5 5.70307e+17
## 6 5.70306e+17
```


### Create Term Document Martix

We convert the word corpus into a document matrix. The Document matrix can be 
analyzed to examine most frequently occurring words. 

```r
tweet_tdm <- TermDocumentMatrix(tweets_corpus,
                                control = list(wordLengths = c(1,Inf)))
tweet_tdm
```

```
## <<TermDocumentMatrix (terms: 6084, documents: 3822)>>
## Non-/sparse entries: 33376/23219672
## Sparsity           : 100%
## Maximal term length: 31
## Weighting          : term frequency (tf)
```


### Word Frequencies

We find the most frequent words and we create a Word Cloud of tweets using
We are limiting the maximum words to 100 and plotting the top 10 frequent words
using the ggplot package.

```r
# Frequent Terms
freq_terms <- findFreqTerms(tweet_tdm)
term_freq <- sort(rowSums(as.matrix(tweet_tdm)),decreasing = TRUE)
freqterms_df <- data.frame(term = names(term_freq), freq = term_freq)


# Creating a word cloud of frequent term
wordcloud(words = freqterms_df$term, freq = freqterms_df$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
```

![](Airline_Sentiment_United_files/figure-html/Word Frequency-1.png)<!-- -->

```r
# Plotting the top 10 frequent words



freqterms_df$rank <- rank(-freqterms_df$freq,ties.method="min")
freqterms_df <- freqterms_df[order(freqterms_df$rank,decreasing = F),]
head(freqterms_df,10)
```

```
##              term freq rank
## united     united  370    1
## get           get  327    2
## thanks     thanks  274    3
## service   service  274    3
## now           now  271    5
## just         just  258    6
## time         time  227    7
## ua             ua  221    8
## customer customer  218    9
## bag           bag  200   10
```

```r
ggplot(head(freqterms_df,10), aes(x=term, y=rank)) + geom_bar(stat="identity") +
xlab("Terms") + ylab("Count") 
```

![](Airline_Sentiment_United_files/figure-html/Word Frequency-2.png)<!-- -->


#### Plot the frequency of the words on log scale .

Plotting the frequency of top 50 words in the logarithmic scale. 

```r
# Word frequency on log scale

freq_terms20 <- head(freqterms_df,20)
ggplot(freq_terms20, aes(rank, log(freq))) + geom_point() +
geom_text_repel(label = rownames(freq_terms20)) +  geom_smooth(method="lm")   
```

![](Airline_Sentiment_United_files/figure-html/Log of frequency plot -1.png)<!-- -->

```r
  #theme(axis.text.x=element_text(angle=45,hjust=1)) 
```

#### Plotting Bigrams for word frequency

The initial exploration of the word analysis was helpful and we will construct 
bigrams and trigrams and plot the top 15 bigram and trigram on a logarithmic 
scale.Bigrams are two word phrases and  trigrams are three word phrases. 
Recall that stop words had been removed so the phrases may look choppy. 

```r
#Bigram 
bigram_df <- freqterms_df %>%
  unnest_tokens(bigram, term , token = "ngrams", n = 2)

bigram_df$rank <- rank(-bigram_df$freq,ties.method="min")
bigram_df <- bigram_df[order(bigram_df$rank,decreasing = F),]

bigram_df15 <- head(bigram_df,15)
head(bigram_df15,15)
```

```
## # A tibble: 15 x 3
##     freq  rank          bigram
##    <dbl> <int>           <chr>
##  1   274     1  thanks service
##  2   125     2       like hour
##  3   100     3      make delay
##  4    91     4         dm lost
##  5    89     5     flightled u
##  6    87     6        way seat
##  7    87     6       seat wait
##  8    85     8      first know
##  9    84     9         yes fly
## 10    82    10    going people
## 11    81    11    worst really
## 12    78    12    crew luggage
## 13    78    12 luggage baggage
## 14    78    12    baggage good
## 15    77    15    agent ticket
```

```r
bigram_df15 <- bigram_df15[c("bigram","freq","rank")]
#Bigram Plot
ggplot(bigram_df15,  aes(reorder(bigram,freq), log10(freq))) +
 geom_bar(stat = "identity") + coord_flip() +
 xlab("Bigrams") + ylab("Frequency") + ggtitle("Most frequent bigrams")
```

![](Airline_Sentiment_United_files/figure-html/Bigrams-1.png)<!-- -->

```r
#Bigram Plot ranking vs frequency on log scale

ggplot(bigram_df15, aes(rank, log(freq))) + geom_point() +
geom_text_repel(label = (bigram_df15$bigram)) + geom_smooth(method="lm")
```

![](Airline_Sentiment_United_files/figure-html/Bigrams-2.png)<!-- -->

####Plotting Bigrams / Trigrams for word frequency


```r
#Trigram
trigram_df <- freqterms_df %>%  unnest_tokens(trigram, term , token = "ngrams", n = 3)
trigram_df <- trigram_df %>% arrange(desc(freq))

trigram_df15 <- head(trigram_df,15)
head(trigram_df15,15)
```

```
## # A tibble: 15 x 3
##     freq  rank              trigram
##    <dbl> <int>                <chr>
##  1    87    43        way seat wait
##  2    78    55 crew luggage baggage
##  3    78    55 luggage baggage good
##  4    77    59   agent ticket seats
##  5    77    59   ticket seats check
##  6    75    64      due even trying
##  7    75    64    even trying staff
##  8    75    64    trying staff ever
##  9    75    64       staff ever min
## 10    73    71    sent another take
## 11    73    71     another take hrs
## 12    68    78      days great want
## 13    64    85        let call made
## 14    64    85   call made problems
## 15    64    85    made problems ive
```

```r
# Trigram Plot 
ggplot(trigram_df15, aes(reorder(trigram,freq), log10(freq))) +
  geom_bar(stat = "identity") + coord_flip() +
  xlab("Trigrams") + ylab("Frequency") +
  ggtitle("Most frequent Trigram")
```

![](Airline_Sentiment_United_files/figure-html/Trigrams-1.png)<!-- -->

```r
#Trigram Plot ranking vs frequency on log scale

ggplot(trigram_df15, aes(rank, log(freq))) + geom_point() +
geom_text_repel(label = (trigram_df15$trigram)) + geom_smooth(method="lm")
```

![](Airline_Sentiment_United_files/figure-html/Trigrams-2.png)<!-- -->



### Sentiments
Let us know look at the sentiments of the tweets for united airlines. It helps in 
identifying the positive, negative and nuetral sentiment of the tweets.  

### Retrieve Data for united airline

```r
united <- united_df
united <- droplevels(united)


united_txt  <- united$text

united_sentiment <- sentiment(united_txt)

# map values 
united_sentiment$score <- NA
united_sentiment$score[united_sentiment$polarity == "positive"] <- 1
united_sentiment$score[united_sentiment$polarity == "negative"] <- -1
#united_table <- table(united_sentiment$polarity)
ggplot(united_sentiment, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +geom_text(stat='count',
  aes(label=..count..),vjust=-0.2)+
  scale_fill_brewer(palette="Dark2") +
 labs(x="Polarity", y="Number of Tweets") +
  ggtitle("Twitter Sentiment Analysis of united Airlines")+
  theme(plot.title = element_text(hjust = 0.5))
```

![](Airline_Sentiment_United_files/figure-html/united Sentiment-1.png)<!-- -->

```r
united_sentiment$tweet_id <- united$tweet_id

united_sentiment$airline <- 'united'
united_sentiment$code <- 'UN'

united_sentiment$freq <- str_count(united_sentiment$text, '\\s+')+1
colnames(united_sentiment) <- paste("sentiment", colnames(united_sentiment), sep = "_")
united_sentiment$tweet_id <- united$tweet_id
united_sentiment$text <- united$text

head(united_sentiment)
```

```
##                                                                                   sentiment_text
## 1                                                                                         thanks
## 2                                                           thanks taking care mr happy customer
## 3          still refund word dm please resolve issue cancelled flightled useless assistants trip
## 4   delayed due lack crew now delayed theres long line deicing still need improve service united
## 5                                                                 thanks filled hows luck common
## 6                                      eriord express connections hugely popular now eriewr hop 
##   sentiment_polarity sentiment_language sentiment_score sentiment_tweet_id
## 1            neutral                 en              NA        5.70308e+17
## 2           positive                 en               1        5.70308e+17
## 3            neutral                 en              NA        5.70307e+17
## 4           negative                 en              -1        5.70307e+17
## 5           positive                 en               1        5.70307e+17
## 6            neutral                 en              NA        5.70306e+17
##   sentiment_airline sentiment_code sentiment_freq    tweet_id
## 1            united             UN              2 5.70308e+17
## 2            united             UN              7 5.70308e+17
## 3            united             UN             13 5.70307e+17
## 4            united             UN             16 5.70307e+17
## 5            united             UN              6 5.70307e+17
## 6            united             UN             10 5.70306e+17
##                                                                                            text
## 1                                                                                        thanks
## 2                                                          thanks taking care mr happy customer
## 3         still refund word dm please resolve issue cancelled flightled useless assistants trip
## 4  delayed due lack crew now delayed theres long line deicing still need improve service united
## 5                                                                thanks filled hows luck common
## 6                                     eriord express connections hugely popular now eriewr hop
```
 
united has most of the tweets that are nuetral. The negative tweets are
more than the positive tweets
### Create  Vcorpus

Creating and cleaning VCorpus to use unigrams, bigrams and trigrams for 
Document Term Matrix.


```r
dtm_corpus <- VCorpus(VectorSource(twitter_united$text))

# Remove Punctuations
dtm_corpus<- tm_map(dtm_corpus,removePunctuation)

#Remove URLs
removeURL <- function(x) {
  gsub("http[^[:space:]]*", "", x)
}
dtm_corpus<- tm_map(dtm_corpus,content_transformer(removeURL))

# Remove anything expect English and Space
remove_others <- function(x) {
  gsub("[^[:alpha:][:space:]]*","",x)
}
dtm_corpus<- tm_map(dtm_corpus,content_transformer(remove_others))

# Transform to lower case

dtm_corpus<- tm_map(dtm_corpus,content_transformer(tolower))

# Remove Stopwords. 
tweets_stopwords <- c(setdiff(stopwords('english'), c("r", "big","united","united","united","airways","airlines","flight","pilot",
 "virgin","US airways","southwest","a","the","is","and")),"use", "see", 
 "used", "via", "amp","the","a","aa","aaaand","i","a","the",
 "flight","airlines","flights","airway","will", "cant","and","is","can","im")
dtm_corpus<- tm_map(dtm_corpus,removeWords,tweets_stopwords)

# Remove WhiteSpace
dtm_corpus<- tm_map(dtm_corpus,stripWhitespace)
```

### Create Document Term Matrix
The  unigrams, bigrams and trigram of  document matrix can be 
analyzed to examine most frequently occurring words. 

#### Unigram of DTM 

The unigram are single word phase from the document term matrix is created 
and the sparse terms are removed. The tweet ID and the tweets are added to the 
dataframe created . The term are placed across the columns and 
their occurence across each tweet are indicated either 0 or 1. 


```r
# Creating DTM
unigram_dtm <- DocumentTermMatrix(dtm_corpus)

# Inspecting the unigram DTM
inspect(unigram_dtm[1:5,5:10])
```

```
## <<DocumentTermMatrix (documents: 5, terms: 6)>>
## Non-/sparse entries: 0/30
## Sparsity           : 100%
## Maximal term length: 11
## Weighting          : term frequency (tf)
## Sample             :
##     Terms
## Docs abbreve abc abcdef abcwtvd abigailedge able
##    1       0   0      0       0           0    0
##    2       0   0      0       0           0    0
##    3       0   0      0       0           0    0
##    4       0   0      0       0           0    0
##    5       0   0      0       0           0    0
```

```r
# Removing Sparseterms from the DTM 
unigram_sparse <- removeSparseTerms(unigram_dtm,0.995)
unigram_sparse
```

```
## <<DocumentTermMatrix (documents: 3822, terms: 330)>>
## Non-/sparse entries: 17642/1243618
## Sparsity           : 99%
## Maximal term length: 14
## Weighting          : term frequency (tf)
```

```r
unigram_df <- as.data.frame(as.matrix(unigram_sparse))
colnames(unigram_df) <- paste("unigram_df.", colnames(unigram_df), sep = "_")
colnames(unigram_df) <- make.names(colnames(unigram_df))

unigram_df$tweet_id <- united_df$tweet_id
unigram_df$text <- united_df$text
dim(unigram_df)
```

```
## [1] 3822  332
```

```r
#head(unigram_df,5)
#write.csv(unigram_df, file.path(data_dir,"united_Unigram.csv"))
```
#### Bigram of DTM 

The bigrams are two word phase from the document term matrix is created 
and the sparse terms are removed.


```r
#Tokenizing
BigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2))}
#BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))

bigram_dtm <- DocumentTermMatrix(dtm_corpus, 
                                 control=list(tokenize=BigramTokenizer))

# Inspecting the bigram DTM
inspect(bigram_dtm[1:5,5:10])
```

```
## <<DocumentTermMatrix (documents: 5, terms: 6)>>
## Non-/sparse entries: 0/30
## Sparsity           : 100%
## Maximal term length: 19
## Weighting          : term frequency (tf)
## Sample             :
##     Terms
## Docs abc direct abcdef whole abigailedge another able app able claim
##    1          0            0                   0        0          0
##    2          0            0                   0        0          0
##    3          0            0                   0        0          0
##    4          0            0                   0        0          0
##    5          0            0                   0        0          0
##     Terms
## Docs able contact
##    1            0
##    2            0
##    3            0
##    4            0
##    5            0
```

```r
# Removing Sparseterms from the DTM 
bigram_sparse <- removeSparseTerms(bigram_dtm,0.995)
bigram_sparse
```

```
## <<DocumentTermMatrix (documents: 3822, terms: 8)>>
## Non-/sparse entries: 437/30139
## Sparsity           : 99%
## Maximal term length: 19
## Weighting          : term frequency (tf)
```

```r
bigram_df <- as.data.frame(as.matrix(bigram_sparse))
colnames(bigram_df) <- paste("bigram_df.", colnames(bigram_df), sep = "_")
colnames(bigram_df) <- make.names(colnames(bigram_df))
bigram_df$tweet_id <- united_df$tweet_id
bigram_df$text <- united_df$text
dim(bigram_df)
```

```
## [1] 3822   10
```

```r
#head(bigram_df,5)
#write.csv(bigram_df, file.path(data_dir,"united_Bigram.csv"))
```


#### Trigram of DTM 

The trigrams are three word phase from the document term matrix is created 
and the sparse terms are removed.


```r
#Tokenizing
#TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))
TrigramTokenizer <- function(x)  {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3))}

trigram_dtm <- DocumentTermMatrix(dtm_corpus, 
                                  control=list(tokenize=TrigramTokenizer))

trigram_dtm
```

```
## <<DocumentTermMatrix (documents: 3822, terms: 26182)>>
## Non-/sparse entries: 26616/100040988
## Sparsity           : 100%
## Maximal term length: 64
## Weighting          : term frequency (tf)
```

```r
# Inspecting the trigram DTM
#inspect(trigram_dtm[1:5,5:10])
# Removing Sparseterms from the DTM 
trigram_sparse <- removeSparseTerms(trigram_dtm,0.999)
trigram_sparse
```

```
## <<DocumentTermMatrix (documents: 3822, terms: 24)>>
## Non-/sparse entries: 136/91592
## Sparsity           : 100%
## Maximal term length: 27
## Weighting          : term frequency (tf)
```

```r
trigram_df <- as.data.frame(as.matrix(trigram_sparse))
colnames(trigram_df) <- paste("trigram_df.", colnames(trigram_df), sep = "_")
colnames(trigram_df) <- make.names(colnames(trigram_df))
trigram_df$tweet_id <- united_df$tweet_id
trigram_df$text <- united_df$text
dim(trigram_df)
```

```
## [1] 3822   26
```

```r
#head(trigram_df,5)
#write.csv(trigram_df, file.path(data_dir,"united_Trigram.csv"))
```
### Preparing for the model

Combining the unigram, bigrams, trigrams and sentiment into one dataframe 
for modelling

```r
tweets_unibigram <- full_join(unigram_df, bigram_df, by = c("tweet_id", "text"))
 
tweets_unibitrigram <- full_join(tweets_unibigram,trigram_df, by = c("tweet_id","text"))

tweets_model <- full_join(tweets_unibitrigram,united_sentiment, by = c("tweet_id","text")) 
dim(tweets_model)
```

```
## [1] 3836  372
```

```r
#write.csv(tweets_model, file.path(data_dir,"Delete.csv"))
```
