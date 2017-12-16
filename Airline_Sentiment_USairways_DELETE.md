Airline Sentiment
================
2017-12-15

### Introduction

Analyzing the US based airlines performances by analyzing the the tweets of the airlines.Identifying the sentiments of tweets and classifying them as nuetral, negative and positive sentiment for each airlines. Identify the issues behind negative sentiments and checking the significance of bigrams, trigrams and airlines on the sentiment.

### Description of Data Set

The dataset contains important fields like tweet\_id,airline\_sentiment, airline, name, text, tweet\_created, tweet\_location which will be widely used in the sentiment analysis.

``` r
data_dir <- "~/Desktop/Springboard/Capstone Project/Capstone" 
twitter_airline <- read.csv(file.path(data_dir,"tweets.csv"),header = TRUE)
dim(twitter_airline)
```

    ## [1] 14640    15

``` r
colnames(twitter_airline)
```

    ##  [1] "tweet_id"                     "airline_sentiment"           
    ##  [3] "airline_sentiment_confidence" "negativereason"              
    ##  [5] "negativereason_confidence"    "airline"                     
    ##  [7] "airline_sentiment_gold"       "name"                        
    ##  [9] "negativereason_gold"          "retweet_count"               
    ## [11] "text"                         "tweet_coord"                 
    ## [13] "tweet_created"                "tweet_location"              
    ## [15] "user_timezone"

``` r
knitr::opts_chunk$set(echo = TRUE)
```

### Load the libraries

Loading the libraries required for sentiment analysis.

``` r
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
```

### Structure of the dataset

``` r
str(twitter_airline)
```

    ## 'data.frame':    14640 obs. of  15 variables:
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

### Summary of the dataset

``` r
summary(twitter_airline)
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

Dataset contains 14640 observations and 15 variables. There are some new variables that will be added to the dataset.

### Replace twitter handle with blank

The tweets contained the airlines twitter handle. We must first remove the twitter handle as they should not be used in the text analysis.

``` r
twitter_airline$text <- gsub("@VirginAmerica","",gsub("@AmericanAir","",
 gsub("@JetBlue ","",gsub("@SouthwestAir","",gsub("@united","",
 gsub("@USAirways","", twitter_airline$text))))))
```

### Subsetting USAirways tweets from the dataset

``` r
twitter_USAirways <- filter(twitter_airline, airline =="US Airways")
```

### Build and cleaning the corpus

Here we convert the text into a word corpus using the function VectorSource. A word corpus enables us to eliminate common words using the text mining package tm. Removing the corpus specific stopwords lets us focus on the important words.

``` r
tweets_corpus <- Corpus(VectorSource(twitter_USAirways$text))


# Inspect Corpus
inspect(tweets_corpus[1:5])
```

    ## <<SimpleCorpus>>
    ## Metadata:  corpus specific: 1, document level (indexed): 0
    ## Content:  documents: 5
    ## 
    ## [1]   is there a better time to call? My flight is on Friday and I need to change it. Worried I may be on hold until then.          
    ## [2]  and when will one of these agents be available to speak?                                                                       
    ## [3]  is a DM possible if you aren't following me?                                                                                   
    ## [4]  Fortunately you have staff like Lynn S. and DeeDee who actually understand customer service and simply being NICE.             
    ## [5]  just hung up on me again.  Another waste of an hour of my time.  How am I supposed to book a one way award flight?  #badwebsite

### Clean the corpus

``` r
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

    ## <<SimpleCorpus>>
    ## Metadata:  corpus specific: 1, document level (indexed): 0
    ## Content:  documents: 5
    ## 
    ## [1]   is there a better time to call My flight is on Friday and I need to change it Worried I may be on hold until then         
    ## [2]  and when will one of these agents be available to speak                                                                    
    ## [3]  is a DM possible if you arent following me                                                                                 
    ## [4]  Fortunately you have staff like Lynn S and DeeDee who actually understand customer service and simply being NICE           
    ## [5]  just hung up on me again  Another waste of an hour of my time  How am I supposed to book a one way award flight  badwebsite

### Convert the corpus to lowercase

``` r
tweets_corpus <- tm_map(tweets_corpus,content_transformer(tolower))

# Remove Stopwords. 
tweets_stopwords <- c(setdiff(stopwords('english'), c("r", "big","USAirways","USAirways","USAirways","airways","airlines","flight","pilot",
 "virgin","US airways","southwest","a","the","is","and")),"use", "see", 
 "used", "via", "amp","the","a","aa","aaaand","i","a","the",
 "flight","airlines","flights","airway","will", "cant","and","is","can","im")
tweets_corpus <- tm_map(tweets_corpus,removeWords,tweets_stopwords)
inspect(tweets_corpus[1:5])
```

    ## <<SimpleCorpus>>
    ## Metadata:  corpus specific: 1, document level (indexed): 0
    ## Content:  documents: 5
    ## 
    ## [1]      better time  call     friday   need  change  worried  may   hold                       
    ## [2]     one   agents  available  speak                                                          
    ## [3]    dm possible   arent following                                                            
    ## [4]  fortunately   staff like lynn s  deedee  actually understand customer service  simply  nice
    ## [5]  just hung      another waste   hour   time     supposed  book  one way award   badwebsite

### Remove extra whitespace

``` r
tweets_corpus <- tm_map(tweets_corpus,stripWhitespace)
inspect(tweets_corpus[1:5])
```

    ## <<SimpleCorpus>>
    ## Metadata:  corpus specific: 1, document level (indexed): 0
    ## Content:  documents: 5
    ## 
    ## [1]  better time call friday need change worried may hold                                 
    ## [2]  one agents available speak                                                           
    ## [3]  dm possible arent following                                                          
    ## [4]  fortunately staff like lynn s deedee actually understand customer service simply nice
    ## [5]  just hung another waste hour time supposed book one way award badwebsite

### Make a copy of the corpus

``` r
tweets_corpus_copy <- tweets_corpus
tweets_corpus_jp <- tweets_corpus
```

### converting corpus to dataframe

``` r
attributes(tweets_corpus_jp)
```

    ## $names
    ## [1] "content" "meta"    "dmeta"  
    ## 
    ## $class
    ## [1] "SimpleCorpus" "Corpus"

``` r
USAirways_df <-data.frame(text=unlist(sapply(tweets_corpus, `[`)), stringsAsFactors=F)

USAirways_df$tweet_id <- twitter_USAirways$tweet_id
head(USAirways_df)
```

    ##                                                                                              text
    ## 1                                           better time call friday need change worried may hold 
    ## 2                                                                      one agents available speak
    ## 3                                                                    dm possible arent following 
    ## 4           fortunately staff like lynn s deedee actually understand customer service simply nice
    ## 5                        just hung another waste hour time supposed book one way award badwebsite
    ## 6  never received horrible service treated poorly richard p today excuse attitude ripping tickets
    ##      tweet_id
    ## 1 5.70311e+17
    ## 2 5.70310e+17
    ## 3 5.70309e+17
    ## 4 5.70309e+17
    ## 5 5.70309e+17
    ## 6 5.70308e+17

### Create Term Document Martix

We convert the word corpus into a document matrix. The Document matrix can be analyzed to examine most frequently occurring words.

``` r
tweet_tdm <- TermDocumentMatrix(tweets_corpus_jp,
                                control = list(wordLengths = c(1,Inf)))
tweet_tdm
```

    ## <<TermDocumentMatrix (terms: 4785, documents: 2913)>>
    ## Non-/sparse entries: 25922/13912783
    ## Sparsity           : 100%
    ## Maximal term length: 40
    ## Weighting          : term frequency (tf)

### Create Document Term Martix

We convert the word corpus into a document matrix. The Document matrix can be analyzed to examine most frequently occurring words.

``` r
tweet_dtm <- DocumentTermMatrix(tweets_corpus_jp)
tweet_dtm
```

    ## <<DocumentTermMatrix (documents: 2913, terms: 4655)>>
    ## Non-/sparse entries: 24941/13535074
    ## Sparsity           : 100%
    ## Maximal term length: 40
    ## Weighting          : term frequency (tf)

``` r
inspect(tweet_dtm[1:5,5:10])
```

    ## <<DocumentTermMatrix (documents: 5, terms: 6)>>
    ## Non-/sparse entries: 7/23
    ## Sparsity           : 77%
    ## Maximal term length: 7
    ## Weighting          : term frequency (tf)
    ## Sample             :
    ##     Terms
    ## Docs agents hold may need time worried
    ##    1      0    1   1    1    1       1
    ##    2      1    0   0    0    0       0
    ##    3      0    0   0    0    0       0
    ##    4      0    0   0    0    0       0
    ##    5      0    0   0    0    1       0

``` r
 freqterm_dtm <- findFreqTerms(tweet_dtm,lowfreq = 20)

sparse <- removeSparseTerms(tweet_dtm,0.995)
sparse
```

    ## <<DocumentTermMatrix (documents: 2913, terms: 339)>>
    ## Non-/sparse entries: 15008/972499
    ## Sparsity           : 98%
    ## Maximal term length: 13
    ## Weighting          : term frequency (tf)

``` r
tweetsSparse <- as.data.frame(as.matrix(sparse))
colnames(tweetsSparse) <- make.names(colnames(tweetsSparse))
tweetsSparse$tweet_id <- USAirways_df$tweet_id
tweetsSparse$text <- USAirways_df$text
tweetsSparse$text1 <- USAirways_df$text
tweetsSparse$text2 <- USAirways_df$text


tweetsSparse <- tweetsSparse %>%
  unnest_tokens(bigram, text , token = "ngrams", n = 2)

tweetsSparse <- tweetsSparse %>%
  unnest_tokens(trigram, text1 , token = "ngrams", n = 3)

write.csv(tweetsSparse, file.path(data_dir,"USAirways.csv"))
```

### Word Frequencies

We find the most frequent words and we create a Word Cloud of tweets using We are limiting the maximum words to 100 and plotting the top 10 frequent words using the ggplot package.

``` r
# Frequent Terms
freq_terms <- findFreqTerms(tweet_tdm)

term_freq <- sort(rowSums(as.matrix(tweet_tdm)),decreasing = TRUE)
freqterms_df <- data.frame(term = names(term_freq), freq = term_freq)



# Creating a word cloud of frequent term
wordcloud(words = freqterms_df$term, freq = freqterms_df$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
```

![](Airline_Sentiment_USairways_DELETE_files/figure-markdown_github-ascii_identifiers/Word%20Frequency-1.png)

``` r
# Plotting the top 10 frequent words
freqterms_df$rank <- rank(-freqterms_df$freq,ties.method="min")
freqterms_df <- freqterms_df[order(freqterms_df$rank,decreasing = F),]
head(freqterms_df,10)
```

    ##                term freq rank
    ## get             get  312    1
    ## hold           hold  277    2
    ## us               us  249    3
    ## service     service  242    4
    ## now             now  241    5
    ## help           help  215    6
    ## cancelled cancelled  214    7
    ## hours         hours  206    8
    ## just           just  182    9
    ## customer   customer  180   10

``` r
ggplot(head(freqterms_df,10), aes(x=term, y=rank)) + geom_bar(stat="identity") +
xlab("Terms") + ylab("Count") 
```

![](Airline_Sentiment_USairways_DELETE_files/figure-markdown_github-ascii_identifiers/Word%20Frequency-2.png)

#### Plot the frequency of the words on log scale .

Plotting the frequency of top 50 words in the logarithmic scale.

``` r
# Word frequency on log scale

freq_terms20 <- head(freqterms_df,20)
ggplot(freq_terms20, aes(rank, log(freq))) + geom_point() +geom_smooth(method = "lm") +
geom_text_repel(label = rownames(freq_terms20))   
```

![](Airline_Sentiment_USairways_DELETE_files/figure-markdown_github-ascii_identifiers/Log%20of%20frequency%20plot%20-1.png)

``` r
  #theme(axis.text.x=element_text(angle=45,hjust=1)) 
```

#### Plotting Bigrams for word frequency

The initial exploration of the word analysis was helpful and we will construct bigrams and trigrams and plot the top 15 bigram and trigram on a logarithmic scale.Bigrams are two word phrases and trigrams are three word phrases. Recall that stop words had been removed so the phrases may look choppy.

``` r
#Bigram 
bigram_df <- freqterms_df %>%
  unnest_tokens(bigram, term , token = "ngrams", n = 2)

bigram_df$rank <- rank(-bigram_df$freq,ties.method="min")
bigram_df <- bigram_df[order(bigram_df$rank,decreasing = F),]

bigram_df15 <- head(bigram_df,15)
head(bigram_df15,15)
```

    ## # A tibble: 15 x 3
    ##     freq  rank         bigram
    ##    <dbl> <int>          <chr>
    ##  1   111     1 late flightled
    ##  2   100     2      thank ive
    ##  3    97     3     trying bag
    ##  4    80     4       clt home
    ##  5    80     4       home phl
    ##  6    79     6   change miles
    ##  7    79     6        miles u
    ##  8    75     8   told luggage
    ##  9    72     9      wait make
    ## 10    72     9     make worst
    ## 11    71    11   another guys
    ## 12    70    12        way due
    ## 13    70    12        due min
    ## 14    67    14  going airport
    ## 15    64    15    know travel

``` r
bigram_df15 <- bigram_df15[c("bigram","freq","rank")]
#Bigram Plot
ggplot(bigram_df15,  aes(reorder(bigram,freq), log(freq))) +
 geom_bar(stat = "identity") + coord_flip() +
 xlab("Bigrams") + ylab("Frequency") + ggtitle("Most frequent bigrams")
```

![](Airline_Sentiment_USairways_DELETE_files/figure-markdown_github-ascii_identifiers/Bigrams-1.png)

``` r
#Bigram Plot ranking vs frequency on log scale

ggplot(bigram_df15, aes(rank, log(freq))) + geom_point()  + geom_smooth(method = "lm") +
geom_text_repel(label = (bigram_df15$bigram)) 
```

![](Airline_Sentiment_USairways_DELETE_files/figure-markdown_github-ascii_identifiers/Bigrams-2.png)

#### Plotting Bigrams / Trigrams for word frequency

``` r
#Trigram
trigram_df <- freqterms_df %>%  unnest_tokens(trigram, term , token = "ngrams", n = 3)
trigram_df <- trigram_df %>% arrange(desc(freq))

trigram_df15 <- head(trigram_df,15)
head(trigram_df15,15)
```

    ## # A tibble: 15 x 3
    ##     freq  rank                   trigram
    ##    <dbl> <int>                     <chr>
    ##  1    80    40              clt home phl
    ##  2    79    43            change miles u
    ##  3    72    51           wait make worst
    ##  4    70    56               way due min
    ##  5    64    64       know travel sitting
    ##  6    60    71          number days bags
    ##  7    57    76          ticket agent dca
    ##  8    57    76            agent dca ever
    ##  9    54    84       flightr flying good
    ## 10    54    84          flying good lost
    ## 11    54    84          good lost really
    ## 12    49    93     flt connection online
    ## 13    49    93 connection online airways
    ## 14    49    93      online airways didnt
    ## 15    49    93        airways didnt mins

``` r
# Trigram Plot 
ggplot(trigram_df15, aes(reorder(trigram,freq), log(freq))) +
  geom_bar(stat = "identity") + coord_flip() +
  xlab("Trigrams") + ylab("Frequency") +
  ggtitle("Most frequent Trigram")
```

![](Airline_Sentiment_USairways_DELETE_files/figure-markdown_github-ascii_identifiers/Trigrams-1.png)

``` r
#Trigram Plot ranking vs frequency on log scale

ggplot(trigram_df15, aes(rank, log(freq))) + geom_point() + geom_smooth(method = "lm")+
geom_text_repel(label = (trigram_df15$trigram)) 
```

![](Airline_Sentiment_USairways_DELETE_files/figure-markdown_github-ascii_identifiers/Trigrams-2.png)

### Sentiments

Let us know look at the sentiments of the tweets for USAirways airlines. It helps in identifying the positive, negative and nuetral sentiment of the tweets.

### Retrieve Data for USAirways airline

``` r
USAirways <- USAirways_df
USAirways <- droplevels(USAirways)


USAirways_txt  <- USAirways$text

USAirways_sentiment <- sentiment(USAirways_txt)

# map values 
USAirways_sentiment$score <- NA
USAirways_sentiment$score[USAirways_sentiment$polarity == "positive"] <- 1
USAirways_sentiment$score[USAirways_sentiment$polarity == "negative"] <- -1
#USAirways_table <- table(USAirways_sentiment$polarity)
ggplot(USAirways_sentiment, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +geom_text(stat='count',
  aes(label=..count..),vjust=-0.2)+
  scale_fill_brewer(palette="Dark2") +
 labs(x="Polarity", y="Number of Tweets") +
  ggtitle("Twitter Sentiment Analysis of USAirways Airlines")+
  theme(plot.title = element_text(hjust = 0.5))
```

![](Airline_Sentiment_USairways_DELETE_files/figure-markdown_github-ascii_identifiers/USAirways%20Sentiment-1.png)

``` r
USAirways_sentiment$tweet_id <- USAirways$tweet_id


USAirways_sentiment$airline <- 'USAirways'
USAirways_sentiment$code <- 'UA'
USAirways_sentiment$freq <- str_count(USAirways_sentiment$text, '\\s+')+1
head(USAirways_sentiment)
```

    ##                                                                                               text
    ## 1                                            better time call friday need change worried may hold 
    ## 2                                                                       one agents available speak
    ## 3                                                                     dm possible arent following 
    ## 4            fortunately staff like lynn s deedee actually understand customer service simply nice
    ## 5                         just hung another waste hour time supposed book one way award badwebsite
    ## 6   never received horrible service treated poorly richard p today excuse attitude ripping tickets
    ##   polarity language score    tweet_id   airline code freq
    ## 1  neutral       en    NA 5.70311e+17 USAirways   UA   11
    ## 2  neutral       en    NA 5.70310e+17 USAirways   UA    5
    ## 3  neutral       en    NA 5.70309e+17 USAirways   UA    6
    ## 4 positive       en     1 5.70309e+17 USAirways   UA   13
    ## 5  neutral       en    NA 5.70309e+17 USAirways   UA   13
    ## 6 negative       en    -1 5.70308e+17 USAirways   UA   14

USAirways has most of the tweets that are nuetral. The negative tweets are more than the positive tweets
