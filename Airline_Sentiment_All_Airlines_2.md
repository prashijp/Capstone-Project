All\_airlines Sentiment
================
2018-07-14

### Introduction

Analyzing the all\_airlines performances by analyzing the the tweets. Identifying the sentiments of tweets and classifying them as nuetral, negative and positive sentiment for each airlines. Identify the issues behind negative sentiments and checking the significance of bigrams, trigrams and airlines on the sentiment.

### Description of Data Set

The dataset contains important fields like tweet\_id,airline\_sentiment, airline, name, text, tweet\_created, tweet\_location which will be widely used in the sentiment analysis.

``` r
data_dir <- "~/Desktop/Springboard/Capstone Project/Capstone" 
twitter_airline <- read.csv(file.path(data_dir,"Tweets.csv"),header = TRUE)
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
library(slam)
library(caret)
library(e1071)
library(ROCR)
library(pROC)
library(ROSE)
library(DMwR)
library(precrec)
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

### Build and cleaning the corpus

Here we convert the text into a word corpus using the function VectorSource. A word corpus enables us to eliminate common words using the text mining package tm. Removing the corpus specific stopwords lets us focus on the important words.

``` r
tweets_corpus <- Corpus(VectorSource(twitter_airline$text))


# Inspect Corpus
inspect(tweets_corpus[1:5])
```

    ## <<SimpleCorpus>>
    ## Metadata:  corpus specific: 1, document level (indexed): 0
    ## Content:  documents: 5
    ## 
    ## [1]  What @dhepburn said.                                                                                           
    ## [2]  plus you've added commercials to the experience... tacky.                                                      
    ## [3]  I didn't today... Must mean I need to take another trip!                                                       
    ## [4]  it's really aggressive to blast obnoxious "entertainment" in your guests' faces &amp; they have little recourse
    ## [5]  and it's a really big bad thing about it

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
    ## [1]  What dhepburn said                                                                                       
    ## [2]  plus youve added commercials to the experience tacky                                                     
    ## [3]  I didnt today Must mean I need to take another trip                                                      
    ## [4]  its really aggressive to blast obnoxious entertainment in your guests faces amp they have little recourse
    ## [5]  and its a really big bad thing about it

### Convert the corpus to lowercase

``` r
tweets_corpus <- tm_map(tweets_corpus,content_transformer(tolower))

# Remove Stopwords. 
tweets_stopwords <- c(setdiff(stopwords('english'), c("r", "big",
"Virgin America","Virgin America","american","airways","airlines","flight",
 "virgin","all_airlines","southwest","a","the","is","and")),"use", "see", 
 "used", "via", "amp","the","a","aa","aaaand","i","a","the",
 "flight","airlines","flights","airway","will", "cant","and","is","can","im",
 "jetblue","pilot")
tweets_corpus <- tm_map(tweets_corpus,removeWords,tweets_stopwords)
inspect(tweets_corpus[1:5])
```

    ## <<SimpleCorpus>>
    ## Metadata:  corpus specific: 1, document level (indexed): 0
    ## Content:  documents: 5
    ## 
    ## [1]   dhepburn said                                                                     
    ## [2]  plus youve added commercials   experience tacky                                    
    ## [3]   didnt today must mean  need  take another trip                                    
    ## [4]   really aggressive  blast obnoxious entertainment   guests faces    little recourse
    ## [5]     really big bad thing

### Remove extra whitespace

``` r
tweets_corpus <- tm_map(tweets_corpus,stripWhitespace)
inspect(tweets_corpus[1:5])
```

    ## <<SimpleCorpus>>
    ## Metadata:  corpus specific: 1, document level (indexed): 0
    ## Content:  documents: 5
    ## 
    ## [1]  dhepburn said                                                               
    ## [2]  plus youve added commercials experience tacky                               
    ## [3]  didnt today must mean need take another trip                                
    ## [4]  really aggressive blast obnoxious entertainment guests faces little recourse
    ## [5]  really big bad thing

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
all_airlines_df <-data.frame(text=unlist(sapply(tweets_corpus, `[`)), 
                             stringsAsFactors=F)


all_airlines_df$tweet_id <- twitter_airline$tweet_id
head(all_airlines_df)
```

    ##                                                                            text
    ## 1                                                                 dhepburn said
    ## 2                                 plus youve added commercials experience tacky
    ## 3                                  didnt today must mean need take another trip
    ## 4  really aggressive blast obnoxious entertainment guests faces little recourse
    ## 5                                                         really big bad thing 
    ## 6                  seriously pay seats didnt playing really bad thing flying va
    ##      tweet_id
    ## 1 5.70306e+17
    ## 2 5.70301e+17
    ## 3 5.70301e+17
    ## 4 5.70301e+17
    ## 5 5.70301e+17
    ## 6 5.70301e+17

### Create Term Document Martix

We convert the word corpus into a document matrix. The Document matrix can be analyzed to examine most frequently occurring words.

``` r
tweet_tdm <- TermDocumentMatrix(tweets_corpus,
                                control = list(wordLengths = c(1,Inf)))
tweet_tdm
```

    ## <<TermDocumentMatrix (terms: 13245, documents: 14640)>>
    ## Non-/sparse entries: 122648/193784152
    ## Sparsity           : 100%
    ## Maximal term length: 46
    ## Weighting          : term frequency (tf)

### Word Frequencies

We find the most frequent words and we create a Word Cloud of tweets using We are limiting the maximum words to 100 and plotting the top 10 frequent words using the ggplot package.

``` r
# Frequent Terms
freq_terms <- findFreqTerms(tweet_tdm)
term_freq <- sort(rowSums(as.matrix(tweet_tdm)),decreasing = TRUE)
freqterms_df <- data.frame(term = names(term_freq), freq = term_freq)

head(freqterms_df)
```

    ##                term freq
    ## get             get 1336
    ## thanks       thanks 1072
    ## cancelled cancelled 1056
    ## now             now 1023
    ## just           just  966
    ## service     service  956

``` r
# Creating a word cloud of frequent term
wordcloud(words = freqterms_df$term, freq = freqterms_df$freq, min.freq = 1,
          max.words=75, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/Word%20Frequency-1.png)

``` r
# Plotting the top 10 frequent words



freqterms_df$rank <- rank(-freqterms_df$freq,ties.method="min")
#dplyr sort 
freqterms_df <- freqterms_df[order(freqterms_df$rank,decreasing = F),]
head(freqterms_df,10)
```

    ##                term freq rank
    ## get             get 1336    1
    ## thanks       thanks 1072    2
    ## cancelled cancelled 1056    3
    ## now             now 1023    4
    ## just           just  966    5
    ## service     service  956    6
    ## help           help  855    7
    ## time           time  770    8
    ## customer   customer  746    9
    ## us               us  706   10

``` r
ggplot(head(freqterms_df,10), aes(x=term, y=rank)) + geom_bar(stat="identity") +
xlab("Terms") + ylab("Count") 
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/Word%20Frequency-2.png)

#### Plot the frequency of the words on log scale .

Plotting the frequency of top 50 words in the logarithmic scale.

``` r
# Word frequency on log scale


freq_terms20 <- head(freqterms_df,20)
ggplot(freq_terms20, aes(rank, log(freq))) + geom_point() +
geom_text_repel(label = rownames(freq_terms20)) +  geom_smooth(method="lm")
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/Log%20of%20frequency%20plot%20-1.png)

#### Plotting Bigrams for word frequency

The initial exploration of the word analysis was helpful and we will construct bigrams and plot the top 15 bigrams on a logarithmic scale.Bigrams are two word phrases.Recall that stop words had been removed so the phrases may look choppy.

``` r
#Bigram 
bigram_df <- freqterms_df %>%
  unnest_tokens(bigram, term , token = "ngrams", n = 2)

bigram_df$rank <- rank(-bigram_df$freq,ties.method="min")
bigram_df <- bigram_df[order(bigram_df$rank,decreasing = F),]

bigram_df <- na.omit(bigram_df)

bigram_df15 <- head(bigram_df,15)
head(bigram_df15,15)
```

    ##       freq rank           bigram
    ## 13055  312   48      flying make
    ## 13053  308   50           go hrs
    ## 13046  294   57     minutes home
    ## 13034  260   69    luggage agent
    ## 13033  249   70        ticket dm
    ## 13032  245   71     ever getting
    ## 13028  239   74        love lost
    ## 13029  239   74       lost worst
    ## 13027  238   76        yes email
    ## 13016  223   86 baggage flighted
    ## 13017  223   86      flighted ua
    ## 12996  183  107    wont boarding
    ## 12995  181  108      better long
    ## 12994  179  109   already doesnt
    ## 12992  178  110         said ill

``` r
bigram_df15 <- bigram_df15[c("bigram","freq","rank")]
#Bigram Plot
ggplot(bigram_df15,  aes(reorder(bigram,freq), log(freq))) +
 geom_bar(stat = "identity") + coord_flip() +
 xlab("Bigrams") + ylab("Frequency") + ggtitle("Most frequent bigrams")
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/Bigrams-1.png)

``` r
#Bigram Plot ranking vs frequency on log scale

ggplot(bigram_df15, aes(rank, log(freq))) + geom_point() +
geom_text_repel(label = (bigram_df15$bigram)) + geom_smooth(method="lm")
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/Bigrams-2.png)

#### Plotting Trigrams for word frequency

We will construct trigrams and plot the top 15 trigrams on a logarithmic scale. Trigrams are three word phrases

``` r
#Trigram
trigram_df <- freqterms_df %>%  unnest_tokens(trigram, term , 
                                              token = "ngrams", n = 3)
trigram_df <- trigram_df %>% arrange(desc(freq))

trigram_df <- na.omit(trigram_df)

trigram_df15 <- head(trigram_df,15)
head(trigram_df15,15)
```

    ##     freq rank              trigram
    ## 74   239   80      love lost worst
    ## 85   223   94  baggage flighted ua
    ## 108  178  122        said ill sure
    ## 109  177  125        left jfk line
    ## 119  157  141  tonight whats miles
    ## 120  156  144   think website find
    ## 122  152  148      yet nothing flt
    ## 123  152  148    nothing flt fleek
    ## 129  145  159    dfw system fleets
    ## 132  142  166      tried hotel put
    ## 137  135  174  pay called anything
    ## 138  135  174 called anything rude
    ## 140  131  179     missed free nice
    ## 141  131  179       free nice done
    ## 142  129  183   ago tickets follow

``` r
# Trigram Plot 
ggplot(trigram_df15, aes(reorder(trigram,freq), log(freq))) +
  geom_bar(stat = "identity") + coord_flip() +
  xlab("Trigrams") + ylab("Frequency") +
  ggtitle("Most frequent Trigram")
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/Trigrams-1.png)

``` r
#Trigram Plot ranking vs frequency on log scale

ggplot(trigram_df15, aes(rank, log(freq))) + geom_point() +
geom_text_repel(label = (trigram_df15$trigram)) + geom_smooth(method="lm")
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/Trigrams-2.png)

### Sentiments

Let us know look at the sentiments of the tweets for all\_airlines. It helps in identifying the positive, negative and nuetral sentiment of the tweets.

### Retrieve Data for all\_airlines

``` r
all_airlines <- all_airlines_df
all_airlines <- droplevels(all_airlines)


all_airlines_txt  <- all_airlines$text

all_airlines_sentiment <- sentiment(all_airlines_txt)
all_airlines_sentiment$airline <- twitter_airline$airline

# map values 
all_airlines_sentiment$score <- NA
all_airlines_sentiment$score[all_airlines_sentiment$polarity=="positive"]<- 1
all_airlines_sentiment$score[all_airlines_sentiment$polarity=="negative"]<- -1

ggplot(all_airlines_sentiment, aes(x=airline)) +
  geom_bar(aes(y=..count.., fill=polarity)) +geom_text(stat='count',
  aes(label=..count..),vjust=-0.2)+
  scale_fill_brewer(palette="Dark2") +
 labs(x="Polarity", y="Number of Tweets") +
  ggtitle("Twitter Sentiment Analysis of all_airlines Airlines")+
  theme(plot.title = element_text(hjust = 0.5))
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/all_airlines%20Sentiment-1.png)

``` r
all_airlines_sentiment$tweet_id <- all_airlines$tweet_id

all_airlines_sentiment$freq <- str_count(all_airlines_sentiment$text, '\\s+')+1
colnames(all_airlines_sentiment) <- paste("sentiment", colnames(all_airlines_sentiment), sep = "_")
all_airlines_sentiment$tweet_id <- twitter_airline$tweet_id
all_airlines_sentiment$text <- twitter_airline$text

all_airlines_sentiment$airline <- twitter_airline$airline
all_airlines_sentiment$code <- 'All'
all_airlines_sentiment$negativereason <- twitter_airline$negativereason
all_airlines_sentiment$location <- twitter_airline$tweet_location
all_airlines_sentiment$timezone <- twitter_airline$user_timezone

all_airlines_sentiment <- all_airlines_sentiment[,c(6:8,1:5,9:14)]


colnames(all_airlines_sentiment)
```

    ##  [1] "sentiment_tweet_id" "sentiment_freq"     "tweet_id"          
    ##  [4] "sentiment_text"     "sentiment_polarity" "sentiment_language"
    ##  [7] "sentiment_airline"  "sentiment_score"    "text"              
    ## [10] "airline"            "code"               "negativereason"    
    ## [13] "location"           "timezone"

All the airlines anf their respective sentiments has been analyzed and plotted. Most of the tweets are nuetral. The negative tweets are more than the positive tweets and we will go ahead and look at the significance of sentiments on unigrams,bigrams and trigrams.

### Create Vcorpus

Creating and cleaning VCorpus to use unigrams, bigrams and trigrams for Document Term Matrix.

``` r
dtm_corpus <- VCorpus(VectorSource(twitter_airline$text))

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
tweets_stopwords <- c(setdiff(stopwords('english'), c("r", "big","Virgin America",
 "jetblue","Virgin America","airways","airlines","flight","pilot",
 "virgin","all_airlines","southwest","a","the","is","and")),"use", "see", 
 "used", "via", "amp","the","a","aa","aaaand","i","a","the",
 "flight","airlines","flights","airway","will", "cant","and","is","can","im")
dtm_corpus<- tm_map(dtm_corpus,removeWords,tweets_stopwords)

# Remove WhiteSpace
dtm_corpus<- tm_map(dtm_corpus,stripWhitespace)
```

### Create Document Term Matrix

The unigrams, bigrams and trigram of document matrix can be analyzed to examine most frequently occurring words.

#### Unigram of DTM

The unigram are single word phase from the document term matrix is created and the sparse terms are removed. The tweet ID , airlines and the tweets are added to thedataframe created . The term are placed across the columns and their occurence across each tweet are indicated either 0 or 1.

``` r
# Creating DTM
unigram_dtm <- DocumentTermMatrix(dtm_corpus)

# Inspecting the unigram DTM
inspect(unigram_dtm[1:5,5:10])
```

    ## <<DocumentTermMatrix (documents: 5, terms: 6)>>
    ## Non-/sparse entries: 0/30
    ## Sparsity           : 100%
    ## Maximal term length: 17
    ## Weighting          : term frequency (tf)
    ## Sample             :
    ##     Terms
    ## Docs aacustomerservice aadavantage aadelay aadfw aadv aadvantage
    ##    1                 0           0       0     0    0          0
    ##    2                 0           0       0     0    0          0
    ##    3                 0           0       0     0    0          0
    ##    4                 0           0       0     0    0          0
    ##    5                 0           0       0     0    0          0

``` r
# Removing Sparseterms from the DTM 
unigram_sparse <- removeSparseTerms(unigram_dtm,0.995)
unigram_sparse
```

    ## <<DocumentTermMatrix (documents: 14640, terms: 319)>>
    ## Non-/sparse entries: 63247/4606913
    ## Sparsity           : 99%
    ## Maximal term length: 18
    ## Weighting          : term frequency (tf)

``` r
unigram_df <- as.data.frame(as.matrix(unigram_sparse))
colnames(unigram_df) <- paste("unigram", colnames(unigram_df), sep = "_")
colnames(unigram_df) <- make.names(colnames(unigram_df))

unigram_df$tweet_id <- twitter_airline$tweet_id
unigram_df$text <- twitter_airline$text
unigram_df$airline <- twitter_airline$airline
dim(unigram_df)
```

    ## [1] 14640   322

#### Bigram of DTM

The bigrams are two word phase from the document term matrix is created and the sparse terms are removed.

``` r
#Tokenizing

BigramTokenizer <-function(x) unlist(lapply(ngrams(words(x), 2), paste, 
                                            collapse = " "), use.names = FALSE)
bigram_dtm <- DocumentTermMatrix(dtm_corpus, 
                                 control=list(tokenize=BigramTokenizer))

# Inspecting the bigram DTM
inspect(bigram_dtm[1:5,5:10])
```

    ## <<DocumentTermMatrix (documents: 5, terms: 6)>>
    ## Non-/sparse entries: 0/30
    ## Sparsity           : 100%
    ## Maximal term length: 21
    ## Weighting          : term frequency (tf)
    ## Sample             :
    ##     Terms
    ## Docs aadavantage miles aadfw gma aadv svc aadvantage dividend
    ##    1                 0         0        0                   0
    ##    2                 0         0        0                   0
    ##    3                 0         0        0                   0
    ##    4                 0         0        0                   0
    ##    5                 0         0        0                   0
    ##     Terms
    ## Docs aadvantage membership aadvantage mileage
    ##    1                     0                  0
    ##    2                     0                  0
    ##    3                     0                  0
    ##    4                     0                  0
    ##    5                     0                  0

``` r
# Removing Sparseterms from the DTM 
bigram_sparse <- removeSparseTerms(bigram_dtm,0.995)
bigram_sparse
```

    ## <<DocumentTermMatrix (documents: 14640, terms: 12)>>
    ## Non-/sparse entries: 2230/173450
    ## Sparsity           : 99%
    ## Maximal term length: 19
    ## Weighting          : term frequency (tf)

``` r
bigram_df <- as.data.frame(as.matrix(bigram_sparse))
colnames(bigram_df) <- paste("bigram", colnames(bigram_df), sep = "_")
colnames(bigram_df) <- make.names(colnames(bigram_df))
bigram_df$tweet_id <- twitter_airline$tweet_id
bigram_df$text <- twitter_airline$text
bigram_df$airline <- twitter_airline$airline
dim(bigram_df)
```

    ## [1] 14640    15

#### Trigram of DTM

The trigrams are three word phase from the document term matrix is created and the sparse terms are removed.

``` r
#Tokenizing
TrigramTokenizer <-function(x) unlist(lapply(ngrams(words(x), 3), 
                                     paste, collapse = " "), use.names = FALSE)
trigram_dtm <- DocumentTermMatrix(dtm_corpus, 
                                  control=list(tokenize=TrigramTokenizer))

# Inspecting the trigram DTM
inspect(trigram_dtm[1:5,5:10])
```

    ## <<DocumentTermMatrix (documents: 5, terms: 6)>>
    ## Non-/sparse entries: 0/30
    ## Sparsity           : 100%
    ## Maximal term length: 28
    ## Weighting          : term frequency (tf)
    ## Sample             :
    ##     Terms
    ## Docs aadfw gma amso aadvantage dividend miles aadvantage membership merged
    ##    1              0                         0                            0
    ##    2              0                         0                            0
    ##    3              0                         0                            0
    ##    4              0                         0                            0
    ##    5              0                         0                            0
    ##     Terms
    ## Docs aadvantage mileage saver aadvantage miles record
    ##    1                        0                       0
    ##    2                        0                       0
    ##    3                        0                       0
    ##    4                        0                       0
    ##    5                        0                       0
    ##     Terms
    ## Docs aadvantage number connected
    ##    1                           0
    ##    2                           0
    ##    3                           0
    ##    4                           0
    ##    5                           0

``` r
# Removing Sparseterms from the DTM 
trigram_sparse <- removeSparseTerms(trigram_dtm,0.999)
trigram_sparse
```

    ## <<DocumentTermMatrix (documents: 14640, terms: 21)>>
    ## Non-/sparse entries: 578/306862
    ## Sparsity           : 100%
    ## Maximal term length: 28
    ## Weighting          : term frequency (tf)

``` r
trigram_df <- as.data.frame(as.matrix(trigram_sparse))
colnames(trigram_df) <- paste("trigram", colnames(trigram_df), sep = "_")
colnames(trigram_df) <- make.names(colnames(trigram_df))
trigram_df$tweet_id <- twitter_airline$tweet_id
trigram_df$text <- twitter_airline$text
trigram_df$airline <- twitter_airline$airline
dim(trigram_df)
```

    ## [1] 14640    24

### Preparing for the model

Combining the unigram, bigrams, trigrams and sentiment into one dataframe for modeling

``` r
test_df  <- inner_join(unigram_df, bigram_df, by = c("tweet_id", "text", 
                                                     "airline"))

test_df1  <- inner_join(test_df, trigram_df, by = c("tweet_id", "text", 
                                                    "airline"))

test_df2 <- inner_join(all_airlines_sentiment, test_df1, by = c("tweet_id",
                                                          "text", "airline"))


# Adding Variables for modeling
test_df2 <- test_df2 %>% 
  mutate(Is_Negative = ifelse(sentiment_polarity ==  "negative",
                              1,0)) 

test_df2 <- test_df2 %>% 
  mutate(Is_Neutral = ifelse(sentiment_polarity ==  "neutral",
                             1,0)) 

test_df2 <- test_df2 %>% 
  mutate(Is_Positive = ifelse(sentiment_polarity ==  "positive",
                             1,0 )) 
```

### Mapping the tweets reason

Creating new feature tweetreason to categorize the negative reason into more meaningful variables

``` r
test_df2$negativereason <- as.character(test_df2$negativereason)
test_df2 <- test_df2 %>% 
  mutate(tweetreason = ifelse(negativereason == "","Can't Tell",negativereason))  

#Mapping Flight Issues
test_df2$tweetreason <- mapvalues(test_df2$tweetreason,
                      c("Bad Flight","Cancelled Flight","Late Flight"), 
                      c("Flight Issue","Flight Issue","Flight Issue"))
#Mapping Luggage Issues
test_df2$tweetreason <- mapvalues(test_df2$tweetreason,
                      c("Damaged Luggage","Lost Luggage"), 
                      c("Luggage Issue","Luggage Issue"))
#Mapping Customer Service Issue
test_df2$tweetreason <- mapvalues(test_df2$tweetreason,
                      c("Flight Booking Problems", "Flight Attendant Complaints",
                        "longlines"), 
                      c("Customer Service Issue","Customer Service Issue",
                        "Customer Service Issue"))
dim(test_df2)
```

    ## [1] 16810   370

``` r
# Plotting the count of negative reason 

ggplot(test_df2, aes(x=tweetreason)) +
  geom_bar(aes(y=..count..)) +geom_text(stat='count',
  aes(label=..count..),vjust=-0.5)+
  scale_fill_brewer(palette="Dark2") +
 labs(x="Negative Reason", y="Number of Tweets") +
  ggtitle("Tweets by Negative Reason")+
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 35, hjust = 1))
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/Mapping%20tweet%20reason-1.png)

``` r
test_df2$tweetreason <- as.factor(test_df2$tweetreason)
```

### User TimeZone

Creating new features timezone to categorize the various timezone into more meaningful variables

``` r
test_df2$timezone <- as.character(test_df2$timezone)

test_df2 <- test_df2 %>% 
  mutate(timezone = ifelse(timezone == "","NA",timezone))  


dim(test_df2)
```

    ## [1] 16810   370

``` r
#Mapping US Timezones
test_df2$timezone <- mapvalues(test_df2$timezone,
                      c("Atlantic Time (Canada)", "Central Time (US & Canada)",
                        "Eastern Time (US & Canada)","Mountain Time (US & Canada)",
                        "Pacific Time (US & Canada)"), 
                      c("AST","CST", "EST","MST","PST"))
 


# Plotting the count of negative reason 

ggplot(test_df2, aes(x=timezone)) +
  geom_bar(aes(y=..count..)) +geom_text(stat='count',
  aes(label=..count..),vjust=-0.5)+
  scale_fill_brewer(palette="Dark2") +
 labs(x="Negative Reason", y="Number of Tweets") +
  ggtitle("Tweets by Timezone")+
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 35, hjust = 1))
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/Mapping%20timezone-1.png)

``` r
test_df2$timezone<- as.factor(test_df2$timezone)
```

### Unigram Modeling

Creating a dataframe test\_unigram which includes tweetreason, timezone, sentiment\_freq,Is\_Positive,Is\_Negative,Is\_Neutral and all unigram varialbles. Losgistic regression is applied for all positive/neutral/negative sentiments based on the tweetreason.

``` r
test_unigram <- select(test_df2,tweetreason, timezone,
                           sentiment_freq,Is_Positive,Is_Negative,Is_Neutral,
                          starts_with("unigram"))


positive_model_unigram <- glm(Is_Positive ~ tweetreason, 
                              family = binomial("logit") , data = test_unigram)

negative_model_unigram <- glm(Is_Negative ~ tweetreason, 
                          family = binomial("logit") , data = test_unigram)

neutral_model_unigram <- glm(Is_Neutral ~ tweetreason, 
                             family = binomial("logit") , data = test_unigram)
```

### Bigram modeling

Creating a dataframe test\_bigram which includes tweetreason, timezone, sentiment\_freq,Is\_Positive,Is\_Negative,Is\_Neutral and all bigram varialbles. Losgistic regression is applied for all positive/neutral/negative sentiments based on the tweetreason.

``` r
test_bigram <- select(test_df2,tweetreason, timezone,
                           sentiment_freq,Is_Positive,Is_Negative,Is_Neutral,
                          starts_with("bigram"))


positive_model_bigram <- glm(Is_Positive ~ tweetreason, 
                              family = binomial("logit") , data = test_bigram)

negative_model_bigram <- glm(Is_Negative ~ tweetreason, 
                          family = binomial("logit") , data = test_bigram)

neutral_model_bigram <- glm(Is_Neutral ~ tweetreason, 
                             family = binomial("logit") , data = test_bigram)
```

### Trigram Modeling

Creating a dataframe test\_trigram which includes tweetreason, timezone, sentiment\_freq,Is\_Positive,Is\_Negative,Is\_Neutral and all trigram varialbles. Losgistic regression is applied for all positive/neutral/negative sentiments based on the tweetreason.

``` r
test_trigram <- select(test_df2,tweetreason, timezone,
                           sentiment_freq,Is_Positive,Is_Negative,Is_Neutral,
                          starts_with("trigram"))

positive_model_trigram <- glm(Is_Positive ~ tweetreason , 
                              family = binomial("logit") , data = test_trigram)

negative_model_trigram <- glm(Is_Negative ~ tweetreason, 
                          family = binomial("logit") , data = test_trigram)

neutral_model_trigram <- glm(Is_Neutral ~ tweetreason , 
                             family = binomial("logit") , data = test_trigram)
```

### Data Split

Dataset is split using createDataPartition from the caret package. The dataset is split into 70:30 ratio.

``` r
## split training data into train batch and test batch
set.seed(23)


training_rows <- createDataPartition(test_df2$sentiment_polarity, 
                                     p = 0.7, list = FALSE)

train_batch <- test_df2[training_rows, ]
test_batch <- test_df2[-training_rows, ]
train_control <- trainControl(method = "repeatedcv", repeats = 5,
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE)
```

### Mapping Values for training dataset

Mapping the values for training dataset and creating dataframe for all the unigrams/bigrams/trigrams for positive/neutral/negative sentiment. SMOTE function is used to balance the dataset.

``` r
train_batch$Is_Positive <- as.factor(train_batch$Is_Positive)
train_batch$Is_Negative <- as.factor(train_batch$Is_Negative)
train_batch$Is_Neutral <- as.factor(train_batch$Is_Neutral)

train_unigram <- select(train_batch,tweetreason, timezone,
                           sentiment_freq,Is_Positive,Is_Negative,Is_Neutral,
                          starts_with("unigram"))


train_unigram_pos <- SMOTE(Is_Positive ~ . -Is_Negative-Is_Neutral, 
                           data =  train_unigram,perc.over=100,perc.under=200)
train_unigram_neg <- SMOTE(Is_Negative ~ .-Is_Positive-Is_Neutral, 
                           data = train_unigram, perc.over=100,perc.under=200)
train_unigram_neu <- SMOTE(Is_Neutral ~ . -Is_Positive-Is_Negative,
                           data = train_unigram,perc.over=100,perc.under=200)


train_bigram <- select(train_batch,tweetreason, timezone,
                           sentiment_freq,Is_Positive,Is_Negative,Is_Neutral,
                          starts_with("bigram"))
train_bigram_pos <- SMOTE(Is_Positive ~ . -Is_Negative-Is_Neutral,
                          data =  train_bigram,perc.over=100,perc.under=200)
train_bigram_neg <- SMOTE(Is_Negative ~ .-Is_Positive-Is_Neutral, 
                         data =  train_bigram,perc.over=100,perc.under=200)
train_bigram_neu <- SMOTE(Is_Neutral ~  . -Is_Positive-Is_Negative, 
                         data =  train_bigram,perc.over=100,perc.under=200)



train_trigram <-select(train_batch,tweetreason, timezone,
                           sentiment_freq,Is_Positive,Is_Negative,Is_Neutral,
                           starts_with("trigram"))

train_trigram_pos <- SMOTE(Is_Positive ~ . -Is_Negative-Is_Neutral,
                          data =  train_trigram,perc.over=100,perc.under=200)
train_trigram_neg <- SMOTE(Is_Negative ~ .-Is_Positive-Is_Neutral, 
                         data =  train_trigram,perc.over=100,perc.under=200)
train_trigram_neu <- SMOTE(Is_Neutral ~  . -Is_Positive-Is_Negative, 
                         data =  train_trigram,perc.over=100,perc.under=200)




#Mapping unigrams
train_unigram_pos <- train_unigram_pos %>% mutate(Is_Positive  = 
                    ifelse(Is_Positive == "1", "Positive","NotPostitive"))

train_unigram_neg <- train_unigram_neg %>% mutate(Is_Negative  = 
                    ifelse(Is_Negative == "1", "Negative","NotNegative"))

train_unigram_neu <- train_unigram_neu %>% mutate(Is_Neutral  = 
                    ifelse(Is_Neutral == "1", "Neutral","NotNeutral"))

#Mapping bigrams
train_bigram_pos <- train_bigram_pos %>% mutate(Is_Positive  = 
                    ifelse(Is_Positive == "1", "Positive","NotPostitive"))

train_bigram_neg <- train_bigram_neg %>% mutate(Is_Negative  = 
                    ifelse(Is_Negative == "1", "Negative","NotNegative"))

train_bigram_neu <- train_bigram_neu %>% mutate(Is_Neutral  = 
                    ifelse(Is_Neutral == "1", "Neutral","NotNeutral"))

#Mapping trigrams
train_trigram_pos <- train_trigram_pos %>% mutate(Is_Positive  = 
                    ifelse(Is_Positive == "1", "Positive","NotPostitive"))

train_trigram_neg <- train_trigram_neg %>% mutate(Is_Negative  = 
                    ifelse(Is_Negative == "1", "Negative","NotNegative"))

train_trigram_neu <- train_trigram_neu %>% mutate(Is_Neutral  = 
                    ifelse(Is_Neutral == "1", "Neutral","NotNeutral"))
```

### Mapping Values for test dataset

Mapping the values for test dataset and creating dataframe for all the unigrams/bigrams/trigrams for positive/neutral/negative sentiment.

``` r
test_batch$Is_Positive <- as.factor(test_batch$Is_Positive)
test_batch$Is_Negative <- as.factor(test_batch$Is_Negative)
test_batch$Is_Neutral <- as.factor(test_batch$Is_Neutral)

#test_batch[sapply(test_batch, is.numeric)] <-
 # lapply(test_batch[sapply(test_batch, is.numeric)], as.factor)


pred_unigram <- select(test_batch,tweetreason, timezone,
                           sentiment_freq,Is_Positive,Is_Negative,Is_Neutral,
                          starts_with("unigram"))
pred_unigram_pos <- pred_unigram[-c(5,6)]
pred_unigram_neg <- pred_unigram[-c(4,6)]
pred_unigram_neu <- pred_unigram[-c(4,5)]

pred_bigram <- select(test_batch,tweetreason, timezone,
                           sentiment_freq,Is_Positive,Is_Negative,Is_Neutral,
                          starts_with("bigram"))

pred_bigram_pos <- pred_bigram[-c(5,6)]
pred_bigram_neg <- pred_bigram[-c(4,6)]
pred_bigram_neu <- pred_bigram[-c(4,5)]

  
pred_trigram <-select(test_batch,tweetreason, timezone,
                           sentiment_freq,Is_Positive,Is_Negative,Is_Neutral,
                           starts_with("trigram"))

pred_trigram_pos <- pred_trigram[-c(5,6)]
pred_trigram_neg <- pred_trigram[-c(4,6)]
pred_trigram_neu <- pred_trigram[-c(4,5)]




#Mapping unigrams
pred_unigram_pos <- pred_unigram_pos %>% mutate(Is_Positive  = 
                    ifelse(Is_Positive == "1", "Positive","NotPostitive"))

pred_unigram_neg <- pred_unigram_neg %>% mutate(Is_Negative  = 
                    ifelse(Is_Negative == "1", "Negative","NotNegative"))

pred_unigram_neu <- pred_unigram_neu %>% mutate(Is_Neutral  = 
                    ifelse(Is_Neutral == "1", "Neutral","NotNeutral"))

#Mapping bigrams
pred_bigram_pos <- pred_bigram_pos %>% mutate(Is_Positive  = 
                    ifelse(Is_Positive == "1", "Positive","NotPostitive"))

pred_bigram_neg <- pred_bigram_neg %>% mutate(Is_Negative  = 
                    ifelse(Is_Negative == "1", "Negative","NotNegative"))

pred_bigram_neu <- pred_bigram_neu %>% mutate(Is_Neutral  = 
                    ifelse(Is_Neutral == "1", "Neutral","NotNeutral"))

#Mapping trigrams
pred_trigram_pos <- pred_trigram_pos %>% mutate(Is_Positive  = 
                    ifelse(Is_Positive == "1", "Positive","NotPostitive"))

pred_trigram_neg <- pred_trigram_neg %>% mutate(Is_Negative  = 
                    ifelse(Is_Negative == "1", "Negative","NotNegative"))

pred_trigram_neu <- pred_trigram_neu %>% mutate(Is_Neutral  = 
                    ifelse(Is_Neutral == "1", "Neutral","NotNeutral"))
```

### Cross Validation for Logistics Regression for tweet reason

Sentiment variables are converted into factors for all the dataframe and cross validation is performed using logistics regression with tweetreason on all the sentiment for unigram/bigram/trigram.

``` r
train_unigram_pos$Is_Positive <- as.factor(train_unigram_pos$Is_Positive)
train_unigram_neg$Is_Negative<- as.factor(train_unigram_neg$Is_Negative)
train_unigram_neu$Is_Neutral <- as.factor(train_unigram_neu$Is_Neutral)

train_bigram_pos$Is_Positive <- as.factor(train_bigram_pos$Is_Positive)
train_bigram_neg$Is_Negative<- as.factor(train_bigram_neg$Is_Negative)
train_bigram_neu$Is_Neutral <- as.factor(train_bigram_neu$Is_Neutral)

train_trigram_pos$Is_Positive <- as.factor(train_trigram_pos$Is_Positive)
train_trigram_neg$Is_Negative<- as.factor(train_trigram_neg$Is_Negative)
train_trigram_neu$Is_Neutral <- as.factor(train_trigram_neu$Is_Neutral)

#Unigram Cross Validation 
system.time(uni_model_pos <- train(Is_Positive ~ tweetreason,
                          data=train_unigram_pos, trControl=train_control,
                          method = "glm", metric = "ROC",
                          family = binomial("logit")))
```

    ##    user  system elapsed 
    ##   2.422   0.178   2.710

``` r
system.time(uni_model_neg <- train(Is_Negative ~ tweetreason,
                          data=train_unigram_neg, trControl=train_control,
                          method = "glm", metric = "ROC",
                          family = binomial("logit")))
```

    ##    user  system elapsed 
    ##   4.915   0.576   5.590

``` r
system.time(uni_model_neu <- train(Is_Neutral ~ tweetreason,
                          data=train_unigram_neu, trControl=train_control,
                          method = "glm", metric = "ROC",
                          family = binomial("logit")))
```

    ##    user  system elapsed 
    ##   6.218   0.682   7.084

``` r
#Bigram Cross Validation 
system.time(bi_model_pos <- train(Is_Positive ~ tweetreason,
                          data=train_bigram_pos, trControl=train_control,
                          method = "glm", metric = "ROC",
                          family = binomial("logit")))
```

    ##    user  system elapsed 
    ##   2.376   0.075   2.536

``` r
system.time(bi_model_neg <- train(Is_Negative ~ tweetreason,
                          data=train_bigram_neg, trControl=train_control,
                          method = "glm", metric = "ROC",
                          family = binomial("logit")))
```

    ##    user  system elapsed 
    ##   5.283   0.564   6.051

``` r
system.time(bi_model_neu <- train(Is_Neutral ~ tweetreason,
                          data=train_bigram_neu, trControl=train_control,
                          method = "glm", metric = "ROC",
                          family = binomial("logit")))
```

    ##    user  system elapsed 
    ##   6.219   0.672   7.172

``` r
#Trigram Cross Validation 
system.time(tri_model_pos <- train(Is_Positive ~ tweetreason,
                          data=train_trigram_pos, trControl=train_control,
                          method = "glm", metric = "ROC",
                          family = binomial("logit")))
```

    ##    user  system elapsed 
    ##   2.291   0.069   2.419

``` r
system.time(tri_model_neg <- train(Is_Negative ~ tweetreason,
                          data=train_trigram_neg, trControl=train_control,
                          method = "glm", metric = "ROC",
                          family = binomial("logit")))
```

    ##    user  system elapsed 
    ##   5.081   0.535   5.866

``` r
system.time(tri_model_neu <- train(Is_Neutral ~ tweetreason,
                          data=train_trigram_neu, trControl=train_control,
                          method = "glm", metric = "ROC",
                          family = binomial("logit")))
```

    ##    user  system elapsed 
    ##   6.153   0.570   6.958

### Bootstrapping

Bootstrapping is performed using logistics regression with tweetreason on all the sentiment for unigram/bigram/trigram.

``` r
# define training control
train_bootcontrol <- trainControl(method="boot", number=20, 
                                  summaryFunction = twoClassSummary,
                        classProbs = TRUE)


#Unigram Cross Validation 
system.time(uni_boost_pos <- train(Is_Positive ~ tweetreason,
                          data=train_unigram_pos, trControl=train_bootcontrol,
                          method = "glm", metric = "ROC",
                          family = binomial("logit")))
```

    ##    user  system elapsed 
    ##   1.552   0.028   1.606

``` r
system.time(uni_boost_neg <- train(Is_Negative ~ tweetreason,
                          data=train_unigram_neg, trControl=train_bootcontrol,
                          method = "glm", metric = "ROC",
                          family = binomial("logit")))
```

    ##    user  system elapsed 
    ##   2.956   0.247   3.311

``` r
system.time(uni_boost_neu <- train(Is_Neutral ~ tweetreason,
                          data=train_unigram_neu, trControl=train_bootcontrol,
                          method = "glm", metric = "ROC",
                          family = binomial("logit")))
```

    ##    user  system elapsed 
    ##   3.531   0.269   3.899

``` r
#Bigram Cross Validation 
system.time(bi_boost_pos <- train(Is_Positive ~ tweetreason,
                          data=train_bigram_pos, trControl=train_bootcontrol,
                          method = "glm", metric = "ROC",
                          family = binomial("logit")))
```

    ##    user  system elapsed 
    ##   1.564   0.032   1.630

``` r
system.time(bi_boost_neg <- train(Is_Negative ~ tweetreason,
                          data=train_bigram_neg, trControl=train_bootcontrol,
                          method = "glm", metric = "ROC",
                          family = binomial("logit")))
```

    ##    user  system elapsed 
    ##   2.958   0.240   3.255

``` r
system.time(bi_boost_neu <- train(Is_Neutral ~ tweetreason,
                          data=train_bigram_neu, trControl=train_bootcontrol,
                          method = "glm", metric = "ROC",
                          family = binomial("logit")))
```

    ##    user  system elapsed 
    ##   3.615   0.263   3.978

``` r
#Trigram Cross Validation 
system.time(tri_boost_pos <- train(Is_Positive ~ tweetreason,
                          data=train_trigram_pos, trControl=train_bootcontrol,
                          method = "glm", metric = "ROC",
                          family = binomial("logit")))
```

    ##    user  system elapsed 
    ##   1.610   0.030   1.673

``` r
system.time(tri_boost_neg <- train(Is_Negative ~ tweetreason,
                          data=train_trigram_neg, trControl=train_bootcontrol,
                          method = "glm", metric = "ROC",
                          family = binomial("logit")))
```

    ##    user  system elapsed 
    ##   2.926   0.235   3.230

``` r
system.time(tri_boost_neu <- train(Is_Neutral ~ tweetreason,
                          data=train_trigram_neu, trControl=train_bootcontrol,
                          method = "glm", metric = "ROC",
                          family = binomial("logit")))
```

    ##    user  system elapsed 
    ##   3.522   0.244   3.844

### ROC curve for Logistic Regression

ROC curves are plotted for the all the cross validated models to check the significance of tweetreason on sentiments for all the dataframe. P & N values describe the number of positives and negative in the model

``` r
library(precrec)

#Unigram Positive
glm_train_uni_pos <- predict(uni_model_pos, train_unigram_pos, type = "prob")

sscurves <- evalmod(scores =  glm_train_uni_pos$Positive, 
                    labels = train_unigram_pos$Is_Positive)
aucs <- auc(sscurves)
# AUC of Unigram Positive
aucs <- aucs[aucs[,3] == "ROC",4]
print("AUC of Unigram Positive")
```

    ## [1] "AUC of Unigram Positive"

``` r
aucs
```

    ## [1] 0.7181216

``` r
plot(sscurves, "ROC")
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/ROC%20curve%20for%20Logistic%20Regression-1.png)

``` r
#Unigram Negative
glm_train_uni_neg <- predict(uni_model_neg, train_unigram_neg, type = "prob")


sscurves <- evalmod(scores =  glm_train_uni_neg$Negative, 
                    labels = train_unigram_neg$Is_Negative)
aucs <- auc(sscurves)

# AUC of Unigram Negative
aucs <- aucs[aucs[,3] == "ROC",4]
print("AUC of Unigram Negative")
```

    ## [1] "AUC of Unigram Negative"

``` r
aucs
```

    ## [1] 0.3378521

``` r
plot(sscurves, "ROC")
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/ROC%20curve%20for%20Logistic%20Regression-2.png)

``` r
#Unigram Neutral
glm_train_uni_neu <- predict(uni_model_neu, train_unigram_neu, type = "prob")
sscurves <- evalmod(scores =  glm_train_uni_neu$Neutral, 
                    labels = train_unigram_neu$Is_Neutral)

aucs <- auc(sscurves)

# AUC of Unigram Neutral
aucs <- aucs[aucs[,3] == "ROC",4]
print("AUC of Unigram Neutral")
```

    ## [1] "AUC of Unigram Neutral"

``` r
aucs
```

    ## [1] 0.4208846

``` r
plot(sscurves, "ROC")
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/ROC%20curve%20for%20Logistic%20Regression-3.png)

``` r
#Bigram Positive
glm_train_bi_pos <- predict(bi_model_pos, train_bigram_pos, type = "prob")
sscurves <- evalmod(scores =  glm_train_bi_pos$Positive, 
                    labels = train_bigram_pos$Is_Positive)

aucs <- auc(sscurves)

# AUC of Bigram Positive
aucs <- aucs[aucs[,3] == "ROC",4]
print("AUC of Bigram Positive")
```

    ## [1] "AUC of Bigram Positive"

``` r
aucs
```

    ## [1] 0.7135652

``` r
plot(sscurves, "ROC")
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/ROC%20curve%20for%20Logistic%20Regression-4.png)

``` r
#Bigram Negative
glm_train_bi_neg <- predict(bi_model_neg, train_bigram_neg, type = "prob")
sscurves <- evalmod(scores =  glm_train_bi_neg$Negative, 
                    labels =train_bigram_neg$Is_Negative)
aucs <- auc(sscurves)

# AUC of Bigram Negative
aucs <- aucs[aucs[,3] == "ROC",4]
print("AUC of Bigram Negative")
```

    ## [1] "AUC of Bigram Negative"

``` r
aucs
```

    ## [1] 0.3389274

``` r
plot(sscurves, "ROC")
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/ROC%20curve%20for%20Logistic%20Regression-5.png)

``` r
#Bigram Neutral
glm_train_bi_neu <- predict(bi_model_neu, train_bigram_neu, type = "prob")
sscurves <- evalmod(scores =  glm_train_bi_neu$Neutral, 
                    labels =train_bigram_neu$Is_Neutral)
aucs <- auc(sscurves)

# AUC of Bigram Neutral
aucs <- aucs[aucs[,3] == "ROC",4]
print("AUC of Bigram Neutral")
```

    ## [1] "AUC of Bigram Neutral"

``` r
aucs
```

    ## [1] 0.4162534

``` r
plot(sscurves, "ROC")
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/ROC%20curve%20for%20Logistic%20Regression-6.png)

``` r
#Trigram Positive
glm_train_tri_pos <- predict(tri_model_pos, train_trigram_pos, type = "prob")
sscurves <- evalmod(scores =  glm_train_tri_pos$Positive, 
                    labels =train_trigram_pos$Is_Positive)
aucs <- auc(sscurves)

# AUC of Trigram Positive
aucs <- aucs[aucs[,3] == "ROC",4]
print("AUC of Trigram Positive")
```

    ## [1] "AUC of Trigram Positive"

``` r
aucs
```

    ## [1] 0.7334924

``` r
plot(sscurves, "ROC")
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/ROC%20curve%20for%20Logistic%20Regression-7.png)

``` r
#Trigram Negative
glm_train_tri_neg <- predict(tri_model_neg, train_trigram_neg, type = "prob")
sscurves <- evalmod(scores =  glm_train_tri_neg$Negative,
                    labels =train_trigram_neg$Is_Negative)
aucs <- auc(sscurves)

# AUC of Trigram Negative
aucs <- aucs[aucs[,3] == "ROC",4]
print("AUC of Trigram Negative")
```

    ## [1] "AUC of Trigram Negative"

``` r
aucs
```

    ## [1] 0.3344611

``` r
plot(sscurves, "ROC")
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/ROC%20curve%20for%20Logistic%20Regression-8.png)

``` r
#Trigram Neutral
glm_train_tri_neu <- predict(tri_model_neu, train_trigram_neu, type = "prob")
sscurves <- evalmod(scores =  glm_train_tri_neu$Neutral, 
                    labels =train_trigram_neu$Is_Neutral)
aucs <- auc(sscurves)

# AUC of Trigram Neutral
aucs <- aucs[aucs[,3] == "ROC",4]
print("AUC of Trigram Neutral")
```

    ## [1] "AUC of Trigram Neutral"

``` r
aucs
```

    ## [1] 0.4458115

``` r
plot(sscurves, "ROC")
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/ROC%20curve%20for%20Logistic%20Regression-9.png)

### Cross Validation for Naive Bayes for tweetreason

cross validation is performed using Naive Bayes with tweetreason on all the sentiment for unigram/bigram/trigram.

``` r
#Unigram Cross Validation 
system.time(uni_model_pos_nb <- train(Is_Positive ~ tweetreason,
                          data=train_unigram_pos, trControl=train_control,
                          method = "nb", metric = "ROC"
                        ))
```

    ##    user  system elapsed 
    ##  27.803   0.366  28.789

``` r
#system.time(uni_model_neg_nb <- train(Is_Negative ~ tweetreason,
 #                         data=train_unigram_neg, trControl=train_control,
  #                        method = "nb", metric = "ROC"
   #                       ))

system.time(uni_model_neu_nb <- train(Is_Neutral ~ tweetreason,
                          data=train_unigram_neu, trControl=train_control,
                          method = "nb", metric = "ROC" ))
```

    ##    user  system elapsed 
    ## 140.493   1.937 149.383

``` r
#Bigram Cross Validation 
system.time(bi_model_pos_nb <- train(Is_Positive ~ tweetreason,
                          data=train_bigram_pos, trControl=train_control,
                          method = "nb", metric = "ROC"))
```

    ##    user  system elapsed 
    ##  27.232   0.146  27.583

``` r
#system.time(bi_model_neg_nb <- train(Is_Negative ~ tweetreason,
 #                         data=train_bigram_neg, trControl=train_control,
  #                        method = "nb", metric = "ROC"))

system.time(bi_model_neu_nb <- train(Is_Neutral ~ tweetreason,
                          data=train_bigram_neu, trControl=train_control,
                          method = "nb", metric = "ROC"))
```

    ##    user  system elapsed 
    ## 128.105   0.634 129.463

``` r
#Trigram Cross Validation 
system.time(tri_model_pos_nb <- train(Is_Positive ~ tweetreason,
                          data=train_trigram_pos, trControl=train_control,
                          method = "nb", metric = "ROC"))
```

    ##    user  system elapsed 
    ##  26.686   0.127  26.974

``` r
#system.time(tri_model_neg_nb <- train(Is_Negative ~ tweetreason,
 #                         data=train_trigram_neg, trControl=train_control,
  #                        method = "nb", metric = "ROC"))

system.time(tri_model_neu_nb <- train(Is_Neutral ~ tweetreason,
                          data=train_trigram_neu, trControl=train_control,
                          method = "nb", metric = "ROC"))
```

    ##    user  system elapsed 
    ## 130.684   0.753 132.384

### Cross Validation for Naive Bayes for sentiment\_freq and tweetreason

cross validation is performed using Naive Bayes with tweetreason and sentiment\_freq on all the sentiment for unigram/bigram/trigram.P & N values describe the number of positives and negative in the model

``` r
#Unigram Cross Validation 


system.time(uni_model_pos_nb <- train(Is_Positive ~ sentiment_freq + tweetreason,
                          data=train_unigram_pos, trControl=train_control,
                          method = "nb", metric = "ROC"
                        ))
```

    ##    user  system elapsed 
    ##  30.674   0.144  30.997

``` r
#system.time(uni_model_neg_nb <- train(Is_Negative ~ sentiment_freq + tweetreason,
 #                        data=train_unigram_neg, trControl=train_control,
  #                        method = "nb", metric = "ROC"
   #                       ))

system.time(uni_model_neu_nb <- train(Is_Neutral ~ sentiment_freq + tweetreason,
                          data=train_unigram_neu, trControl=train_control,
                          method = "nb", metric = "ROC" ))
```

    ##    user  system elapsed 
    ## 148.158   0.680 149.562

``` r
#Bigram Cross Validation 
system.time(bi_model_pos_nb <- train(Is_Positive ~ sentiment_freq + tweetreason,
                          data=train_bigram_pos, trControl=train_control,
                          method = "nb", metric = "ROC"))
```

    ##    user  system elapsed 
    ##  30.793   0.140  31.101

``` r
#system.time(bi_model_neg_nb <- train(Is_Negative ~ sentiment_freq + tweetreason,
 #                         data=train_bigram_neg, trControl=train_control,
  #                        method = "nb", metric = "ROC"))

system.time(bi_model_neu_nb <- train(Is_Neutral ~ sentiment_freq + tweetreason,
                          data=train_bigram_neu, trControl=train_control,
                          method = "nb", metric = "ROC"))
```

    ##     user   system  elapsed 
    ##  148.907    0.870 3557.035

``` r
#Trigram Cross Validation 
system.time(tri_model_pos_nb <- train(Is_Positive ~ sentiment_freq + tweetreason,
                          data=train_trigram_pos, trControl=train_control,
                          method = "nb", metric = "ROC"))
```

    ##    user  system elapsed 
    ##  32.512   0.276  33.483

``` r
#system.time(tri_model_neg_nb <- train(Is_Negative ~ sentiment_freq + tweetreason,
 #                         data=train_trigram_neg, trControl=train_control,
  #                        method = "nb", metric = "ROC"))

system.time(tri_model_neu_nb <- train(Is_Neutral ~ sentiment_freq + tweetreason,
                          data=train_trigram_neu, trControl=train_control,
                          method = "nb", metric = "ROC"))
```

    ##    user  system elapsed 
    ## 147.749   0.854 149.595

### ROC Curve for Naive Bayes

ROC curves are plotted for the all the cross validated models to check the significance of tweetreason and sentiment\_freq on sentiments for unigram,bigram and trigram

``` r
#unigram Positive ROC Curve
nb_train_uni_pos <- predict(uni_model_pos_nb, train_unigram_pos, type= "prob")
sscurves <- evalmod(scores =  nb_train_uni_pos$Positive, 
                    labels =train_unigram_pos$Is_Positive)

aucs <- auc(sscurves)

# AUC of Unigram Positive
aucs <- aucs[aucs[,3] == "ROC",4]
print("AUC of Unigram Positive")
```

    ## [1] "AUC of Unigram Positive"

``` r
aucs
```

    ## [1] 0.7816422

``` r
plot(sscurves, "ROC")
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/ROC%20Curve%20for%20NB-1.png)

``` r
#Unigram Negative ROC Curve
#nb_train_uni_neg <- predict(uni_model_neg_nb, train_unigram_neg, type = "prob")

#sscurves <- evalmod(scores =  nb_train_uni_neg$Negative, 
#                     labels =train_unigram_neg$Is_Negative)

#aucs <- auc(sscurves)

#aucs_pr <- auc(sscurves1)
# AUC of Trigram Negative
#aucs <- aucs[aucs[,3] == "ROC",4]
#print("AUC of Trigram Negative")
#aucs
#plot(sscurves, "ROC")

#Unigram Neutral ROC Curve
nb_train_uni_neu <- predict(uni_model_neu_nb, train_unigram_neu, type = "prob")
sscurves <- evalmod(scores =  nb_train_uni_neu$Neutral, 
                    labels =train_unigram_neu$Is_Neutral)
aucs <- auc(sscurves)

# AUC of Unigram Neutral
aucs <- aucs[aucs[,3] == "ROC",4]
print("AUC of Unigram Neutral")
```

    ## [1] "AUC of Unigram Neutral"

``` r
aucs
```

    ## [1] 0.3493536

``` r
plot(sscurves, "ROC")
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/ROC%20Curve%20for%20NB-2.png)

``` r
#bigram Positive ROC Curve
nb_train_bi_pos <- predict(bi_model_pos_nb, train_bigram_pos, type= "prob")
sscurves <- evalmod(scores =  nb_train_bi_pos$Positive,
                    labels =train_bigram_pos$Is_Positive)
aucs <- auc(sscurves)

# AUC of Bigram Positive
aucs <- aucs[aucs[,3] == "ROC",4]
print("AUC of Bigram Positive")
```

    ## [1] "AUC of Bigram Positive"

``` r
aucs
```

    ## [1] 0.7787282

``` r
plot(sscurves, "ROC")
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/ROC%20Curve%20for%20NB-3.png)

``` r
#bigram Negative ROC Curve
#nb_train_bi_neg <- predict(bi_model_neg_nb, train_bigram_neg, type = "prob")

#sscurves <- evalmod(scores =  nb_train_bi_neg$Negative, 
#                     labels =train_bigram_neg$Is_Negative)
#aucs <- auc(sscurves)

# AUC of Bigram Negative
#aucs <- aucs[aucs[,3] == "ROC",4]
#print("AUC of Bigram negative")
#aucs
#plot(sscurves, "ROC")

#Bigram Neutral ROC Curve
nb_train_bi_neu <- predict(bi_model_neu_nb, train_bigram_neu, type = "prob")
sscurves <- evalmod(scores =  nb_train_bi_neu$Neutral,
                    labels =train_bigram_neu$Is_Neutral)
aucs <- auc(sscurves)

# AUC of Bigram Neutral
aucs <- aucs[aucs[,3] == "ROC",4]
print("AUC of Bigram Neutral")
```

    ## [1] "AUC of Bigram Neutral"

``` r
aucs
```

    ## [1] 0.3098108

``` r
plot(sscurves, "ROC")
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/ROC%20Curve%20for%20NB-4.png)

``` r
#Trigram Positive ROC Curve
nb_train_tri_pos <- predict(tri_model_pos_nb, train_trigram_pos, type= "prob")
sscurves <- evalmod(scores =  nb_train_tri_pos$Positive, 
                    labels =train_trigram_pos$Is_Positive)
aucs <- auc(sscurves)

# AUC of Trigram Positive
aucs <- aucs[aucs[,3] == "ROC",4]
print("AUC of Trigram Positive")
```

    ## [1] "AUC of Trigram Positive"

``` r
aucs
```

    ## [1] 0.7964556

``` r
plot(sscurves, "ROC")
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/ROC%20Curve%20for%20NB-5.png)

``` r
#trigram Negative ROC Curve
#nb_train_tri_neg <- predict(tri_model_neg_nb, train_trigram_neg, type = "prob")

#sscurves <- evalmod(scores =  nb_train_tri_neg$Negative, 
  #labels =train_trigram_neg$Is_Negative)
#aucs <- auc(sscurves)

# AUC of Trigram Negative
#aucs <- aucs[aucs[,3] == "ROC",4]
#print("AUC of Trigram Negative")
#aucs
#plot(sscurves, "ROC")

#Trigram Neutral ROC Curve
nb_train_tri_neu <- predict(tri_model_neu_nb, train_trigram_neu, type = "prob")
sscurves <- evalmod(scores =  nb_train_tri_neu$Neutral, 
                    labels =train_trigram_neu$Is_Neutral)
aucs <- auc(sscurves)

# AUC of Trigram Neutral
aucs <- aucs[aucs[,3] == "ROC",4]
print("AUC of Trigram Neutral")
```

    ## [1] "AUC of Trigram Neutral"

``` r
aucs
```

    ## [1] 0.3213559

``` r
plot(sscurves, "ROC")
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/ROC%20Curve%20for%20NB-6.png)

### Cross Validation for Naive Bayes for entire training dataset

cross validation is performed using Naive Bayes with entire training dataset on all the sentiment for unigram/bigram/trigram. timezone is excluded as it had zero variances.

``` r
#Unigram Cross Validation 


system.time(uni_model_pos_nb1 <- train(Is_Positive ~ .-Is_Negative-Is_Neutral-timezone,
                          data=train_unigram_pos, trControl=train_control,
                          method = "nb", metric = "ROC"
                        ))
```

    ##     user   system  elapsed 
    ## 1124.998   12.468 4388.894

``` r
#system.time(uni_model_neg_nb1 <- train(Is_Negative ~. -Is_Neutral-Is_Positive-timezone,
 #                       data=train_unigram_neg, trControl=train_control,
  #                       method = "nb", metric = "ROC"
   #                      ))

system.time(uni_model_neu_nb1 <- train(Is_Neutral ~ .-Is_Negative-Is_Positive-timezone,
                          data=train_unigram_neu, trControl=train_control,
                          method = "nb", metric = "ROC" ))
```

    ##      user    system   elapsed 
    ##  5500.853   103.037 59924.322

``` r
#Bigram Cross Validation 
system.time(bi_model_pos_nb1 <- train(Is_Positive ~ .-Is_Negative-Is_Neutral-timezone,
                          data=train_bigram_pos, trControl=train_control,
                          method = "nb", metric = "ROC"))
```

    ##    user  system elapsed 
    ##  64.543   0.421  65.563

``` r
#system.time(bi_model_neg_nb1 <- train(Is_Negative ~ . -Is_Neutral-Is_Positive-timezone,
 #                       data=train_bigram_neg, trControl=train_control,
#                        method = "nb", metric = "ROC"))

system.time(bi_model_neu_nb1 <- train(Is_Neutral ~ . -Is_Neutral-Is_Positive-timezone,
                          data=train_bigram_neu, trControl=train_control,
                          method = "nb", metric = "ROC"))
```

    ##      user    system   elapsed 
    ##   337.632     6.145 25203.451

``` r
#Trigram Cross Validation 
system.time(tri_model_pos_nb1 <- train(Is_Positive ~ .-Is_Negative-Is_Neutral-timezone,
                          data=train_trigram_pos, trControl=train_control,
                          method = "nb", metric = "ROC"))
```

    ##    user  system elapsed 
    ##  93.443   0.459  94.497

``` r
#system.time(tri_model_neg_nb1 <- train(Is_Negative ~ . -Is_Neutral-Is_Positive-timezone,
 #                        data=train_trigram_neg, trControl=train_control,
  #                        method = "nb", metric = "ROC"))

#system.time(tri_model_neu_nb1 <- train(Is_Neutral ~ . -Is_Neutral-Is_Positive-timezone,
 #                         data=train_trigram_neu, trControl=train_control,
  #                        method = "nb", metric = "ROC"))
```

### ROC Curves for Entire training dataset models

ROC curves are plotted for executed cross validated models to check the significance of entire variables on sentiments for unigram/bigram/trigram.

``` r
#Unigram Positive 
library(precrec)

nb_train_uni_pos1 <- predict(uni_model_pos_nb1, train_unigram_pos, type= "prob")
 
sscurves <- evalmod(scores =  nb_train_uni_pos1$Positive, 
                    labels = train_unigram_pos$Is_Positive)
sscurves1 <- sscurves

aucs <- auc(sscurves)
aucs_pr <- auc(sscurves1)

# AUC of Unigram Positive
aucs <- aucs[aucs[,3] == "ROC",4]
print("AUC of Unigram Positive")
```

    ## [1] "AUC of Unigram Positive"

``` r
aucs
```

    ## [1] 0.5

``` r
plot(sscurves, "ROC")
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/ROC%20curves%20for%20entire%20training%20dataset-1.png)

``` r
# AUC PR of Unigram Positive
aucs_pr <- aucs_pr[aucs_pr[,3] == "PRC",4]
print("AUC PR of Unigram Positive")
```

    ## [1] "AUC PR of Unigram Positive"

``` r
aucs_pr
```

    ## [1] 0.5

``` r
plot(sscurves, "PRC")
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/ROC%20curves%20for%20entire%20training%20dataset-2.png)

``` r
#Unigram Negative ROC Curve
#nb_train_uni_neg1 <- predict(uni_model_neg_nb1, train_unigram_neg, type = "prob")
#sscurves <- evalmod(scores =  nb_train_uni_neg1$Negative, 
#labels = train_unigram_neg$Is_Negative)
#sscurves1 <- sscurves

#aucs <- auc(sscurves)
#aucs_pr <- auc(sscurves1)

# AUC of Unigram Positive
#aucs <- aucs[aucs[,3] == "ROC",4]
#print("AUC of Unigram Positive")
#aucs
#plot(sscurves, "ROC")
# AUC PR of Unigram Positive
#aucs_pr <- aucs_pr[aucs_pr[,3] == "PRC",4]
#print("AUC PR of Unigram Positive")
#aucs_pr
#plot(sscurves, "PRC")


#Unigram Neutral ROC Curve
#nb_train_uni_neu1 <- predict(uni_model_neu_nb1, train_unigram_neu, type = "prob")
#sscurves <- evalmod(scores =  uni_model_neu_nb1$Neutral, 
#labels = train_unigram_neg$Is_Negative)
#sscurves1 <- sscurves

#aucs <- auc(sscurves)
#aucs_pr <- auc(sscurves1)

# AUC of Unigram Neutral
#aucs <- aucs[aucs[,3] == "ROC",4]
#print("AUC of Unigram Neutral")
#aucs
#plot(sscurves, "ROC")
# AUC PR of Unigram Neutral
#aucs_pr <- aucs_pr[aucs_pr[,3] == "PRC",4]
#print("AUC PR of Unigram Neutral")
#aucs_pr
#plot(sscurves, "PRC")



#Bigram Positive 

nb_train_bi_pos1 <- predict(bi_model_pos_nb1, train_bigram_pos, type= "prob")
pred <- prediction( nb_train_bi_pos1$Positive, train_bigram_pos$Is_Positive) 


sscurves <- evalmod(scores = nb_train_bi_pos1$Positive, 
                    labels = train_bigram_pos$Is_Positive)
sscurves1 <- sscurves

aucs <- auc(sscurves)
aucs_pr <- auc(sscurves1)

# AUC of Bigram Positive
aucs <- aucs[aucs[,3] == "ROC",4]
print("AUC of Bigram Positive")
```

    ## [1] "AUC of Bigram Positive"

``` r
aucs
```

    ## [1] 0.8001695

``` r
plot(sscurves, "ROC")
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/ROC%20curves%20for%20entire%20training%20dataset-3.png)

``` r
# AUC PR of Unigram Positive
aucs_pr <- aucs_pr[aucs_pr[,3] == "PRC",4]
print("AUC PR of Bigram Positive")
```

    ## [1] "AUC PR of Bigram Positive"

``` r
aucs_pr
```

    ## [1] 0.7880565

``` r
plot(sscurves, "PRC")
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/ROC%20curves%20for%20entire%20training%20dataset-4.png)

``` r
#bigram Negative ROC Curve
#nb_train_bi_neg1 <- predict(bi_model_neg_nb1, train_bigram_neg, type = "prob")
#sscurves <- evalmod(scores =  nb_train_bi_neg1$Negative, 
#labels = train_bigram_neg$Is_Negative)
#sscurves1 <- sscurves

#aucs <- auc(sscurves)
#aucs_pr <- auc(sscurves1)

# AUC of Bigram Negative
#aucs <- aucs[aucs[,3] == "ROC",4]
#print("AUC of Bigram Negative")
#aucs
#plot(sscurves, "ROC")
# AUC PR of Bigram Negative
#aucs_pr <- aucs_pr[aucs_pr[,3] == "PRC",4]
#print("AUC PR of Bigram Negative")
#aucs_pr
#plot(sscurves, "PRC")




#bigram Neutral ROC Curve
nb_train_bi_neu1 <- predict(bi_model_neu_nb1, train_bigram_neu, type = "prob")
pred <- prediction( nb_train_bi_neu1$Neutral, train_bigram_neu$Is_Neutral)



sscurves <- evalmod(scores = nb_train_bi_neu1$Neutral, labels = train_bigram_neu$Is_Neutral)
sscurves1 <- sscurves

aucs <- auc(sscurves)
aucs_pr <- auc(sscurves1)

# AUC of Bigram Neutral
aucs <- aucs[aucs[,3] == "ROC",4]
print("AUC of Bigram Neutral")
```

    ## [1] "AUC of Bigram Neutral"

``` r
aucs
```

    ## [1] 0.1173694

``` r
plot(sscurves, "ROC")
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/ROC%20curves%20for%20entire%20training%20dataset-5.png)

``` r
# AUC PR of Bigram Neutral 
aucs_pr <- aucs_pr[aucs_pr[,3] == "PRC",4]
print("AUC PR of Bigram Neutral")
```

    ## [1] "AUC PR of Bigram Neutral"

``` r
aucs_pr
```

    ## [1] 0.3227377

``` r
plot(sscurves, "PRC")
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/ROC%20curves%20for%20entire%20training%20dataset-6.png)

``` r
#trigram Positive 

nb_train_tri_pos1 <- predict(tri_model_pos_nb1, train_trigram_pos, type= "prob")

sscurves <- evalmod(scores = nb_train_tri_pos1$Positive, labels = train_trigram_pos$Is_Positive)
sscurves1 <- sscurves

aucs <- auc(sscurves)
aucs_pr <- auc(sscurves1)

# AUC of Trigram Positive
aucs <- aucs[aucs[,3] == "ROC",4]
print("AUC of Trigam Positive")
```

    ## [1] "AUC of Trigam Positive"

``` r
aucs
```

    ## [1] 0.7810173

``` r
plot(sscurves, "ROC")
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/ROC%20curves%20for%20entire%20training%20dataset-7.png)

``` r
# AUC PR of Trigram Positive 
aucs_pr <- aucs_pr[aucs_pr[,3] == "PRC",4]
print("AUC PR of Trigram Positive")
```

    ## [1] "AUC PR of Trigram Positive"

``` r
aucs_pr
```

    ## [1] 0.7193496

``` r
plot(sscurves, "PRC")
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/ROC%20curves%20for%20entire%20training%20dataset-8.png)

``` r
#trigram Negative ROC Curve
#nb_train_tri_neg1 <- predict(tri_model_neg_nb1, train_trigram_neg, type = "prob")
#sscurves <- evalmod(scores = nb_train_tri_neg1$Negative, 
 #                   labels = train_trigram_neg$Is_Negative)
#sscurves1 <- sscurves

#aucs <- auc(sscurves)
#aucs_pr <- auc(sscurves1)

# AUC of Trigram Negative
#aucs <- aucs[aucs[,3] == "ROC",4]
#print("AUC of Trigam Negative")
#aucs
#plot(sscurves, "ROC")
# AUC PR of Trigram Negative 
#aucs_pr <- aucs_pr[aucs_pr[,3] == "PRC",4]
#print("AUC PR of Trigram Negative")
#aucs_pr
#plot(sscurves, "PRC")


#trigram Neutral ROC Curve
#nb_train_tri_neu1 <- predict(tri_model_neu_nb1, train_trigram_neu, type = "prob")
#pred <- prediction( nb_train_tri_neu1$Neutral, train_trigram_neu$Is_Neutral)

#sscurves <- evalmod(scores = nb_train_tri_neu1$Neutral, labels = train_trigram_neu$Is_Neutral)
#sscurves1 <- sscurves

#aucs <- auc(sscurves)
#aucs_pr <- auc(sscurves1)

# AUC of Trigram Neutral
#aucs <- aucs[aucs[,3] == "ROC",4]
#print("AUC of Trigam Neutral")
#aucs
#plot(sscurves, "ROC")
# AUC PR of Trigram Neutral 
#aucs_pr <- aucs_pr[aucs_pr[,3] == "PRC",4]
#print("AUC PR of Trigram Neutral")
#aucs_pr
#plot(sscurves, "PRC")
```

### Cross Validation for Naive Bayes for entire testing dataset

cross validation is performed using Naive Bayes with entire test dataset on all the sentiment for unigram/bigram/trigram. timezone is excluded as it had zero variances. P & N values describe the number of positives and negative in the model

``` r
pred_unigram_pos$Is_Positive <- as.factor(pred_unigram_pos$Is_Positive)
pred_unigram_neg$Is_Negative<- as.factor(pred_unigram_neg$Is_Negative)
pred_unigram_neu$Is_Neutral <- as.factor(pred_unigram_neu$Is_Neutral)

pred_bigram_pos$Is_Positive <- as.factor(pred_bigram_pos$Is_Positive)
pred_bigram_neg$Is_Negative<- as.factor(pred_bigram_neg$Is_Negative)
pred_bigram_neu$Is_Neutral <- as.factor(pred_bigram_neu$Is_Neutral)

pred_trigram_pos$Is_Positive <- as.factor(pred_trigram_pos$Is_Positive)
pred_trigram_neg$Is_Negative<- as.factor(pred_trigram_neg$Is_Negative)
pred_trigram_neu$Is_Neutral <- as.factor(pred_trigram_neu$Is_Neutral)
#Unigram Cross Validation 


#system.time(uni_test_pos_nb1 <- train(Is_Positive ~ .-timezone,
 #                         data=pred_unigram_pos, trControl=train_control,
  #                      method = "nb", metric = "ROC"
   #                   ))
#system.time(uni_test_neg_nb1 <- train(Is_Negative ~. -timezone,
 #                        data=pred_unigram_neg, trControl=train_control,
  #                        method = "nb", metric = "ROC"
   #                       ))

#system.time(uni_test_neu_nb1 <- train(Is_Neutral ~ .-timezone,
 #                         data=pred_unigram_neu, trControl=train_control,
  #                        method = "nb", metric = "ROC" ))

#Bigram Cross Validation 
system.time(bi_test_pos_nb1 <- train(Is_Positive ~ .-timezone,
                          data=pred_bigram_pos, trControl=train_control,
                          method = "nb", metric = "ROC"))
```

    ##    user  system elapsed 
    ## 111.680   1.079 114.749

``` r
system.time(bi_test_neg_nb1 <- train(Is_Negative ~ . -timezone,
                       data=pred_bigram_neg, trControl=train_control,
                          method = "nb", metric = "ROC"))
```

    ##    user  system elapsed 
    ## 138.155   1.287 146.240

``` r
#system.time(bi_test_neu_nb1 <- train(Is_Neutral ~ . -timezone,
 #                         data=pred_bigram_neu, trControl=train_control,
  #                        method = "nb", metric = "ROC"))

#Trigram Cross Validation 
#system.time(tri_test_pos_nb1 <- train(Is_Positive ~ .-timezone,
 #                         data=pred_trigram_pos, trControl=train_control,
  #                        method = "nb", metric = "ROC"))


system.time(tri_test_neg_nb1 <- train(Is_Negative ~ .-timezone,
                          data=pred_trigram_neg, trControl=train_control,
                          method = "nb", metric = "ROC"))
```

    ##    user  system elapsed 
    ## 159.708   1.088 162.729

``` r
#system.time(tri_test_neu_nb1 <- train(Is_Neutral ~ .-timezone,
 
#                         data=pred_trigram_neu, trControl=train_control,
 #                         method = "nb", metric = "ROC"))
```

ROC Curves for the test dataset
===============================

ROC curves are plotted for executed cross validated models to check the significance of entire variables on sentiments for unigram/bigram/trigram

``` r
#Unigram Positive 


#nb_test_uni_pos1 <- predict(uni_test_pos_nb1, pred_unigram_pos, type= "prob")
#pred <- prediction( nb_test_uni_pos1$Positive, pred_unigram_pos$Is_Positive) 
#nb_test_uni_pos_roc1 <- roc(response = pred_unigram_pos$Is_Positive,
 #           predictor = nb_test_uni_pos1$Positive,
  #          levels = levels(pred_unigram_pos$Is_Positive))
#au <- auc(nb_test_uni_pos_roc1)
#plot(nb_test_uni_pos_roc, type = "S", 
 #    main = paste("Naive Bayes Unigram Positive , AUC = ", round(au,2)))


#nb_test_uni_pos_pr <- performance(pred, measure = "prec", x.measure = "rec")
#plot(nb_test_uni_pos_pr, main="Unigram Positive Precision/recall curve")


#Unigram Negative ROC Curve

#nb_test_uni_neg1 <- predict(uni_test_neg_nb1, pred_unigram_neg, type = "prob")
#pred <- prediction( nb_test_uni_neg1$Negative, pred_unigram_neg$Is_Negative) 
#sscurves <- evalmod(scores =  nb_test_uni_neg1$Negative, labels = pred_unigram_neg$Is_Negative)
#sscurves1 <- sscurves

#aucs <- auc(sscurves)
#aucs_pr <- auc(sscurves1)

# AUC of Unigram Negative
#aucs <- aucs[aucs[,3] == "ROC",4]
#print("AUC of Unigram Negative")
#aucs
#plot(sscurves, "ROC")
# AUC PR of Unigram Negative
#aucs_pr <- aucs_pr[aucs_pr[,3] == "PRC",4]
#print("AUC PR of Unigram Negative")
#aucs_pr
#plot(sscurves, "PRC")


#Unigram Neutral ROC Curve
#nb_test_uni_neu1 <- predict(uni_test_neu_nb1, pred_unigram_neu, type = "prob")
#nb_test_uni_neu_roc1 <- roc(response = pred_unigram_neu$Is_Neutral,
#            predictor = nb_test_uni_neu1$Neutral,
 #           levels = levels(pred_unigram_neu$Is_Neutral))
#au <-auc(nb_test_uni_neu_roc1)
#plot(nb_test_uni_neu_roc1, type = "S", 
#     main = paste("Naive Bayes Unigram Neutral , AUC = ", round(au,2)))

#nb_test_uni_neu_pr <- performance(pred, measure = "prec", x.measure = "rec")
#plot(nb_test_uni_neu_pr, main="Unigram Neutral Precision/recall curve")



#bigram Positive 

nb_test_bi_pos1 <- predict(bi_test_pos_nb1, pred_bigram_pos, type= "prob")

sscurves <- evalmod(scores =  nb_test_bi_pos1$Positive, labels = pred_bigram_pos$Is_Positive)
sscurves1 <- sscurves

aucs <- auc(sscurves)
aucs_pr <- auc(sscurves1)

# AUC of Bigram Positive
aucs <- aucs[aucs[,3] == "ROC",4]
print("AUC of Bigram Positive")
```

    ## [1] "AUC of Bigram Positive"

``` r
aucs
```

    ## [1] 0.7462536

``` r
plot(sscurves, "ROC")
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/ROC%20Curve%20for%20test%20dataset-1.png)

``` r
# AUC PR of Bigram Positive
aucs_pr <- aucs_pr[aucs_pr[,3] == "PRC",4]
print("AUC PR of Bigram Positive")
```

    ## [1] "AUC PR of Bigram Positive"

``` r
aucs_pr
```

    ## [1] 0.1383809

``` r
plot(sscurves, "PRC")
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/ROC%20Curve%20for%20test%20dataset-2.png)

``` r
#bigram Negative ROC Curve
nb_test_bi_neg1 <- predict(bi_test_neg_nb1, pred_bigram_neg, type = "prob")

sscurves <- evalmod(scores =  nb_test_bi_neg1$Negative, labels = pred_bigram_neg$Is_Negative)
sscurves1 <- sscurves

aucs <- auc(sscurves)
aucs_pr <- auc(sscurves1)

# AUC of Bigram Negative
aucs <- aucs[aucs[,3] == "ROC",4]
print("AUC of Bigram Negative")
```

    ## [1] "AUC of Bigram Negative"

``` r
aucs
```

    ## [1] 0.19419

``` r
plot(sscurves, "ROC")
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/ROC%20Curve%20for%20test%20dataset-3.png)

``` r
# AUC PR of Bigram Negative
aucs_pr <- aucs_pr[aucs_pr[,3] == "PRC",4]
print("AUC PR of Bigram Negative")
```

    ## [1] "AUC PR of Bigram Negative"

``` r
aucs_pr
```

    ## [1] 0.59856

``` r
plot(sscurves, "PRC")
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/ROC%20Curve%20for%20test%20dataset-4.png)

``` r
#bigram Neutral ROC Curve
#nb_test_bi_neu1 <- predict(bi_test_neu_nb1, pred_bigram_neu, type = "prob")

#sscurves <- evalmod(scores =  nb_test_bi_neu1$Neutral, labels = pred_bigram_neu$Is_Neutral)
#sscurves1 <- sscurves

#aucs <- auc(sscurves)
#aucs_pr <- auc(sscurves1)

# AUC of Bigram Neutral
#aucs <- aucs[aucs[,3] == "ROC",4]
#print("AUC of Bigram Neutral")
#aucs
#plot(sscurves, "ROC")
# AUC PR of Bigram Neutral
#aucs_pr <- aucs_pr[aucs_pr[,3] == "PRC",4]
#print("AUC PR of Bigram Neutral")
#aucs_pr
#plot(sscurves, "PRC")

#trigram Positive 

#nb_test_tri_pos1 <- predict(tri_test_pos_nb1, pred_trigram_pos, type= "prob")
#sscurves <- evalmod(scores =  nb_test_tri_pos1$Positive, labels = pred_trigram_pos$Is_Positive)
#sscurves1 <- sscurves

#aucs <- auc(sscurves)
#aucs_pr <- auc(sscurves1)

# AUC of Trigram Positive
#aucs <- aucs[aucs[,3] == "ROC",4]
#print("AUC of Trigram Positive")
#aucs
#plot(sscurves, "ROC")
# AUC PR of Trigram Positive
#aucs_pr <- aucs_pr[aucs_pr[,3] == "PRC",4]
#print("AUC PR of Trigram Positive")
#aucs_pr
#plot(sscurves, "PRC")

#Trigram Negative ROC Curve
nb_test_tri_neg1 <- predict(tri_test_neg_nb1, pred_trigram_neg, type = "prob")
sscurves <- evalmod(scores =  nb_test_tri_neg1$Negative, labels = pred_trigram_neg$Is_Negative)
sscurves1 <- sscurves

aucs <- auc(sscurves)
aucs_pr <- auc(sscurves1)

# AUC of Trigram Negative
aucs <- aucs[aucs[,3] == "ROC",4]
print("AUC of Trigram Negative")
```

    ## [1] "AUC of Trigram Negative"

``` r
aucs
```

    ## [1] 0.2203082

``` r
plot(sscurves, "ROC")
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/ROC%20Curve%20for%20test%20dataset-5.png)

``` r
# AUC PR of Trigram Negative
aucs_pr <- aucs_pr[aucs_pr[,3] == "PRC",4]
print("AUC PR of Trigram Negative")
```

    ## [1] "AUC PR of Trigram Negative"

``` r
aucs_pr
```

    ## [1] 0.6140335

``` r
plot(sscurves, "PRC")
```

![](Airline_Sentiment_All_Airlines_2_files/figure-markdown_github/ROC%20Curve%20for%20test%20dataset-6.png)

``` r
#trigram Neutral ROC Curve
#nb_test_tri_neu1 <- predict(tri_test_neu_nb1, pred_trigram_neu, type = "prob")

#sscurves <- evalmod(scores =  nb_test_tri_neu1$Neutral, labels =  pred_trigram_neu$Is_Neutral)
#sscurves1 <- sscurves

#aucs <- auc(sscurves)
#aucs_pr <- auc(sscurves1)

# AUC of trigram Neutral
#aucs <- aucs[aucs[,3] == "ROC",4]
#print("AUC of trigram Neutral")
#aucs
#plot(sscurves, "ROC")
# AUC PR of trigram Neutral
#aucs_pr <- aucs_pr[aucs_pr[,3] == "PRC",4]
#print("AUC PR of trigram Neutral")
#aucs_pr
#plot(sscurves, "PRC")
```
