Airline Sentiment
================
2017-11-04

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
```

    ## Warning: package 'dplyr' was built under R version 3.4.2

``` r
library(plyr)
library(sentiment)
library(twitteR)
library(wordcloud)
library(ggplot2)
library(magrittr)
library(tidytext)
```

    ## Warning: package 'tidytext' was built under R version 3.4.2

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
    ##  $ text                        : Factor w/ 14427 levels "_\xd9\xd4\xe4_\xd9_\xbbRT @JetBlue: Our fleet's on fleek. http://t.co/dSDEbodmEL",..: 14016 13923 13794 13857 13659 13937 14049 13928 14015 13855 ...
    ##  $ tweet_coord                 : Factor w/ 833 levels "","[-33.87144962, 151.20821275]",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ tweet_created               : Factor w/ 6804 levels "2/16/15 23:36",..: 6414 6397 6397 6397 6396 6396 6395 6394 6393 6376 ...
    ##  $ tweet_location              : Factor w/ 3082 levels "","  || san antonio, texas||",..: 1 1 1465 1 1 1 2405 1529 2387 1529 ...
    ##  $ user_timezone               : Factor w/ 86 levels "","Abu Dhabi",..: 32 64 29 64 64 64 64 64 64 32 ...

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
inspect(tweets_corpus[1:2])
```

    ## <<SimpleCorpus>>
    ## Metadata:  corpus specific: 1, document level (indexed): 0
    ## Content:  documents: 2
    ## 
    ## [1]  What @dhepburn said.                                     
    ## [2]  plus you've added commercials to the experience... tacky.

``` r
### Clean the corpus

# Remove Punctuations
tweets_corpus <- tm_map(tweets_corpus,removePunctuation)
inspect(tweets_corpus[1:2])
```

    ## <<SimpleCorpus>>
    ## Metadata:  corpus specific: 1, document level (indexed): 0
    ## Content:  documents: 2
    ## 
    ## [1]  What dhepburn said                                  
    ## [2]  plus youve added commercials to the experience tacky

``` r
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
inspect(tweets_corpus[1:15])
```

    ## <<SimpleCorpus>>
    ## Metadata:  corpus specific: 1, document level (indexed): 0
    ## Content:  documents: 15
    ## 
    ##  [1]  What dhepburn said                                                                                                 
    ##  [2]  plus youve added commercials to the experience tacky                                                               
    ##  [3]  I didnt today Must mean I need to take another trip                                                                
    ##  [4]  its really aggressive to blast obnoxious entertainment in your guests faces amp they have little recourse          
    ##  [5]  and its a really big bad thing about it                                                                            
    ##  [6]  seriously would pay  a flight for seats that didnt have this playing\nits really the only bad thing about flying VA
    ##  [7]  yes nearly every time I fly VX this ear worm wont go away                                                          
    ##  [8]  Really missed a prime opportunity for Men Without Hats parody there                                                
    ##  [9] virginamerica Well I didntbut NOW I DO D                                                                            
    ## [10]  it was amazing and arrived an hour early Youre too good to me                                                      
    ## [11]  did you know that suicide is the second leading cause of death among teens                                         
    ## [12]  I lt pretty graphics so much better than minimal iconography D                                                     
    ## [13]  This is such a great deal Already thinking about my nd trip to Australia amp I havent even gone on my st trip yet p
    ## [14]  virginmedia Im flying your fabulous Seductive skies again U take all the stress away from travel                   
    ## [15]  Thanks

``` r
# Convert the corpus to lowercase 
tweets_corpus <- tm_map(tweets_corpus,content_transformer(tolower))

# Remove Stopwords. 
tweets_stopwords <- c(setdiff(stopwords('english'), c("r", "big","delta","united","american","airways","airlines","flight","pilot",
 "virgin","US airways","southwest","a","the","is","and")),"use", "see", 
 "used", "via", "amp","the","a","thanks","thank","aa","aaaand","i","a","the",
 "flight","airlines","flights","airway","will")
tweets_corpus <- tm_map(tweets_corpus,removeWords,tweets_stopwords)
inspect(tweets_corpus[1:15])
```

    ## <<SimpleCorpus>>
    ## Metadata:  corpus specific: 1, document level (indexed): 0
    ## Content:  documents: 15
    ## 
    ##  [1]   dhepburn said                                                                           
    ##  [2]  plus youve added commercials   experience tacky                                          
    ##  [3]   didnt today must mean  need  take another trip                                          
    ##  [4]   really aggressive  blast obnoxious entertainment   guests faces    little recourse      
    ##  [5]  and   really big bad thing                                                               
    ##  [6]  seriously  pay     seats  didnt   playing\n really   bad thing  flying va                
    ##  [7]  yes nearly every time  fly vx  ear worm wont go away                                     
    ##  [8]  really missed  prime opportunity  men without hats parody                                
    ##  [9] virginamerica well  didntbut now   d                                                      
    ## [10]    amazing and arrived  hour early youre  good                                            
    ## [11]    know  suicide is  second leading cause  death among teens                              
    ## [12]   lt pretty graphics  much better  minimal iconography d                                  
    ## [13]   is   great deal already thinking   nd trip  australia   havent even gone   st trip yet p
    ## [14]  virginmedia im flying  fabulous seductive skies  u take   stress away  travel            
    ## [15]

``` r
# Remove extra whitespace
tweets_corpus <- tm_map(tweets_corpus,stripWhitespace)
inspect(tweets_corpus[1:15])
```

    ## <<SimpleCorpus>>
    ## Metadata:  corpus specific: 1, document level (indexed): 0
    ## Content:  documents: 15
    ## 
    ##  [1]  dhepburn said                                                                  
    ##  [2]  plus youve added commercials experience tacky                                  
    ##  [3]  didnt today must mean need take another trip                                   
    ##  [4]  really aggressive blast obnoxious entertainment guests faces little recourse   
    ##  [5]  and really big bad thing                                                       
    ##  [6]  seriously pay seats didnt playing really bad thing flying va                   
    ##  [7]  yes nearly every time fly vx ear worm wont go away                             
    ##  [8]  really missed prime opportunity men without hats parody                        
    ##  [9] virginamerica well didntbut now d                                               
    ## [10]  amazing and arrived hour early youre good                                      
    ## [11]  know suicide is second leading cause death among teens                         
    ## [12]  lt pretty graphics much better minimal iconography d                           
    ## [13]  is great deal already thinking nd trip australia havent even gone st trip yet p
    ## [14]  virginmedia im flying fabulous seductive skies u take stress away travel       
    ## [15]

``` r
# Make a copy of the corpus
tweets_corpus_copy <- tweets_corpus
```

### Stemming

``` r
tweets_corpus <- tm_map(tweets_corpus,stemDocument)

# Stem Completion
tweets_corpus <- tm_map(tweets_corpus,content_transformer(stemCompletion), 
                        dictionary = tweets_corpus_copy)
```

### Create Term Document Martix

We convert the word corpus into a document matrix. The Document matrix can be analyzed to examine most frequently occurring words.

``` r
tweet_tdm <- TermDocumentMatrix(tweets_corpus,
                                control = list(wordLengths = c(1,Inf)))
tweet_tdm
```

    ## <<TermDocumentMatrix (terms: 209, documents: 14640)>>
    ## Non-/sparse entries: 489/3059271
    ## Sparsity           : 100%
    ## Maximal term length: 28
    ## Weighting          : term frequency (tf)

### Word Frequencies

We find the most frequent words and we create a Word Cloud of tweets using We are limiting the maximum words to 200 and plotting the top 10 frequent words using the ggplot package.

``` r
# Frequent Terms
freq.terms <- findFreqTerms(tweet_tdm, lowfreq = 4)
term.freq <- sort(rowSums(as.matrix(tweet_tdm)),decreasing = TRUE)
df <- data.frame(term = names(term.freq), freq = term.freq)


# Creating a word cloud of frequent term
wordcloud(words = df$term, freq = df$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
```

![](Airline_Sentiment_v2_files/figure-markdown_github-ascii_identifiers/Word%20Frequency-1.png)

``` r
# Plotting the top 10 frequent words
library(ggplot2)
ggplot(df[1:10,], aes(x=term, y=freq)) + geom_bar(stat="identity") +
xlab("Terms") + ylab("Count") + coord_flip() 
```

![](Airline_Sentiment_v2_files/figure-markdown_github-ascii_identifiers/Word%20Frequency-2.png)

#### Plot the frequency of the words on log scale .

Plotting the frequency of top 50 words in the logarithmic scale.

``` r
Freq_df <- df

Freq_df <- Freq_df %>% arrange(desc(freq))
# Word frequency on log scale
ggplot(head(Freq_df,50), aes(term, log10(freq))) + geom_point() +theme(axis.text.x=element_text(angle=45,hjust=1))
```

![](Airline_Sentiment_v2_files/figure-markdown_github-ascii_identifiers/Log%20of%20frequency%20plot-1.png)

#### Plotting Bigrams / Trigrams for word frequency

The initial exploration of the word analysis was helpful and we will construct bigrams and trigrams and plot the top 15 bigram and trigram on a logarithmic scale.Bigrams are two word phrases and trigrams are three word phrases. Recall that stop words had been removed so the phrases may look choppy.

``` r
library(dplyr)
#Bigram 
bigram_df <- df %>%
  unnest_tokens(bigram, term , token = "ngrams", n = 2)

bigram_df <- bigram_df %>% arrange(desc(freq))
head(bigram_df,10)
```

    ## # A tibble: 10 x 2
    ##     freq            bigram
    ##    <dbl>             <chr>
    ##  1    11        done great
    ##  2     4          suck yes
    ##  3     4          yes dmed
    ##  4     4   dmed definitely
    ##  5     4 definitely update
    ##  6     4 update appreciate
    ##  7     4   appreciate help
    ##  8     3    absolute delay
    ##  9     3        delay okay
    ## 10     3          okay got

``` r
#Bigram Plot
ggplot(head(bigram_df,15), aes(reorder(bigram,freq), log10(freq))) +
 geom_bar(stat = "identity") + coord_flip() +
 xlab("Bigrams") + ylab("Frequency") + ggtitle("Most frequent bigrams")
```

![](Airline_Sentiment_v2_files/figure-markdown_github-ascii_identifiers/Bigrams%20&%20Trigrams-1.png)

``` r
#Trigram
trigram_df <- df %>%  unnest_tokens(trigram, term , token = "ngrams", n = 3)

trigram_df <- trigram_df %>% arrange(desc(freq))
head(trigram_df,10)
```

    ## # A tibble: 10 x 2
    ##     freq                      trigram
    ##    <dbl>                        <chr>
    ##  1     4                suck yes dmed
    ##  2     4          yes dmed definitely
    ##  3     4       dmed definitely update
    ##  4     4 definitely update appreciate
    ##  5     4       update appreciate help
    ##  6     3          absolute delay okay
    ##  7     3               delay okay got
    ##  8     3                okay got thnx
    ##  9     3                got thnx made
    ## 10     3               thnx made good

``` r
# Trigram Plot 
ggplot(head(trigram_df,15), aes(reorder(trigram,freq), log10(freq))) +
  geom_bar(stat = "identity") + coord_flip() +
  xlab("Trigrams") + ylab("Frequency") +
  ggtitle("Most frequent Trigram")
```

![](Airline_Sentiment_v2_files/figure-markdown_github-ascii_identifiers/Bigrams%20&%20Trigrams-2.png)

### Tweets by Airlines

We will analyze the total number of tweets for each airlines.

``` r
ggplot(twitter_airline, aes(x= airline)) + geom_bar(aes(y=..count.., fill = airline))+geom_text(stat='count',aes(label=..count..),vjust=-0.2)+
  scale_fill_brewer(palette="Dark2") + xlab("Airlines") + ylab("Tweets Count")
```

![](Airline_Sentiment_v2_files/figure-markdown_github-ascii_identifiers/Tweets%20Count-1.png)

United Airlines has the most tweets and Virgin America has the least tweets. Having higher number of tweets can either be because of their popularty or they might have lot of issues which needs to be investigated further.

### Sentiments

Let us know look at the sentiments of the tweets for each airlines. It helps in identifying the positive, negative and nuetral sentiment of the tweets for each airlines

### Retrieve Data for Delta airline

``` r
delta <- subset(twitter_airline,airline == "Delta")
delta_txt  <- delta$text
delta_sentiment <- sentiment(delta_txt)
delta_sentiment$score <- 0
delta_sentiment$score[delta_sentiment$polarity == "positive"] <- 1
delta_sentiment$score[delta_sentiment$polarity == "negative"] <- -1
delta_table <- table(delta_sentiment$polarity)
ggplot(delta_sentiment, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +geom_text(stat='count',
  aes(label=..count..),vjust=-0.2)+
  scale_fill_brewer(palette="Dark2") +
 labs(x="Polarity", y="Number of Tweets") +
  ggtitle("Twitter Sentiment Analysis of Delta Airlines")+
  theme(plot.title = element_text(hjust = 0.5))
```

![](Airline_Sentiment_v2_files/figure-markdown_github-ascii_identifiers/Delta%20Sentiment-1.png)

``` r
delta_sentiment$airline = 'Delta'
delta_sentiment$code = 'DL'
colnames(delta_sentiment)
```

    ## [1] "text"     "polarity" "language" "score"    "airline"  "code"

Delta has most of the tweets that are nuetral. But the negative tweets are more than the positive tweets and requires inspection. \#\#\# Retrieve Data for American airline

``` r
american <- subset(twitter_airline,airline == "American")
american_txt  <- american$text
american_sentiment <- sentiment(american_txt)
american_sentiment$score <- 0
american_sentiment$score[american_sentiment$polarity == "positive"] <- 1
american_sentiment$score[american_sentiment$polarity == "negative"] <- -1
american_table <- table(american_sentiment$polarity)
ggplot(american_sentiment, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +geom_text(stat='count',
   aes(label=..count..),vjust=-0.2)+
  scale_fill_brewer(palette="Dark2") +
  labs(x="Polarity", y="Number of Tweets") +
  ggtitle("Twitter Sentiment Analysis of American Airlines")+
  theme(plot.title = element_text(hjust = 0.5))
```

![](Airline_Sentiment_v2_files/figure-markdown_github-ascii_identifiers/American%20Airlines%20Sentiment-1.png)

``` r
american_sentiment$airline = 'American'
american_sentiment$code = 'AA'
colnames(american_sentiment)
```

    ## [1] "text"     "polarity" "language" "score"    "airline"  "code"

AA tweets indicate that their negative tweets are almost equal to the neutral tweets. The reason behind the negative tweets must be explored.

### Retrieve Data for United airline

``` r
united <- subset(twitter_airline,airline == "United")
united_txt  <- united$text
united_sentiment <- sentiment(united_txt)
united_sentiment$score <- 0
united_sentiment$score[united_sentiment$polarity == "positive"] <- 1
united_sentiment$score[united_sentiment$polarity == "negative"] <- -1
united_table <- table(united_sentiment$polarity)
ggplot(united_sentiment, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +geom_text(stat='count',
  aes(label=..count..),vjust=-0.2)+
  scale_fill_brewer(palette="Dark2") +
  labs(x="Polarity", y="Number of Tweets") +
  ggtitle("Twitter Sentiment Analysis of United Airlines")+
  theme(plot.title = element_text(hjust = 0.5))
```

![](Airline_Sentiment_v2_files/figure-markdown_github-ascii_identifiers/United%20Airlines%20Sentiment-1.png)

``` r
united_sentiment$airline = 'United'
united_sentiment$code = 'UA'
colnames(united_sentiment)
```

    ## [1] "text"     "polarity" "language" "score"    "airline"  "code"

United airlines tweets indicate that their negative tweets are almost equal to the neutral tweets. The reason behind the negative tweets must be explored.

### Retrieve Data for Southwest airline

``` r
southwest <- subset(twitter_airline,airline == "Southwest")
southwest_txt  <- southwest$text
southwest_sentiment <- sentiment(southwest_txt)
southwest_sentiment$score <- 0
southwest_sentiment$score[southwest_sentiment$polarity == "positive"] <- 1
southwest_sentiment$score[southwest_sentiment$polarity == "negative"] <- -1
southwest_table <- table(southwest_sentiment$polarity)
ggplot(southwest_sentiment, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +geom_text(stat='count',
  aes(label=..count..),vjust=-0.2)+
  scale_fill_brewer(palette="Dark2") +
  labs(x="Polarity", y="Number of Tweets") +
  ggtitle("Twitter Sentiment Analysis of Southwest Airlines")+
  theme(plot.title = element_text(hjust = 0.5))
```

![](Airline_Sentiment_v2_files/figure-markdown_github-ascii_identifiers/Southwest%20Airlines%20Sentiment-1.png)

``` r
southwest_sentiment$airline = 'Southwest'
southwest_sentiment$code = 'SW'
colnames(southwest_sentiment)
```

    ## [1] "text"     "polarity" "language" "score"    "airline"  "code"

United airlines tweets indicate that they have almost double the amount of neutral tweets as compared to the negative tweets.

### Retrieve Data for US Airways

``` r
us_airways <- subset(twitter_airline,airline == "US Airways")
us_airways_txt  <- us_airways$text
us_airways_sentiment <- sentiment(us_airways_txt)
us_airways_sentiment$score <- 0
us_airways_sentiment$score[us_airways_sentiment$polarity == "positive"] <- 1
us_airways_sentiment$score[us_airways_sentiment$polarity == "negative"] <- -1
us_airways_table <- table(us_airways_sentiment$polarity)
ggplot(us_airways_sentiment, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +geom_text(stat='count',
  aes(label=..count..),vjust=-0.2)+
  scale_fill_brewer(palette="Dark2") +
  labs(x="Polarity", y="Number of Tweets") +
  ggtitle("Twitter Sentiment Analysis of Us_airways Airlines")+
  theme(plot.title = element_text(hjust = 0.5))
```

![](Airline_Sentiment_v2_files/figure-markdown_github-ascii_identifiers/US%20Airways%20Sentiment-1.png)

``` r
us_airways_sentiment$airline = 'US Airways'
us_airways_sentiment$code = 'UW'
colnames(us_airways_sentiment)
```

    ## [1] "text"     "polarity" "language" "score"    "airline"  "code"

US Airways tweets indicate that they have almost equal amount of neutral and negative tweets. The reason behind the negative tweets must be investigated further.

### Retrieve Data for Virgin America

``` r
VA <- subset(twitter_airline,airline == "Virgin America")
VA_txt  <- VA$text
VA_sentiment <- sentiment(VA_txt)
VA_sentiment$score <- 0
VA_sentiment$score[VA_sentiment$polarity == "positive"] <- 1
VA_sentiment$score[VA_sentiment$polarity == "negative"] <- -1
VA_table <- table(VA_sentiment$polarity)
ggplot(VA_sentiment, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +geom_text(stat='count',
    aes(label=..count..),vjust=-0.2)+
  scale_fill_brewer(palette="Dark2") +
  labs(x="Polarity", y="Number of Tweets") +
  ggtitle("Twitter Sentiment Analysis of Virgin America ")+
  theme(plot.title = element_text(hjust = 0.5))
```

![](Airline_Sentiment_v2_files/figure-markdown_github-ascii_identifiers/Virgin%20America%20Sentiment-1.png)

``` r
VA_sentiment$airline = 'Virgin America'
VA_sentiment$code = 'VA'
colnames(VA_sentiment)
```

    ## [1] "text"     "polarity" "language" "score"    "airline"  "code"

Virgin America tweets indicate that they have more neutral tweets

### Combining data from all airlines

``` r
all_sentiment <- rbind(delta_sentiment,american_sentiment,united_sentiment,
    southwest_sentiment,VA_sentiment,us_airways_sentiment)
all_sentiment$score <- 0
 all_sentiment$score[all_sentiment$polarity == "positive"] <- 1
 all_sentiment$score[all_sentiment$polarity == "negative"] <- -1

 #Plot by sentiment for all airlines
 
ggplot(all_sentiment, aes(x=airline,position = 'fill')) + 
  geom_bar(aes(y=..count.., fill=polarity), position = "fill")  + 
  scale_fill_brewer(palette="Dark2") +
 labs(x="Polarity", y="Number of Tweets") +
 ggtitle("Twitter Sentiment Analysis of US based airlines ")+
  theme(plot.title = element_text(hjust = 0.5)) 
```

![](Airline_Sentiment_v2_files/figure-markdown_github-ascii_identifiers/Overall%20Sentiment-1.png)

``` r
# Plot by Location 
all_airlines <- rbind(delta,american,united,us_airways,VA,southwest)
```

The 100% stacked bar indicate that American, United and US Airways have the most negative tweets as compared to other airlines. It is important now to understand the reason behind the negative tweets

Bigrams of Sentiments
=====================

We will construct bigrams of the setniment and plot them on a logarithmic scale. We will check whether the bigram is present in the tweet

``` r
library(stringr)
# Creating multiple variable of text variable for bigram and trigram to split 
all_sentiment$text2 <- all_sentiment$text
all_sentiment$tweet <- all_sentiment$text

all_sentiment$tweet <- str_to_lower(all_sentiment$tweet)

# Creating Bigram of sentiments
all_sentiment <- all_sentiment %>% 
  unnest_tokens(bigram,text,token="ngrams", n=2)
#head(all_sentiment$bigram)
all_sentiment$rank1 <- order(all_sentiment$bigram)


# Bigram Plot 
ggplot(head(all_sentiment,15), aes(reorder(bigram,rank1), log10(rank1))) +
  geom_bar(stat = "identity") + coord_flip() +
  xlab("Bigrams") + ylab("Rank") +
  ggtitle("Most frequent Bigrams")
```

![](Airline_Sentiment_v2_files/figure-markdown_github-ascii_identifiers/Sentiment%20Bigrams-1.png)

``` r
# Checking if bigram is available in the tweet 
all_sentiment$Is_Bigram <-str_detect(all_sentiment$tweet, all_sentiment$bigram)
```

Trigrams of Sentiments
======================

We will construct trigram of the setniment and plot them on a logarithmic scale. We will check whether the trigram is present in the tweet

``` r
# Creating trigram of sentiments
all_sentiment <- all_sentiment %>% 
  unnest_tokens(trigram, text2 , token = "ngrams", n = 3)

all_sentiment$rank2 <- order(all_sentiment$trigram)


# Trigram Plot 
ggplot(head(all_sentiment,15), aes(reorder(trigram,rank2), log10(rank2))) +
  geom_bar(stat = "identity") + coord_flip() +
  xlab("Trigrams") + ylab("Rank") +
  ggtitle("Most frequent Trigrams")
```

![](Airline_Sentiment_v2_files/figure-markdown_github-ascii_identifiers/Trigram%20Sentiment-1.png)

``` r
# Checking if bigram is available in the tweet 
all_sentiment$Is_Trigram <-str_detect(all_sentiment$tweet,all_sentiment$trigram)
```

#### Analysis of Variance of the sentiments

Creating a numeric variable for polarity. polarity\_score is -1 for negative sentiment, 0 for neutral and 1 for positive sentiment

``` r
sentiment_variance <- subset(all_sentiment, polarity != "neutral")

# Creating polarity_score as numeric
sentiment_variance <- all_sentiment %>%  
  mutate(polarity_score =  ifelse(polarity == "negative",0,1))

# Converting the Is_Bigram and Is_Trigram to numeric 

sentiment_variance$Is_Bigram <- as.numeric(sentiment_variance$Is_Bigram)
sentiment_variance$Is_Trigram <- as.numeric(sentiment_variance$Is_Trigram)

# Box plot of the polarity score for airlines.
ggplot(sentiment_variance, aes(x = airline, y = polarity_score)) +
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete() + xlab("Airline") +
  ylab("Polarity Score")
```

![](Airline_Sentiment_v2_files/figure-markdown_github-ascii_identifiers/Sentiment%20Variance-1.png)

``` r
# Calculating Analysis of Variance with airlines as predictor variable 


Myglm <- glm(polarity_score ~ airline + Is_Trigram + Is_Bigram  ,
             data = sentiment_variance, family = binomial)
summary(Myglm)
```

    ## 
    ## Call:
    ## glm(formula = polarity_score ~ airline + Is_Trigram + Is_Bigram, 
    ##     family = binomial, data = sentiment_variance)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6531  -1.1438   0.8668   1.1668   1.2985  
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error  z value Pr(>|z|)    
    ## (Intercept)            0.387957   0.003549  109.319  < 2e-16 ***
    ## airlineDelta           0.635082   0.003439  184.682  < 2e-16 ***
    ## airlineSouthwest       0.476437   0.003270  145.711  < 2e-16 ***
    ## airlineUnited          0.182073   0.002794   65.165  < 2e-16 ***
    ## airlineUS Airways     -0.018418   0.002950   -6.243 4.28e-10 ***
    ## airlineVirgin America  0.683991   0.006268  109.125  < 2e-16 ***
    ## Is_Trigram            -0.286722   0.002280 -125.739  < 2e-16 ***
    ## Is_Bigram             -0.362969   0.002802 -129.555  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 6207457  on 4482298  degrees of freedom
    ## Residual deviance: 6097418  on 4482291  degrees of freedom
    ## AIC: 6097434
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
print(anova(Myglm, test="Chisq"))
```

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: polarity_score
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##            Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                     4482298    6207457              
    ## airline     5    72616   4482293    6134841 < 2.2e-16 ***
    ## Is_Trigram  1    20406   4482292    6114434 < 2.2e-16 ***
    ## Is_Bigram   1    17016   4482291    6097418 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
