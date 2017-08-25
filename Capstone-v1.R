data_dir <- "https://github.com/prashijp/Capstone-Project" 
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
which(apply(tweet_tdm,1,sum)>5)
freq.terms <- findFreqTerms(tweet_tdm,lowfreq = 5)
term.freq <- rowSums(as.matrix(tweet_tdm))
tweets_df <- data.frame(term = names(term.freq), freq = term.freq)
