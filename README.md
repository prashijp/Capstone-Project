# Objective: 
Comparing US based airlines performance using twitter sentimental analysis. The sentiment analysis will be performed on each of the airlines seperately and also on the entire dataset. 

# Project Problem:
Analyzing the US based airlines performances by analyzing the the tweets of the airlines.Identifying the sentiments of tweets and classifying them as nuetral,negative and positive sentiment for each airlines. Then tokenizing the tweets as unigrams,bigrams and trigrams and finding their significance on the sentiments. 

# Dataset: 
Data is obtained from the airline sentiment dataset from Kaggle https://www.kaggle.com/crowdflower/twitter-airline-sentiment

# The process followed for Data Cleaning 
1. Load data to R studio 
2. Replace twitter handle with blank and filter the dataset by airlines
3. Build a corpus, and specify the source to be character vectors
4. Convert the corpus to lowercase 
5. Remove punctuations from corpus
6. Remove URLs
7. Remove anything expect English and Space
8. Remove Stopwords
9. Remove extra whitespace
10.Make a copy of the corpus
11.Converting corpus to dataframe 

# Term Document Matrix 
12. Create a term document matrix

# Word Frequencies
13. Find Frequent Terms
16. Creating a word cloud of frequent term
17. Plotting the top 10 frequent words
18. Plot the frequency of the words on linear / log scale 
19. Plotting Bigrams on words frequency
20. Plotting trigram on word frequency

# Sentiments
21. Retrieve data for the chosen airlines
22. Perform sentiment analysis on the tweets of the chosen airlines

# Document Term Matrix
23. Creating a Vcorpus 
24. Crating a Unigram,Bigram and Trigram DTM from Vcorpus

# Preparing the model 
25. Joining the unigram, bigram, trigram dataframe with sentiments dataframe. 
26. Mapping the tweet reason to check its significance on the sentiment
27. Mapping the tweet location to check its significance on the sentiment

# Unigram/Bigram/Trigram Modeling
26. Creating a dataframe with tweetreason, timezone,sentiment frequency, sentiments and unigrams/bigrams/trigrams.
29. Performing logistic regression for each of unigram/bigram/trigram wth positive/negative/neutral sentiments 
    with the variable tweetreason.

# Splitting the dataset
30. Splitting the dataset into training and testing dataset in 70:30 ratio
31. Apllying the balancing algorithm for balancing the unbalanced dataset. 
32. Mapping both the test and training dataset 

# Cross Validation and Bootstrappping 
33. Perfoming CV  and bootsrapping for logisitic regression for the variable tweetreason for all the nine models
34. Plotting the ROC curve for logistic regression
35. Performing CV for NaiveBayes for the variable tweet reason
36. Performing CV for NaiveBayes for the variable tweet reason and sentiment frequency 
37. Plotting the ROC curve for NaiveBayes 
38. Performing CV for NaiveBayes for the entire training dataset excluding timezone as it is insignifacnt 
39. Plotting ROC Curves and Precision Recall Curves for Entire training dataset  
40. Performing CV for NaiveBayes for the entire testing dataset 
41. Plotting ROC Curves and Precision Recall Curves for Entire testing dataset 

# Observation

We have found that the unigram/bigram/trigram positive performed well for all the airlines with ROC value greater than 0.5
on the training dataset. 

For the test datset, unigram sentiments were not significant while bigram positive performed well while bigram/trigram negative perfomed poorly. Those model that were not fitting were excluded from the observation and has been commented out in the appendix.

# Limitation 

Eventhough NaiveBayes and Logistics Regression has been applied for looking at the significance of unigram/bigram/trigram on the sentiments, multinomial regression could have provided better insights on the signficance of the variables. 



