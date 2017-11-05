# Objective: 
Comparing US based airlines performance using twitter sentimental analysis

# Project Problem:
Analyzing the US based airlines performances by analyzing the the tweets of the airlines.Identifying the sentiments of tweets and classifying them as nuetral,negative and positive sentiment for each airlines. Identify the issues  behind negative sentiments and checking the significance of bigrams, trigrams and airlines on the sentiment

# Dataset: 
Data is obtained from the airline sentiment dataset from Kaggle https://www.kaggle.com/crowdflower/twitter-airline-sentiment

# The process followed for Data Cleaning 
1. Load data to R studio
2. Replace twitter handle with blank 
3. Convert the table into dataframe 
4. Build a corpus, and specify the source to be character vectors
5. Convert the corpus to lowercase 
6. Remove punctuations from corpus
7. Remove URLs
8. Remove anything expect English and Space
9. Remove Stopwords
10.Remove extra whitespace
11.Make a copy of the corpus

# Stemming
12. Stem Completion

# Term Document Matrix 
14. Create a term document matrix

# Word Frequencies
15. Find Frequent Terms
16. Creating a word cloud of frequent term
17. Plotting the top 10 frequent words
18. Plot the frequency of the words on linear / log scale 
19. Plotting Bigrams on words frequency
20. Plotting trigram on word frequency

# Tweets by Airlines
21. Retrieve Data for Delta airline
22. Retrieve Data for American airline
23. Retrieve Data for United airline
24. Retrieve Data for Southwest airline
25. Retrieve Data for US Airways
26. Retrieve Data for Virgin America
27. Combining data from all airlines
28. Negative tweets by Airlines
29. Reason behind the negative sentiment
30. Bigrams and Trigrams of Sentiments on log scale

# Analysis of Variance of the sentiments 
31. Creating polarity_score as numeric
32. Box plot of the polarity score for airlines
33. Linerar regression with airlines as predictor variable
