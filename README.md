# An Experiment on using classical ML techniques in analyzing sentiments of Twitter Data

This repo is the source code of our experiment, which develop some machine learning mechanisms to classify the tweets into positive or negative ones. It is crucial to evaluate the models carefully before using them. We address the problem by training the classifiers by a published labeled Twitter dataset, using two different methods.
We would also explore Topic Modelling and Clustering to get deeper insights to figure out the relevant topics that are most discussed in public.

## Dataset
### Classification Training
We use the dataset in SemEval 2014, which contains 13583 labeled tweets to train the classification models. They are stored as ".csv" files in foler ./Data.

* SemEvalProcessed.csv: The whole dataset, which are manually classified into "positive", "neutral", and "negative".
* SemEvalWithoutNeutral.csv: The sampled dataset, in which only "positive" and "negative" tweets are kept. We use this one as the main training data for the experiments.
* SemEval7000.csv: 7000 "positive" and "negative" tweets are randomly sampled. We use it to train the model with complex feature (e.g. Bigrams, Mixture model), as our hardware is limited.
* SemEvalHalfBalance.csv: 5000 "positive" and "negative" tweets are randomly sampled.

### Dataset for Candidates
We use the Java program, in "SearchTwitter" folder, to fetch the Twitter data via RESTful Twitter Apis. We collect 2 separated dataset for 2 candidates - Ed Miliband and David Cameron. The names of the candidates are the fetching queries.

* tweets_CAM.csv: All tweets containing "David Cameron" from April to May, 2015 
* Milliband_Tweet.csv: All tweets containing "Ed Miliband" from April to May, 2015 
## Experiment Script
All the codes are in the folder "./Code". These are the important files:

* classification.R: The script for training and testing classification (e.g. Naive Bayes). These following lines need to be modified depending on the context.

Change the path to your training data.
```
rawTrainingData <- read.csv("~/TUE/Quartile1/IRandDM/SentimentAnalysis/WebIR-Full/Data/SemEvalProcessed.csv", sep = ",")
```

Change the "~/TUE/Quartile1/IRandDM/SentimentAnalysis" path to the folder containing the repo.
```
pos = scan(
  "~/TUE/Quartile1/IRandDM/SentimentAnalysis/WebIR-Full/tweetsPreProcessing/data/positive-words.txt", what = 'character', comment.char = ';'
)
neg = scan(
  "~/TUE/Quartile1/IRandDM/SentimentAnalysis/WebIR-Full/tweetsPreProcessing/data/negative-words.txt", what = 'character', comment.char = ';'
)
```

Change the path you want to save the file to
```
write.csv(result, paste("~/Desktop/result" , as.String(time) ,".csv"), quote = T)
```

* crossValidationScript.R: Our manual script for cross validation. Just compile everything in this file, and call the "performClassification" function to run the cross-validation. Besides, 2 functions "getTermFrequencyList" and "getBigramFrequencyList" may be also helpful in other contexts.

For example, the below code is for cross-validation of Naive Bayes, with mixture language model, filtering by Hu&Liu Corpus, stemming and stopword removal:
```
model <- performClassification(rawTrainingData =  rawTrainingData, method =  "NB", isCrossValidation = T, lm =  "mix", feature =  "huliu", isStem = T, isStopWord = T)
```

* topicModeling.R: the function and sample script to do the topic modeling. To run topic modeling, please build the function "performTopicModeling" inside this script. And call it.

Here is an example, where we would like to perform the topic modeling on the miliband data, removing several meaningless terms, and setting 7 topics and 4 terms for each:

```
miliband <- read.csv("~/TUE/Quartile1/IRandDM/SentimentAnalysis/WebIR-Full/Data/Milliband_Tweet.csv", sep = ",")
removeWordVector = c("david", "cameron", "camerons", "miliband", 
                     "will", "can", "ge2015", "david_cameron", "ed_miliband",
                     "the", "davidcameron", "edmiliband",
                     "2015", "tory", "tories", "snp", "via", "don", "just", "com",
                     "election")
performTopicModeling(data = miliband, 
                     removeWordVector = removeWordVector,
                     nTopics = 7,
                     nTerms = 4)

```

* clustering_final.R: the script for doing Hierarhical Clustering on our dataset. Please build the function "getHCDendogram" and call it.

For example, the below script builds the HC Dendogram for the cameron dataset, after removing sparse terms with 0.98 threshold, and removing several meaningless words.

```
cameron <- read.csv("~/TUE/Quartile1/IRandDM/SentimentAnalysis/WebIR-Full/Data/tweets_CAM.csv", sep = ",", quote = '\"')
removeWordVector = c("david", "cameron", "camerons", "miliband", 
                     "will", "can", "ge2015", "david_cameron", "ed_miliband",
                     "the", "davidcameron", "edmiliband",
                     "2015", "tory", "tories", "snp", "via", "don", "just", "com")
getHCDendogram(cameron, 0.98, removeWordVector)
```

