rm(list = ls(all = TRUE))
#LOAD TRAINING SET
rawTrainingData <- read.csv("C:/Users/LENOVO/Desktop/TUE Lectures/Q1/WEB IR/WebIR-Full-master/Data/SemEvalProcessed.csv", sep = ",", quote = '\"')
rawTrainingData <- read.csv("~/TUE/Quartile1/IRandDM/SentimentAnalysis/WebIR-Full/Data/SemEvalWithoutNeutral.csv", sep = ",", quote = '\"')
rawTrainingData <- read.csv("~/TUE/Quartile1/IRandDM/SentimentAnalysis/WebIR-Full/Data/standford-sample.csv", sep = ",", quote = '\"')
summary(rawTrainingData$Sentiment)
  
model <- performClassification(rawTrainingData =  rawTrainingData, method =  "NB", isCrossValidation = T, lm =  "unigram", feature =  "huliu")
model <- performClassification(rawTrainingData =  rawTrainingData, method =  "NB", isCrossValidation = T, lm =  "unigram", feature =  "MFT")
model <- performClassification(rawTrainingData =  rawTrainingData, method =  "NB", isCrossValidation = T, lm =  "bigram", feature =  "MFT")
model <- performClassification(rawTrainingData =  rawTrainingData, method =  "NB", isCrossValidation = T, lm =  "bigram", feature =  "huliu")
model <- performClassification(rawTrainingData =  rawTrainingData, method =  "NB", isCrossValidation = T, lm =  "unigram", feature =  "MFT")
