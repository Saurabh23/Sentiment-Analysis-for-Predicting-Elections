rm(list = ls(all = TRUE))
library(tm)

#LOADING pos and neg words
pos = scan(
  "~/TUE/Quartile1/IRandDM/SentimentAnalysis/WebIR-Full/tweetsPreProcessing/data/positive-words.txt", what = 'character', comment.char = ';'
)
neg = scan(
  "~/TUE/Quartile1/IRandDM/SentimentAnalysis/WebIR-Full/tweetsPreProcessing/data/negative-words.txt", what = 'character', comment.char = ';'
)

neg <- cbind(neg)
pos <- cbind(pos)

posNegWords <- cbind(pos)
posNegWords <- rbind(neg, pos)
rm(neg, pos)
posNegWords <- as.list(posNegWords)

##define constants
MOST_FREQ_TERMS <- 15
TRAIN_1 <- 1
TRAIN_2 <- 3000
TRAIN_3 <- 13001
TRAIN_4 <- 13583
TEST_1 <- 3001
TEST_2 <- 13000

#LOAD TRAINING SET
rawTrainingData <- read.csv("C:/Users/LENOVO/Desktop/TUE Lectures/Q1/WEB IR/WebIR-Full-master/Data/SemEvalProcessed.csv", sep = ",", quote = '\"')
rawTrainingData <- read.csv("~/TUE/Quartile1/IRandDM/SentimentAnalysis/WebIR-Full/Data/SemEvalWithoutNeutral.csv-processed.csv", sep = ",", quote = '\"')
rawTrainingData <- read.csv("~/TUE/Quartile1/IRandDM/SentimentAnalysis/WebIR-Full/Data/standford-sample.csv", sep = ",", quote = '\"')
summary(rawTrainingData$Sentiment)

#Pre-processing methods
removeURL <- function(x)
  gsub('http\\S+\\s*',' ', x)
removeHashTag <- function(x)
  gsub('#\\S+\\s*',' ', x)
removeReference <- function(x)
  gsub('@\\S+', ' ', x)
removeShortWords <- function(x) gsub("\\b[a-zA-Z0-9]{1,2}\\b", " ", x)

#START PREPROCESSING
k <- length(rawTrainingData$Text)
textList <- list()
for(i in 1: k){
  #text <- as.String(rawTrainingData$Text[i])
  text <- rawTrainingData$Text[i]
  text <- iconv(text, to = 'UTF-8', sub = 'byte')
  text <- tolower(text)
  text <- removeHashTag(text)
  text <- removeReference(text)
  text <- removeShortWords(text)
  text <- removeURL(text)
  text <- removePunctuation(text)
  text <- stripWhitespace(text)
  text <- removeWords(text, stopwords("en"))
  text <- stemDocument(text, language = "english")
  textList[i] <- text
}
##combine sentiment and tweet columns after preprocessing
text.df <- data.frame(matrix(unlist(textList), nrow = k, byrow = T))
processedTrainingData <- data.frame(rawTrainingData$Sentiment, text.df$matrix.unlist.textList...nrow...k..byrow...T.)

colnames(processedTrainingData) <- c("Sentiment","Text")

#remove variables
rm(text.df, text, textList, i)

#VECTOR OF THE TWEETS
trainingVector <- VectorSource(processedTrainingData$Text)
trainingVector

#BUILD THE CORPUS
myCorpus <- Corpus(trainingVector)
#myCorpusTemp <- Corpus(trainingVector)
#rm(myCorpusTemp)

#PREPROCESSING
#myCorpus <- tm_map(myCorpus,
#                   content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')),
#                   mc.cores=1)
#myCorpus <- tm_map(myCorpus, content_transformer(removeURL), lazy = T)
#myCorpus <- tm_map(myCorpus, content_transformer(removeReference), lazy = T)
#myCorpus <- tm_map(myCorpus, content_transformer(removeHashTag), lazy = T)
#myCorpus <- tm_map(myCorpus, removePunctuation, lazy = T)
#myCorpus <- tm_map(myCorpus, stripWhitespace, lazy = T)
#myCorpus <- tm_map(myCorpus, removeWords, stopwords("en"), lazy = T)
#myCorpus <- tm_map(myCorpus, removeWords, '#\\S+\\s*|@\\S+\\s*|http\\S+\\s*', lazy = T)
#myCorpus <- tm_map(myCorpus, content_transformer(tolower), lazy = T)
#Below words to be removed from most freq. list
#myCorpus <- tm_map(myCorpus, removeWords, '\\\\u002c|\\\\u2019s|\\\\u2019m|\\\\u2019ll', lazy = T)

myCorpus
myCorpus[[9]]$content

text <- rawTrainingData$Text[10]
bi <- ngrams(words(text), 2)

#DOCUMENT-TERM MATRIX
bigramTokenizer <-
  function(x) {
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
  }
dtm <- DocumentTermMatrix(myCorpus, control = list(weighting= weightTfIdf))

#bigram
dtm <- DocumentTermMatrix(myCorpus, control = list(weighting=weightTfIdf, tokenize=bigramTokenizer))
dtm.tf <- DocumentTermMatrix(myCorpus)
#####change weighting to tf-idf etc; currently its tf
dtm

#EXTRACT MOST FREQUENT TEMRS
getTermFrequencyList <- function(dtm, vectorSource, freq) {
  myCorpus <- Corpus(vectorSource)
  
  dtm <- DocumentTermMatrix(myCorpus)
  
  dtm2 <- as.matrix(dtm)
  frequency <- colSums(dtm2)
  frequency <- sort(frequency, decreasing = TRUE)
  head(frequency)
  
  #top_frequency_terms <- frequency[1:500]
  top_frequency_terms <- subset(frequency, frequency >= freq)
  top_frequency_terms <- as.list(rownames(as.matrix(top_frequency_terms)))
  
  return (top_frequency_terms)
}


mostFrequentTerms <- getTermFrequencyList(dtm, trainingVector, MOST_FREQ_TERMS)

getPosNegBigrams <- function(dtm) {
  colnames <- colnames(dtm)
  k <- length(colnames)
  termList <- list()
  for (i in 1:k){
    terms <- strsplit(colnames[i], " ")[[1]]
    if (grepl("\\d", terms[1]) || grepl("\\d", terms[2]) ) {
      next
    } else if (terms[1] %in% posNegWords || terms[2] %in% posNegWords) {
      termList[length(termList) + 1] <- colnames[i]
    }
  }
  
  return (termList)
}

termList <- getPosNegBigrams(dtm)

#FILTER the terms in the DTM
dtmFromMostFrequentTerms <- dtm[, colnames(dtm)%in%posNegWords]
dtmFromMostFrequentTerms <- dtmFromMostFrequentTerms[, colnames(dtmFromMostFrequentTerms)%in%mostFrequentTerms]
dtmFromMostFrequentTerms <- dtm[, colnames(dtm)%in%termList]
dtmFromMostFrequentTerms  ##Mostfreq words are 1662,but new filtered dtm shows 1383 terms?? 
matrixFromMostFrequentTerms <- as.matrix(dtmFromMostFrequentTerms)
##TBD: remove dates(as day year month (numbers)) from most frequent terms
##TBD: remove words like '\\u___'

View(matrixFromMostFrequentTerms)
data <- data.frame(matrixFromMostFrequentTerms, "Final.Sentiment"=as.factor(rawTrainingData$Sentiment))

#training and testing
dataTrain <- data.frame(rbind(data[2001:20000,]))
dataTest <- data.frame(rbind(data[1:2000,]))

dataTrain <- data.frame(rbind(data[1:2000,], data[4001:20000,]))
dataTest <- data.frame(rbind(data[2001:4000,]))

dataTrain <- data.frame(rbind(data[1:4000,], data[6001:20000,]))
dataTest <- data.frame(rbind(data[4001:6000,]))

dataTrain<- data.frame(rbind(data[1:6000,],data[8001:20000,]))
dataTest <- data.frame(rbind(data[6001:8000,]))

dataTrain <- data.frame(rbind(data[1:8000,], data[10001:20000,]))
dataTest <- data.frame(rbind(data[8001:10000,]))

dataTrain <- data.frame(rbind(data[1:10000,], data[12001:20000,]))
dataTest <- data.frame(rbind(data[10000:12001,]))

dataTrain <- data.frame(rbind(data[1:12000,], data[14001:20000,]))
dataTest <- data.frame(rbind(data[12000:14001,]))

dataTrain <- data.frame(rbind(data[1:14000,], data[16001:20000,]))
dataTest <- data.frame(rbind(data[14001:16001,]))


dataTrain <- data.frame(rbind(data[1:16000,], data[18001:20000,]))
dataTest <- data.frame(rbind(data[16001:18000,]))

dataTrain <- data.frame(rbind(data[1:18000,]))
dataTest <- data.frame(rbind(data[18001:20000,]))

dataTrain <- data.frame(rbind(data[1:2000,], data[4001:10400, ]))
dataTest <- data.frame(rbind(data[2000:4000,]))

#FOLD 1
dataTrain <- data.frame(rbind(data[3396:13583,]))
dataTest <- data.frame(rbind(data[1:3395,]))
#FOLD 2
dataTrain <- data.frame(rbind(data[1:3395,], data[6791:13583,]))
dataTest <- data.frame(rbind(data[3396:6790,]))
#FOLD 3
dataTrain <- data.frame(rbind(data[1:6790,], data[10187:13583,]))
dataTest <- data.frame(rbind(data[6791:10186,]))
#FOLD 4
dataTrain<- data.frame(rbind(data[1:10186,]))
dataTest <- data.frame(rbind(data[10187:13583,]))
require(caTools)
set.seed(101) 
sample = sample.split(data$Final.Sentiment, SplitRatio = .75)
dataTrain = subset(data, sample == TRUE)
dataTest = subset(data, sample == FALSE)

#Check training data
summary(dataTrain$Final.Sentiment)
dataTrainFiltered <- dataTrain[dataTrain$Final.Sentiment== "positive",]
dataTrainFiltered <- dataTrainFiltered[sample(nrow(dataTrainFiltered), 2250), ]
dataTrain <- rbind(dataTrain[dataTrain$Final.Sentiment != "positive", ], dataTrainFiltered)
dataTrain <- dataTrain[dataTrain$Final.Sentiment != "neutral"]

rm(matrixFromMostFrequentTerms,processedTrainingData,rawTrainingData)

library(e1071)
library(caret)
library(pROC)
#NAIVE BAYES
model = naiveBayes(as.factor(dataTrain$Final.Sentiment)~., data = dataTrain)
pred <- predict(model, dataTest, type = "class", threshold = 0.05)
pred <- attr(pred,"probabilities")[,c("SI")]

summary(dataTest$Final.Sentiment)
model$tables$love.lol
roc((dataTest$Final.Sentiment == 1)*1, pred)
confusionMatrix = table(pred, as.factor(dataTest$Final.Sentiment))
accuracy_nb <- sum(diag(confusionMatrix))/nrow(dataTest) * 100
sprintf("The accuracy of the model using naive bayes is %0.2f", accuracy_nb)
confusionMatrix

#Write result out
testingText <- data.frame(rbind(rawTrainingData[1:3395,]))
result <- cbind(testingText, pred)
write.csv(result, "~/TUE/Quartile1/IRandDM/SentimentAnalysis/WebIR-Full/Data/result_fold1.csv", quote = T)
word <- data.frame(model$tables$amazing)
model$tables$glad
  