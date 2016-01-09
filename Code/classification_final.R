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
posNegWords <- data.frame(posNegWords)

#LOAD TRAINING SET
rawTrainingData <- read.csv("~/TUE/Quartile1/IRandDM/SentimentAnalysis/WebIR-Full/Data/SemEvalProcessed.csv", sep = ",", quote = '\"')

#VECTOR OF THE TWEETS
trainingVector <- VectorSource(rawTrainingData$Text)

#BUILD THE CORPUS
myCorpus <- Corpus(trainingVector)

#PRE PROCESSING TASKS
myCorpus <- tm_map(myCorpus,
                   content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')),
                   mc.cores=1)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL), lazy = T)
myCorpus <- tm_map(myCorpus, content_transformer(removeReference), lazy = T)
myCorpus <- tm_map(myCorpus, content_transformer(removeHashTag), lazy = T)
myCorpus <- tm_map(myCorpus, removePunctuation, lazy = T)
myCorpus <- tm_map(myCorpus, stripWhitespace, lazy = T)
myCorpus <- tm_map(myCorpus, removeWords, stopwords("en"), lazy = T)
myCorpus <- tm_map(myCorpus, removeWords, '#\\S+\\s*|@\\S+\\s*|http\\S+\\s*', lazy = T)
myCorpus <- tm_map(myCorpus, content_transformer(tolower), lazy = T)

#DOCUMENT-TERM MATRIX
dtm <- DocumentTermMatrix(myCorpus)

#EXTRACT MOST FREQUENT TEMRS
mostFrequentTerms <- getTermFrequencyList(trainingVector)

#FILTER the terms in the DTM
dtmFromMostFrequentTerms <- dtm[, colnames(dtm)%in%mostFrequentTerms]
matrixFromMostFrequentTerms <- as.matrix(dtmFromMostFrequentTerms)

data <- data.frame(matrixFromMostFrequentTerms, "Final.Sentiment"=as.factor(rawTrainingData$Sentiment))

#training and testing
dataTrain <- data.frame(rbind(data[1:8000,], data[11001:13583,]))
dataTest <- data.frame(rbind(data[8001:11000,]))

library(e1071)
#NAIVE BAYES
model = naiveBayes(as.factor(dataTrain$Final.Sentiment)~., data = dataTrain)
pred <-predict(model, dataTest, type = "class", threshold = 0.05)

confusionMatrix = table(pred, as.factor(dataTest$Final.Sentiment))
accuracy.nb <- sum(diag(confusionMatrix))/nrow(dataTest) * 100
sprintf("The accuracy of the model using naive bayes is %0.2f", accuracy.nb)
confusionMatrix
