rm(list= ls()[!(ls() %in% c('top_frequency_terms_merged', 'model'))])
rm(list = ls(all = TRUE))
library(tm)
library(e1071)

rawTrainingData <- read.csv("~/TUE/Quartile1/IRandDM/SentimentAnalysis/WebIR-Full/Data/SemEvalWithoutNeutral.csv", sep = ",")
rawTrainingData <- read.csv("~/TUE/Quartile1/IRandDM/SentimentAnalysis/WebIR-Full/Data/cameron-tweet-sorted.csv", sep = ",", quote = '\"')
rawTrainingData <- rawTrainingData[1:5000,]

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

#Pre-processing methods
removeURL <- function(x)
  gsub('http\\S+\\s*',' ', x)
removeHashTag <- function(x)
  gsub('#\\S+\\s*',' ', x)
removeReference <- function(x)
  gsub('@\\S+', ' ', x)
removeShortWords <- function(x) gsub("\\b[a-zA-Z0-9]{1,2}\\b", " ", x)

stopwords <- scan("~/TUE/Quartile1/IRandDM/SentimentAnalysis/WebIR-Full/Data/stopwords.txt", what = 'character')
k <- length(rawTrainingData$Text)
textList <- list()
for(i in 1: k){
  #text <- as.String(rawTrainingData$Text[i])
  text <- rawTrainingData$Text[i]
  text <- iconv(text, to = 'utf-8', sub = '')
  text <- tolower(text)
  text <- removeHashTag(text)
  text <- removeReference(text)
  text <- removePunctuation(text)
  text <- removeShortWords(text)
  text <- removeURL(text)
  text <- removeWords(text, stopwords)
  
  #Stemming
  text <- lapply(strsplit(text, " "), stemDocument, language = "english") [[1]]
  newText <- ""
  for (word in text) {
    newText <- paste(newText, word)
  }
  text <- newText
  text <- stripWhitespace(text)
  
  textList[i] <- text
}

##combine sentiment and tweet columns after preprocessing
text.df <- data.frame(matrix(unlist(textList), nrow = k, byrow = T))
processedTrainingData <- data.frame(rawTrainingData$Sentiment, text.df$matrix.unlist.textList...nrow...k..byrow...T.)
processedTrainingData <- text.df

colnames(processedTrainingData) <- c("Sentiment","Text")
colnames(processedTrainingData) <- c("Text")

#remove variables
rm(text.df, text, textList, i)
processedTrainingData1 <- as.data.frame(processedTrainingData[8001:16820,])
colnames(processedTrainingData1) <- c("Text")
  
#VECTOR OF THE TWEETS
trainingVector <- VectorSource(processedTrainingData1$Text)
  
myCorpus <- Corpus(trainingVector)

bigramTokenizer <-
 function(x) {
   unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
 }
bigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 2))}
dtm <- DocumentTermMatrix(myCorpus, control = list(tokenizer = bigramTokenizer))
dtm <- DocumentTermMatrix(myCorpus)


#FILTER the terms in the DTM
mostFrequentBigrams <- getBigramFrequencyList(trainingVector, 15)
mostFrequentTerms <- getTermFrequencyList(trainingVector, 30)
dtm_frequency_terms <- dtm[, colnames(dtm)%in%c(mostFrequentTerms, mostFrequentBigrams)]
dtm_frequency_terms <- dtm[, colnames(dtm)%in%c(mostFrequentTerms, posNegWords)]
dtm2 <- as.matrix(dtm_frequency_terms)
data <- data.frame(dtm2, "sentiment"=as.factor(rawTrainingData$Sentiment))

#training and testing
data_train <- data.frame(rbind(data[1:3000,], data[3700:5000,]))
data_test <- data.frame(rbind(data[3001:3699,]))

model = naiveBayes(as.factor(data_train$sentiment)~., data = data_train, laplace = 0.03)
pred <- predict(model, data_test, type = "class", threshold = 0.03, esp = 0)
pred <- predict(model, dtm2, type = "raw", threshold = 0.03, esp = 0)

pred_prob <- predict(model, data_test, type = "raw", threshold = 0.03, eps = 0)
pred.df <- as.data.frame(pred)

sent <- list()
for (i in 1:8820) {
  if (pred.df$positive[i] == 1) {
    sent[i] <- "positive"
  } else if (pred.df$negative[i] > 0.5){
    sent[i] <- "negative"
  } else {
    sent[i] <- "neutral"
  }
}
sent <- unlist(sent)

sent.df <- data.frame(unlist(sent))
summary(sent.df$unlist.sent.)

library(ROCR)
prediction <- prediction(pred.df$positive, data_test$Final.Sentiment, label.ordering = NULL)
perf <- performance(prediction, "tpr", "fpr")
plot(perf)
#Classification table with test data
conf.nb=table(pred, as.factor(data_test$sentiment))
accuracy.nb <- sum(diag(conf.nb))/nrow(data_test) * 100
sprintf("The accuracy of the model using naive bayes is %0.2f", accuracy.nb)
conf.nb

list <- list()
probs <- list()
for (i in 1:ncol(sample)) {
  if (as.numeric(sample[1,i]) != 0) {
    list[length(list) + 1] <- colnames(sample) [i]
    probs[length(probs) + 1] <- sample[1,i]
  }
}

testingText <- data.frame(rbind(rawTrainingData[3001:3699,]))
result <- cbind(testingText, pred)
time <- timestamp()
write.csv(result, paste("~/Desktop/result" , as.String(time) ,".csv"), quote = T)

library(caret)
# define training control
train_control <- trainControl(method="cv", number=10)
# train the model 
model <- train(Final.Sentiment~., data=data, trControl=train_control, method="nb")
# make predictions
predictions <- predict(model, data)
# summarize results
confusionMatrix(predictions, data$Final.Sentiment)
