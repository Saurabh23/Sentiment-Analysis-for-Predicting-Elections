rm(list = ls(all = TRUE))

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

#Pre-processing methods
removeURL <- function(x)
  gsub('http\\S+\\s*',' ', x)
removeHashTag <- function(x)
  gsub('#\\S+\\s*',' ', x)
removeReference <- function(x)
  gsub('@\\S+', ' ', x)
removeShortWords <- function(x) gsub("\\b[a-zA-Z0-9]{1,3}\\b", " ", x)

#START PREPROCESSING
k <- length(rawTrainingData$Text)
textList <- list()
for(i in 1: k){
  #text <- as.String(rawTrainingData$Text[i])
  text <- rawTrainingData$Text[i]
  text <- removeHashTag(text)
  text <- removeReference(text)
  text <- removeShortWords(text)
  text <- removeURL(text)
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
myCorpus <- tm_map(myCorpus,
                   content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')),
                   mc.cores=1)
#myCorpus <- tm_map(myCorpus, content_transformer(removeURL), lazy = T)
#myCorpus <- tm_map(myCorpus, content_transformer(removeReference), lazy = T)
#myCorpus <- tm_map(myCorpus, content_transformer(removeHashTag), lazy = T)
myCorpus <- tm_map(myCorpus, removePunctuation, lazy = T)
myCorpus <- tm_map(myCorpus, stripWhitespace, lazy = T)
myCorpus <- tm_map(myCorpus, removeWords, stopwords("en"), lazy = T)
myCorpus <- tm_map(myCorpus, removeWords, '#\\S+\\s*|@\\S+\\s*|http\\S+\\s*', lazy = T)
myCorpus <- tm_map(myCorpus, content_transformer(tolower), lazy = T)
#Below words to be removed from most freq. list
#myCorpus <- tm_map(myCorpus, removeWords, '\\\\u002c|\\\\u2019s|\\\\u2019m|\\\\u2019ll', lazy = T)

myCorpus
myCorpus[[9]]$content

#DOCUMENT-TERM MATRIX
dtm <- DocumentTermMatrix(myCorpus)
#####change weighting to tf-idf etc; currently its tf


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

#FILTER the terms in the DTM
dtmFromMostFrequentTerms <- dtm[, colnames(dtm)%in%mostFrequentTerms]
dtmFromMostFrequentTerms  ##Mostfreq words are 1662,but new filtered dtm shows 1383 terms?? 
matrixFromMostFrequentTerms <- as.matrix(dtmFromMostFrequentTerms)
##TBD: remove dates(as day year month (numbers)) from most frequent terms
##TBD: remove words like '\\u___'

View(matrixFromMostFrequentTerms)
data <- data.frame(matrixFromMostFrequentTerms, "Final.Sentiment"=as.factor(rawTrainingData$Sentiment))

#training and testing
dataTrain <- data.frame(rbind(data[1:1000,], data[4001:13583,]))
dataTest <- data.frame(rbind(data[1001:4000,]))

rm(matrixFromMostFrequentTerms,processedTrainingData,rawTrainingData)

library(e1071)
#NAIVE BAYES
model = naiveBayes(as.factor(dataTrain$Final.Sentiment)~., data = dataTrain)
pred <-predict(model, dataTest, type = "class", threshold = 0.05)

confusionMatrix = table(pred, as.factor(dataTest$Final.Sentiment))
accuracy_nb <- sum(diag(confusionMatrix))/nrow(dataTest) * 100

sprintf("The accuracy of the model using naive bayes is %0.2f", accuracy_nb)
confusionMatrix
