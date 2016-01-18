library(tm)
library(e1071)

rawTrainingData <- read.csv("~/TUE/Quartile1/IRandDM/SentimentAnalysis/WebIR-Full/Data/SemEvalWithoutNeutral.csv", sep = ",")
vector <- VectorSource(rawTrainingData$Text)

bigrams <- getBigramFrequencyList(vector, 10)
pos = scan(
  "~/TUE/Quartile1/IRandDM/SentimentAnalysis/WebIR-Full/tweetsPreProcessing/data/positive-words.txt", what = 'character', comment.char = ';'
)
neg = scan(
  "~/TUE/Quartile1/IRandDM/SentimentAnalysis/WebIR-Full/tweetsPreProcessing/data/negative-words.txt", what = 'character', comment.char = ';'
)

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

k <- length(rawTrainingData$Text)
textList <- list()
for(i in 1: k){
  #text <- as.String(rawTrainingData$Text[i])
  text <- rawTrainingData$Text[i]
  text <- iconv(text, to = 'UTF-8', sub = 'byte')
  text <- tolower(text)
  text <- removeHashTag(text)
  text <- removeReference(text)
  text <- removePunctuation(text)
  text <- removeShortWords(text)
  text <- removeURL(text)
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

myCorpus <- Corpus(trainingVector)

bigramTokenizer <-
  function(x) {
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
  }
dtm <- DocumentTermMatrix(myCorpus, control = list(weighting=weightTfIdf, tokenize=bigramTokenizer))
mostFrequentTerms <- getTermFrequencyList(trainingVector, 200)
bigramFrequentTerms <- filterBigrams(dtm, mostFrequentTerms)
mostFrequentBigram <- getBigramFrequencyList(trainingVector, 10)
dtmFromMostFrequentTerms <- dtm[, colnames(dtm)%in%mostFrequentBigram]

matrixFromMostFrequentTerms <- as.matrix(dtmFromMostFrequentTerms)
data <- data.frame(matrixFromMostFrequentTerms, "Final.Sentiment"=as.factor(rawTrainingData$Sentiment))

dataTrain <- data.frame(rbind(data[1001:10400, ]))
dataTest <- data.frame(rbind(data[1:1000,]))
testingText <- data.frame(rbind(rawTrainingData[1:1000,]))

model = naiveBayes(as.factor(dataTrain$Final.Sentiment)~., data = dataTrain, laplace = 0.3)
pred <- predict(model, dataTest, type = "class")
pred_prob <- predict(model, dataTest, type = "raw")
confusionMatrix <- table(pred, as.factor(dataTest$Final.Sentiment))

accuracy <- sum(diag(confusionMatrix))/nrow(dataTest) * 100
truepositives <- confusionMatrix[4]
falsepositives <- confusionMatrix[2]
truenegatives<- confusionMatrix[1]
falsenegatives <- confusionMatrix[3]

precisionpos <- truepositives / (truepositives + falsepositives) * 100
recallpos <- truepositives / (truepositives + falsenegatives) * 100
f1scorepos <- 2 * precisionpos * recallpos / (precisionpos + recallpos)

precisionneg <- truenegatives / (truenegatives + falsenegatives) * 100
recallneg <- truenegatives / (truenegatives + falsepositives) * 100
f1scoreneg <- 2 * precisionneg * recallneg/ (precisionneg + recallneg)

print(paste("the accuracy of the model using naive bayes is " , accuracy))
print(paste("the precision for positive and negative are ", precisionpos, "and", precisionneg))
print(paste("the recall for positive and negative are ", recallpos, "and", recallneg))
print(paste("the f1-score for positive and negative are ", f1scorepos, "and", f1scoreneg))
print(confusionMatrix)

time <- timestamp()
write.table(confusionMatrix, paste("~/Desktop/confusion-", method ,"-fold", as.String(time), ".txt"))

result <- cbind(testingText, pred)
time <- timestamp()
write.csv(result, paste("~/Desktop/result" , as.String(time) ,".csv"), quote = T)
