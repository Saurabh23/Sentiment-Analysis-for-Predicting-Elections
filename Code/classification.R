rm(list= ls()[!(ls() %in% c('top_frequency_terms_merged', 'model.svm'))])
library(tm)

############## DAVID CAMERON ############################
cameron <- read.csv("~/TUE/Quartile1/IRandDM/SentimentAnalysis/WebIR-Full/Data/final_dave.csv", stringsAsFactors = FALSE) 
miliband <- read.csv("~/TUE/Quartile1//IRandDM/SentimentAnalysis/WebIR-Full/Data/final_ed.csv", sep = ",")
cameron <- read.csv("~/TUE/Quartile1/IRandDM/SentimentAnalysis/WebIR-Full/Data/Milliband_Tweet.csv", sep = ",")
cameron <- read.csv("~/TUE/Quartile1/IRandDM/SentimentAnalysis/WebIR-Full/Data/tweets_CAM.csv", sep = ",")
trainingSet <- read.csv("~/TUE/Quartile1/IRandDM/SentimentAnalysis/WebIR-Full/Data/trainingandtestdata/training.1600000.processed.noemoticon.csv")
colnames(trainingSet) <- c("polarity", "id", "date", "query", "user", "text")

cameron.text <- as.data.frame(cameron$Tweets)
cameron.sentiment <- as.data.frame(cameron$FinalSentiment)
miliband.text <- as.data.frame(miliband$Text)
miliband.sentiment <- as.data.frame(miliband$Sentiment)
cameron <- data.frame(cameron.sentiment, cameron.text)
colnames(cameron) <- c("Sentiment", "Tweets")
miliband <- data.frame(miliband.sentiment, miliband.text)
colnames(miliband) <- c("Sentiment", "Tweets")

cameron <- rbind(miliband, cameron)

cameron <- as.data.frame(cameron[0:10000, ])
cameron <- as.data.frame(cameron[10001:20000, ])
cameron <- as.data.frame(cameron[20001:27662, ])

cameron <- as.data.frame(cameron[20001:30000, ])
cameron <- as.data.frame(cameron[30001:40000, ])
cameron <- as.data.frame(cameron[40001:50000, ])
cameron <- as.data.frame(cameron[50001:56178, ])
colnames(cameron) <- c("Tweets")

cameronVector <- VectorSource(cameron$Tweets)
top_frequency_terms_1 <- getTermFrequencyList(cameronVector)
top_frequency_terms_2 <- getTermFrequencyList(cameronVector)
top_frequency_terms_3 <- getTermFrequencyList(cameronVector)
top_frequency_terms_4 <- getTermFrequencyList(cameronVector)
top_frequency_terms_5 <- getTermFrequencyList(cameronVector)
top_frequency_terms_6 <- getTermFrequencyList(cameronVector)
top_frequency_terms_7 <- getTermFrequencyList(cameronVector)
top_frequency_terms_8 <- getTermFrequencyList(cameronVector)
top_frequency_terms_9 <- getTermFrequencyList(cameronVector)
top_frequency_terms_10 <- getTermFrequencyList(cameronVector)

trainingVector <- VectorSource(trainingSet$text)
topFrequencyTerms <- getTermFrequencyList(trainingVector)
top_frequency_terms_merged<- c(top_frequency_terms_1,
                                     top_frequency_terms_2,
                                     top_frequency_terms_3,
                                     top_frequency_terms_4,
                                     top_frequency_terms_5,
                                     top_frequency_terms_6,
                                     top_frequency_terms_7,
                                     top_frequency_terms_8,
                                     top_frequency_terms_9
                                     )

top_frequency_terms_merged <- unique(top_frequency_terms_merged)

myCorpus <- Corpus(cameronVector)
myCorpus <- tm_map(myCorpus,
                   content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')),
                   mc.cores=1)
myCorpus <- tm_map(myCorpus, removePunctuation, lazy = T)
myCorpus <- tm_map(myCorpus, stripWhitespace, lazy = T)
myCorpus <- tm_map(myCorpus, removeWords, stopwords("en"), lazy = T)
myCorpus <- tm_map(myCorpus, removeWords, '#\\S+\\s*|@\\S+\\s*|http\\S+\\s*', lazy = T)
myCorpus <- tm_map(myCorpus, content_transformer(tolower), lazy = T)

dtm <- DocumentTermMatrix(myCorpus)
dtm <- DocumentTermMatrix(myCorpus, control = list(tolower = TRUE,
                                                    removePunctuation = TRUE, 
                                                    stopwords = TRUE,
                                                    removeNumbers = TRUE,
                                                    stemCompletion = TRUE))
matrix <-  data.frame(inspect(dtm))
dtm2 <- as.matrix(dtm)
frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing = TRUE)
head(frequency)
frequency_matrix <- as.matrix(frequency)

top_frequency_terms <- as.matrix(frequency_matrix[1:100,])
top_frequency_terms <- frequency[1:100]
top_frequency_terms <- as.list(rownames(as.matrix(top_frequency_terms)))

top_frequency_terms_2 <- as.matrix(frequency_matrix[1:100,])
top_frequency_terms_2 <- frequency[1:100]
top_frequency_terms_2 <- as.list(rownames(as.matrix(top_frequency_terms_2)))

top_frequency_terms_3 <- frequency[1:100]
top_frequency_terms_3 <- as.list(rownames(as.matrix(top_frequency_terms_3)))
#FILTER the terms in the DTM
dtm_frequency_terms <- dtm[, colnames(dtm)%in%top_frequency_terms_merged]
dtm2 <- as.matrix(dtm_frequency_terms)

matrix <- data.frame(inspect(dtm_frequency_terms))
matrix <- data.frame(dtm2)
#wordcloud (for fun)
library(wordcloud)
words <- names(frequency)
wordcloud(words[1:100], frequency[1:100])

data <- data.frame(dtm2, "Final.Sentiment"=as.factor(cameron$Sentiment))
data <- data.frame(matrix, "Final.Sentiment"=as.factor(mixed_final$Sentiment))

#training and testing
data_train <- data.frame(rbind(data[1:2000,], data[3001:5000,]))
data_test <- data.frame(rbind(data[2001:3000,]))

data_train <- data.frame(rbind(data[1:4000,], data[6001:10000,]))
data_test <- data.frame(rbind(data[4001:6000,]))

table(data_train$Final.Sentiment)
table(data_test$Final.Sentiment)

model = naiveBayes(as.factor(data_train$Final.Sentiment)~., data = data_train)
model
pred <-predict(model, data_test,
               type = "class", threshold = 0.05)
#Classification table with test data
conf.nb=table(pred, as.factor(data_test$Final.Sentiment))
accuracy.nb <- sum(diag(conf.nb))/nrow(data_test) * 100
sprintf("The accuracy of the model using naive bayes is %0.2f", accuracy.nb)
conf.nb

#SVM

help(svm)
library(e1071)
model.svm <- svm(data_train$Final.Sentiment ~.,data = data_train, method = "C-classification", kernel = "linear", cost = 10, gamma = 0.1)
model.svm <- svm(data_train,data_train$Final.Sentiment,method = "C-classification", kernel = "radial basis")
model.svm <- svm(data_train_matrix,data_train$Final.Sentiment,method = "C-classification")
summary(model.svm)
pred=predict(model.svm,data_test)
data_test <- data.frame(data_test, as.list(pred))
pred=predict(model.svm, matrix)
#Classification table with test data
conf.svm <- table(pred, as.factor(data_test$Final.Sentiment))
conf.svm <- table(pred)
accuracy.svm <- sum(diag(conf.svm))/nrow(data_test) * 100
sprintf("The accuracy of the model using SVM is %0.2f", accuracy.svm)
conf.svm


#Random Forest
set.seed=1234
modelrf <- randomForest(as.factor(data_train$Final.Sentiment)~., data = data_train,ntree=10)
#plot(modelrf)
key_words <- importance(modelrf)
write.table(key_words,"D:/myproject/key_words.csv",sep=",",row.names=TRUE)
pred.rf <- predict(modelrf, data_test,type="class")
#Classification table with test data 
conf.rf=table(pred.rf, as.factor(data_test$Final.Sentiment)) 
accuracy.rf <- sum(diag(conf.rf))/nrow(data_test) * 100 
sprintf("The accuracy of the model using random forest is %0.2f", accuracy.rf)
conf.rf


############## ED MILLIBAND ############################
milliband <- read.csv("~/TUE/IRandDM/SentimentAnalysis/WebIR-Full/Data/final_dave.csv", sep = ",")
millibandVector <- VectorSource(milliband$Text)
myCorpus <- Corpus(millibandVector)
dtm <- DocumentTermMatrix(myCorpus, control = list(tolower = TRUE,
                                                    removePunctuation = TRUE, 
                                                    stopwords = TRUE,
                                                    removeNumbers = TRUE,
                                                    stemCompletion = TRUE))
                                                    

matrix <-  data.frame(inspect(dtm))
data <- data.frame(matrix, "Final.Sentiment"=as.factor(milliband$Sentiment))
data <- data.frame(matrix, "Final.Sentiment"=as.factor(mixed_final$Sentiment), row.names = NULL)

#training and testing
data_train <- data.frame(rbind(data[1:2000,], data[3001:5000,]))
data_test <- data.frame(rbind(data[2001:3000,]))

data_train <- data.frame(rbind(data_train[1:2000,], data_train[6001:8000,]))
data_test <- data.frame(rbind(data[2001:3000,]))

table(data_train$Final.Sentiment)
table(data_test$Final.Sentiment)

model = naiveBayes(as.factor(data_train$Final.Sentiment)~., data = data_train)
model
pred <-predict(model, data_test,
               type = "class", threshold = 0.05)
#Classification table with test data
conf.nb=table(pred, as.factor(data_test$Final.Sentiment))
accuracy.nb <- sum(diag(conf.nb))/nrow(data_test) * 100
sprintf("The accuracy of the model using naive bayes is %0.2f", accuracy.nb)
conf.nb

#SVM

help(svm)
library(e1071)
model.svm <- svm(data_train$Final.Sentiment ~., data = data_train, method = "C-classification", kernel = "linear", cost = 10, gamma = 0.1)
model.svm <- svm(data_train,data_train$Final.Sentiment,method = "C-classification", kernel = "radial basis")
model.svm <- svm(data_train_matrix,data_train$Final.Sentiment,method = "C-classification")
summary(model.svm)
pred=predict(model.svm,data_test)
#Classification table with test data
conf.svm <- table(pred, as.factor(data_test$Final.Sentiment))
accuracy.svm <- sum(diag(conf.svm))/nrow(data_test) * 100
sprintf("The accuracy of the model using SVM is %0.2f", accuracy.svm)
conf.svm

#Random Forest
set.seed=1234
modelrf <- randomForest(as.factor(data_train$Final.Sentiment)~., data = data_train,ntree=10)
#plot(modelrf)
key_words <- importance(modelrf)
write.table(key_words,"D:/myproject/key_words.csv",sep=",",row.names=TRUE)
pred.rf <- predict(modelrf, data_test,type="class")
#Classification table with test data 
conf.rf=table(pred.rf, as.factor(data_test$Final.Sentiment)) 
accuracy.rf <- sum(diag(conf.rf))/nrow(data_test) * 100 
sprintf("The accuracy of the model using random forest is %0.2f", accuracy.rf)
conf.rf
