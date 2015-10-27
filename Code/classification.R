library(tm)

############## DAVID CAMERON ############################
cameron <- read.csv("~/TUE/IRandDM/SentimentAnalysis/WebIR-Full/Data/final_ed.csv", sep = ",")
cameron <- read.csv("~/TUE/IRandDM/SentimentAnalysis/WebIR-Full/Data/Milliband_Tweets_extracted.csv", sep = ",")
cameron.text <- as.data.frame(cameron$Tweets)
cameron.sentiment <- as.data.frame(cameron$FinalSentiment)
cameron <- data.frame(cameron.sentiment, cameron.text)
colnames(cameron) <- c("Sentiment", "Tweets")

cameron <- as.data.frame(cameron[11000:11000, ])
colnames(cameron) <- c("Tweets")



cameronVector <- VectorSource(cameron$Text)
myCorpus <- Corpus(cameronVector)
dtm <- DocumentTermMatrix(myCorpus, control = list(tolower = TRUE,
                                                    removePunctuation = TRUE, 
                                                    stopwords = TRUE,
                                                    removeNumbers = TRUE,
                                                    stemCompletion = TRUE))
matrix <-  data.frame(inspect(dtm))

data <- data.frame(matrix, "Final.Sentiment"=as.factor(cameron$Sentiment))
data <- data.frame(matrix, "Final.Sentiment"=as.factor(mixed_final$Sentiment))

#training and testing
data_train <- data.frame(rbind(data[1:2000,], data[3001:5000,]))
data_test <- data.frame(rbind(data[2001:3000,]))

data_train <- data.frame(rbind(data[1:3000,], data[8001:10000,]))
data_test <- data.frame(rbind(data[3001:8000,]))

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
pred=predict(model.svm, matrix)
#Classification table with test data
conf.svm <- table(pred, as.factor(data_test$Final.Sentiment))
accuracy.svm <- sum(diag(conf.svm))/nrow(data_test) * 100
sprintf("The accuracy of the model using SVM is %0.2f", accuracy.svm)


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
