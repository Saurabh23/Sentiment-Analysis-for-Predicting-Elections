#EXTRACT MOST FREQUENT TEMRS
getBigramFrequencyList <- function(vectorSource, freq) {
  myCorpus <- Corpus(vectorSource)
  
  bigramTokenizer <-
    function(x) {
      unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
    }
  dtm <- DocumentTermMatrix(myCorpus, control = list(tokenizer=bigramTokenizer))
  
  dtm2 <- as.matrix(dtm)
  frequency <- colSums(dtm2)
  frequency <- sort(frequency, decreasing = TRUE)
  head(frequency)
  
  top_frequency_terms <- subset(frequency, frequency >= freq)
  top_frequency_terms <- as.list(rownames(as.matrix(top_frequency_terms)))
  
  return (top_frequency_terms)
}

getTermFrequencyList <- function(vectorSource, freq) {
  myCorpus <- Corpus(vectorSource)
  
  dtm <- DocumentTermMatrix(myCorpus)
  
  dtm2 <- as.matrix(dtm)
  frequency <- colSums(dtm2)
  frequency <- sort(frequency, decreasing = TRUE)
  head(frequency)
  
  top_frequency_terms <- subset(frequency, frequency >= freq)
  top_frequency_terms <- as.list(rownames(as.matrix(top_frequency_terms)))
  
  return (top_frequency_terms)
}


filterBigrams <- function(bigrams, wordList) {
  k <- length(bigrams)
  termList <- list()
  for (i in 1:k){
    terms <- strsplit(as.character(bigrams[i]), " ")[[1]]
    if (grepl("\\d", terms[1]) || grepl("\\d", terms[2]) ) {
      next
    } else if (terms[1] %in% wordList|| terms[2] %in% wordList) {
      termList[length(termList) + 1] <- as.character(bigrams[i])
    }
  }
  return (termList)
}

trainAndTest <- function(testingText, dataTrain, dataTest, foldSize, method) {
  print(summary(dataTrain$Final.Sentiment))
  if(method == "NB") {
    model = naiveBayes(as.factor(dataTrain$Final.Sentiment)~., data = dataTrain, laplace = 0.03)
    pred <- predict(model, dataTest, type = "raw", threshold = 0.03, esp = 0)
    
  } else if(method == "SVM") {
    model <- svm(dataTrain$Final.Sentiment ~., data = dataTrain, method = "C-classification", kernel = "linear", cost = 10, gamma = 0.1)
    pred = predict(model, dataTest)
  }
  
  len <- length(dataTest$Final.Sentiment)
  features <- list()
  for (i in 1:len) {
    list <- list()
    sample <- dataTest[i,]
    for (i in 1:ncol(sample)) {
      if (nrow(sample) < 1) {
        break
      }
      if (as.numeric(sample[1,i]) != 0) {
        list[length(list) + 1] <- colnames(sample) [i]
      }
    }
    features[length(features) + 1] <- paste(list, sep = "", collapse = " ")
  }
  
  sent <- list()
  pred.df <- as.data.frame(pred)
  for (i in 1:len) {
    if (pred.df$positive[i] == 1) {
      sent[i] <- "positive"
    } else {
      sent[i] <- "negative"
    }
  }
  sent <- unlist(sent)
  
  print(length(pred))
  print(length(dataTest$Final.Sentiment))
  confusionMatrix <- table(sent, as.factor(dataTest$Final.Sentiment))
  
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
  
  statistics <- list(accuracy, precisionpos, recallpos, precisionneg, recallneg, f1scorepos, f1scoreneg)
  
  time <- timestamp()
  write.table(confusionMatrix, paste("~/Desktop/confusion-", method ,"-fold", as.String(time), ".txt"))
  
  features <- unlist(features)
  result <- cbind(testingText, sent, features)
  time <- timestamp()
  write.csv(result, paste("~/Desktop/result" , as.String(time) ,".csv"), quote = T)
  
#   UNCOMMENT IF RUNNING WITHOUT CROSS VALIDATION AND NEED AN ROC CURVE
#   require(ROCR)
#   prediction <- prediction(pred.df$positive, dataTest$Final.Sentiment, label.ordering = NULL)
#   perf <- performance(prediction, "tpr", "fpr")
#   plot(perf)
  return (statistics)
}

performClassification <- function(rawTrainingData, method, isCrossValidation, lm, feature, isStem, isStopWord) {
  require(tm)
  require(e1071)
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
  stopwords <- scan("~/Desktop/stopwords.txt", what = 'character')
  
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
    text <- removeShortWords(text)
    text <- removeURL(text)
    text <- removePunctuation(text)
    text <- stripWhitespace(text)
    
    if (isStopWord) {
      text <- removeWords(text, stopwords)
    }
    
    #Stemming
    if (isStem) {
      text <- lapply(strsplit(text, " "), stemDocument, language = "english") [[1]]
      newText <- ""
      for (word in text) {
        newText <- paste(newText, word)
      }
      text <- newText
    }
    
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
  
  if (lm == "bigram") {
    bigramTokenizer <-
      function(x) {
        unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
      }
    dtm <- DocumentTermMatrix(myCorpus, control = list(tokenize=bigramTokenizer))
    if (feature == "MFT") {
      bigramFrequentTerms <- getBigramFrequencyList(trainingVector, 10)
      dtmFromMostFrequentTerms <- dtm[, colnames(dtm)%in%bigramFrequentTerms]
    } else if (feature == "huliu") {
      bigramFrequentTerms <- getBigramFrequencyList(trainingVector, 10)
      bigramFrequentTerms <- filterBigrams(bigramFrequentTerms, posNegWords)
      dtmFromMostFrequentTerms <- dtm[, colnames(dtm)%in%bigramFrequentTerms]
    } else if (feature == "mix") {
      mostFrequentTerms <- getTermFrequencyList(trainingVector, 15)
      bigramFrequentTerms1 <- filterBigrams(dtm, posNegWords)
      bigramFrequentTerms2 <- filterBigrams(dtm, mostFrequentTerms)
      dtmFromMostFrequentTerms <- dtm[, colnames(dtm)%in%c(bigramFrequentTerms1,
                                                           bigramFrequentTerms2)]
    }
  } else if (lm == "unigram") {
    dtm <- DocumentTermMatrix(myCorpus) 
    if (feature == "MFT") {
      mostFrequentTerms <- getTermFrequencyList(trainingVector, 15)
      dtmFromMostFrequentTerms <- dtm[, colnames(dtm)%in%mostFrequentTerms]
    } else if (feature == "huliu") {
      dtmFromMostFrequentTerms <- dtm[, colnames(dtm)%in%posNegWords]
    } else if (feature == "mix") {
      mostFrequentTerms <- getTermFrequencyList(trainingVector, 30)
      dtmFromMostFrequentTerms <- dtm[, colnames(dtm)%in% c(mostFrequentTerms
                                                           ,posNegWords)]
    }
  } else if (lm == "mix") {
    bigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 2))}
    dtm <- DocumentTermMatrix(myCorpus, control = list(tokenize=bigramTokenizer))
    if (feature == "MFT") {
      mostFrequentTerms <- getTermFrequencyList(trainingVector, 20)
      bigramFrequentTerms <- getBigramFrequencyList(trainingVector, 15)
      dtmFromMostFrequentTerms <- dtm[, colnames(dtm)%in%c(bigramFrequentTerms,mostFrequentTerms)]
    } else if (feature == "huliu") {
      bigramFrequentTerms <- getBigramFrequencyList(trainingVector, 10)
      bigramFrequentTerms <- filterBigrams(bigramFrequentTerms, posNegWords)
      dtmFromMostFrequentTerms <- dtm[, colnames(dtm)%in% c(bigramFrequentTerms,
                                                                posNegWords)]
    } else if (feature == "mix") {
      mostFrequentTerms <- getTermFrequencyList(trainingVector, 15)
      bigramFrequentTerms <- getBigramFrequencyList(trainingVector, 15)
      dtmFromMostFrequentTerms <- dtm[, colnames(dtm)%in% c(mostFrequentTerms,
                                                           bigramFrequentTerms,
                                                           posNegWords)]
    }
  }
  
  rm(dtm)
  matrixFromMostFrequentTerms <- as.matrix(dtmFromMostFrequentTerms)
  #rm(list= ls()[!(ls() %in% c('matrixFromMostFrequentTerms', 'rawTrainingData',
  #                            'method', 'isCrossValidation', 'lm', 'feature', 'isStem', 'isStopWord'))])
  data <- data.frame(matrixFromMostFrequentTerms, "Final.Sentiment"=as.factor(rawTrainingData$Sentiment))
  rm(matrixFromMostFrequentTerms)
  
  len <- length(rawTrainingData$Text)
  foldSize <- len / 10
  
  accuracies <- list()
  posPrecisions <- list()
  posRecalls <- list()
  negPrecisions <- list()
  negRecalls <- list()
  posf1 <- list()
  negf1 <- list()
  
  if(isCrossValidation) {
    for (i in 1:10) {
      print(paste("training fold ", i))
      testBegin <- (i - 1) * foldSize + 1
      print(testBegin)
      print(testBegin + foldSize)
      dataTest <- data.frame(rbind(data[testBegin:(testBegin + foldSize - 1),]))
      dataTrain <- data.frame(rbind(data[1:testBegin,], data[(testBegin + foldSize + 1):len,]))
      testingText <- data.frame(rbind(rawTrainingData[testBegin:(testBegin + foldSize - 1),]))
      
      print(paste("test size: ", length(dataTest$Final.Sentiment)))
      print(paste("train size: ", length(dataTrain$Final.Sentiment)))
      
      model <- trainAndTest(testingText, dataTrain, dataTest, foldSize, method)
      accuracies[length(accuracies) + 1] <- model[1]
      posPrecisions[length(posPrecisions) + 1] <- model[2]
      negPrecisions[length(negPrecisions) + 1] <- model[3]
      posRecalls[length(posRecalls) + 1] <- model[4]
      negRecalls[length(negRecalls) + 1] <- model[5]
      posf1[length(posf1) + 1] <- model[6]
      negf1[length(negf1) + 1] <- model[7]
    }
  }else {
    print("training ...")
    dataTrain <- data.frame(rbind(data[(foldSize+ 1):len, ]))
    dataTest <- data.frame(rbind(data[1:foldSize,]))
    testingText <- data.frame(rbind(rawTrainingData[1:foldSize,]))
    
    print(paste("test size: ", length(dataTest$Final.Sentiment)))
    print(paste("train size: ", length(dataTrain$Final.Sentiment)))
    
    model <- trainAndTest(testingText, dataTrain, dataTest, foldSize, method)
  }
  
#   print(paste("mean accuracy = ", mean(as.numeric(accuracies))))
#   print(paste("sd accuracy = ", sd(as.numeric(accuracies))))
#   
#   print(paste("mean pos precision= ", mean(as.numeric(posPrecisions))))
#   print(paste("sd pos precision= ", sd(as.numeric(posPrecisions))))
#   
#   print(paste("mean pos recall = ", mean(as.numeric(posRecalls))))
#   print(paste("sd pos recall = ", sd(as.numeric(posRecalls))))
#   
#   print(paste("mean neg precision = ", mean(as.numeric(negPrecisions))))
#   print(paste("sd neg precision = ", sd(as.numeric(negPrecisions))))
#   
#   print(paste("mean neg recall = ", mean(as.numeric(negRecalls))))
#   print(paste("sd neg recall = ", sd(as.numeric(negRecalls))))
#   
#   print(paste("mean pos f1 = ", mean(as.numeric(posf1))))
#   print(paste("sd pos f1 = ", sd(as.numeric(posf1))))
#   
#   print(paste("mean neg f1 = ", mean(as.numeric(negf1))))
#   print(paste("sd pos f1 = ", sd(as.numeric(negf1))))
  return (model)
}