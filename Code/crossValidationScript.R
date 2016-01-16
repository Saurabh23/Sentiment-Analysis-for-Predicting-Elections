#EXTRACT MOST FREQUENT TEMRS
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

filterBigrams <- function(dtm, wordList) {
  colnames <- colnames(dtm)
  k <- length(colnames)
  termList <- list()
  for (i in 1:k){
    terms <- strsplit(colnames[i], " ")[[1]]
    if (grepl("\\d", terms[1]) || grepl("\\d", terms[2]) ) {
      next
    } else if (terms[1] %in% wordList|| terms[2] %in% wordList) {
      termList[length(termList) + 1] <- colnames[i]
    }
  }
  return (termList)
}

trainAndTest <- function(testingText, dataTrain, dataTest, foldSize, method) {
  if(method == "NB") {
    model = naiveBayes(as.factor(dataTrain$Final.Sentiment)~., data = dataTrain, laplace = 0.3)
    pred <- predict(model, dataTest, type = "class", threshold = 0.05)
    print(summary(pred))
    
  } else if(method == "SVM") {
    model <- svm(dataTrain$Final.Sentiment ~., data = dataTrain, method = "C-classification", kernel = "linear", cost = 10, gamma = 0.1)
    pred = predict(model, dataTest)
  }
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
  return (model)
}

performClassification <- function(rawTrainingData, method, isCrossValidation, lm, feature) {
  require(tm)
  require(e1071)
  pos = scan(
    "~/TUE/Quartile1/IRandDM/SentimentAnalysis/WebIR-Full/tweetsPreProcessing/data/positive-words.txt", what = 'character', comment.char = ';'
  )
  neg
  
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
  
  myCorpus <- Corpus(trainingVector)
  
  if (lm == "bigram") {
    bigramTokenizer <-
      function(x) {
        unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
      }
    dtm <- DocumentTermMatrix(myCorpus, control = list(weighting=weightTfIdf, tokenize=bigramTokenizer))
    if (feature == "MFT") {
      mostFrequentTerms <- getTermFrequencyList(trainingVector, 15)
      bigramFrequentTerms <- filterBigrams(dtm, mostFrequentTerms)
      dtmFromMostFrequentTerms <- dtm[, colnames(dtm)%in%bigramFrequentTerms]
    } else if (feature == "huliu") {
      bigramFrequentTerms <- filterBigrams(dtm, posNegWords)
      dtmFromMostFrequentTerms <- dtm[, colnames(dtm)%in%bigramFrequentTerms]
    } else if (feature == "mix") {
      mostFrequentTerms <- getTermFrequencyList(trainingVector, 15)
      bigramFrequentTerms1 <- filterBigrams(dtm, posNegWords)
      bigramFrequentTerms2 <- filterBigrams(dtm, mostFrequentTerms)
      dtmFromMostFrequentTerms <- dtm[, colnames(dtm)%in%bigramFrequentTerms1
                                                           && colnames(dtm)%in%bigramFrequentTerms2]
    }
  } else if (lm == "unigram") {
    dtm <- DocumentTermMatrix(myCorpus, control = list(weighting= weightTfIdf))
    if (feature == "MFT") {
      mostFrequentTerms <- getTermFrequencyList(trainingVector, 15)
      dtmFromMostFrequentTerms <- dtm[, colnames(dtm)%in%mostFrequentTerms]
    } else if (feature == "huliu") {
      dtmFromMostFrequentTerms <- dtm[, colnames(dtm)%in%posNegWords]
    } else if (feature == "mix") {
      mostFrequentTerms <- getTermFrequencyList(trainingVector, 15)
      dtmFromMostFrequentTerms <- dtm[, colnames(dtm)%in%mostFrequentTerms
                                                           && colnames(dtm)%in%posNegWords]
    }
  } else if (lm == "mix") {
    bigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 2))}
    dtm <- DocumentTermMatrix(myCorpus, control = list(weighting=weightTfIdf, tokenize=bigramTokenizer))
    if (feature == "MFT") {
      mostFrequentTerms <- getTermFrequencyList(trainingVector, 15)
      bigramFrequentTerms <- filterBigrams(dtm, mostFrequentTerms)
      dtmFromMostFrequentTerms <- dtm[, colnames(dtm)%in%bigramFrequentTerms]
    } else if (feature == "huliu") {
      bigramFrequentTerms <- filterBigrams(dtm, posNegWords)
      dtmFromMostFrequentTerms <- dtm[, colnames(dtm)%in%bigramFrequentTerms]
    } else if (feature == "mix") {
      mostFrequentTerms <- getTermFrequencyList(trainingVector, 15)
      bigramFrequentTerms1 <- filterBigrams(dtm, posNegWords)
      bigramFrequentTerms2 <- filterBigrams(dtm, mostFrequentTerms)
      dtmFromMostFrequentTerms <- dtm[, colnames(dtm)%in%bigramFrequentTerms1
                                                           && colnames(dtm)%in%bigramFrequentTerms2]
    }
  }
  
  matrixFromMostFrequentTerms <- as.matrix(dtmFromMostFrequentTerms)
  data <- data.frame(matrixFromMostFrequentTerms, "Final.Sentiment"=as.factor(rawTrainingData$Sentiment))
  
  len <- length(rawTrainingData$Text)
  foldSize <- len / 10
  
  if(isCrossValidation) {
    for (i in 1:10) {
      print(paste("training fold ", i))
      testBegin <- (i - 1) * foldSize + 1
      print(testBegin)
      print(testBegin + foldSize)
      dataTest <- data.frame(rbind(data[testBegin:(testBegin + foldSize),]))
      dataTrain <- data.frame(rbind(data[1:testBegin,], data[(testBegin + foldSize + 1):len,]))
      testingText <- data.frame(rbind(rawTrainingData[testBegin:(testBegin + foldSize),]))
      
      print(paste("test size: ", length(dataTest$Final.Sentiment)))
      print(paste("train size: ", length(dataTrain$Final.Sentiment)))
      
      model <- trainAndTest(testingText, dataTrain, dataTest, foldSize, method)
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
  
  return (model)
}