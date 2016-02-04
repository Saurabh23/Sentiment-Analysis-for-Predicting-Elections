library(tm)

#function for topic modeling, arguments: dataframe, stopword list, number of topics, number of terms
performTopicModeling <- function(data, removeWordVector, nTopics, nTerms) {
  require(tm)
  require(topicmodels)
  require(ggplot2)
  
  
	k <- length(data$Text)
	textList <- list()
	for(i in 1: k){
		text <- data$Text[i]
		text <- iconv(text, to = 'UTF-8', sub = 'byte')
		text <- tolower(text)
		text <- removeHashTag(text)
		text <- removeReference(text)
		text <- removeShortWords(text)
		text <- removeURL(text)
		text <- removePunctuation(text)
		text <- stripWhitespace(text)
		text <- removeWords(text, stopwords("en"))
		text <- removeWords(text, removeWordVector)
		textList[i] <- text
	}
	
	processedTrainingData <- data.frame(matrix(unlist(textList), nrow = k, byrow = T))
	colnames(processedTrainingData) <- c("Text")
	
  vector <- VectorSource(processedTrainingData$Text)
  myCorpus <- Corpus(vector)
  #create dtm, remove empty rows
  dtm <- DocumentTermMatrix(myCorpus)
  dtm <- removeSparseTerms(dtm, 0.99)
  rowTotals <- apply(dtm, 1, sum)
  dtm2.new <- dtm[rowTotals > 0,]
  
  #use LDA to cluster topics
  lda <- LDA(dtm2.new, k = nTopics)
  topic <- topics(lda,1)
  
  #extract representative terms for each topic
  term <- terms(lda, nTerms)
  term <- apply(term, MARGIN = 2, paste, collapse = ", ")
  
  #construct the date list, remove the dates corresponding to the empty rows above
  k <- length(rowTotals)
  filteredDate <- list()
  for (i in 1:k) {
    if (rowTotals[i] > 0) {
      date <- as.String(data$Date[i])
      filteredDate[length(filteredDate) + 1] <- date
    }
  }
  
  filteredDate <-
    strptime(filteredDate,format("%a %b %d %H:%M:%M CEST %Y"))
  topics <- data.frame(date = as.Date(filteredDate),topic)
  qplot(
    date, ..count..,data = topics, geom = "density", fill = term[topic]
  )
}


#Example
cameron <- read.csv("~/TUE/Quartile1/IRandDM/SentimentAnalysis/WebIR-Full/Data/tweets_CAM.csv", sep = ",")
miliband <- read.csv("~/TUE/Quartile1/IRandDM/SentimentAnalysis/WebIR-Full/Data/Milliband_Tweet.csv", sep = ",")
removeWordVector = c("david", "cameron", "camerons", "miliband", 
                     "can", "ge2015", "david_cameron", "ed_miliband",
                     "the", "davidcameron", "edmiliband",
                     "2015", "tory", "tories", "snp", "via", "don", "just", "com",
                     "election")
performTopicModeling(data = miliband, 
                     removeWordVector = removeWordVector,
                     nTopics = 7,
                     nTerms = 4)
