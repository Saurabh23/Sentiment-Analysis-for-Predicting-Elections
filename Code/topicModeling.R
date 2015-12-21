library(tm)

#function for topic modeling, arguments: dataframe, stopword list, number of topics, number of terms
performTopicModeling <- function(data, removeWordVector, nTopics, nTerms) {
  require(tm)
  require(topicmodels)
  require(ggplot2)
  
  vector <- VectorSource(data$Text)
  myCorpus <- Corpus(vector)
  
  #pre-processing
  myCorpus <- tm_map(myCorpus,
                     content_transformer(function(x)
                       iconv(x, to = 'UTF-8', sub = 'byte')),
                     mc.cores = 1)
  myCorpus <-
    tm_map(myCorpus, content_transformer(removeURL), lazy = T)
  myCorpus <-
    tm_map(myCorpus, content_transformer(removeReference), lazy = T)
  myCorpus <-
    tm_map(myCorpus, content_transformer(removeHashTag), lazy = T)
  
  myCorpus <- tm_map(myCorpus, removePunctuation, lazy = T)
  myCorpus <- tm_map(myCorpus, stripWhitespace, lazy = T)
  myCorpus <-
    tm_map(myCorpus, removeWords, stopwords("en"), lazy = T)
  myCorpus <-
    tm_map(myCorpus, removeWords, removeWordVector, lazy = T)
  myCorpus <-
    tm_map(myCorpus, content_transformer(tolower), lazy = T)
  
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
removeWordVector = c("david", "cameron", "camerons", "miliband", "will", "can", "ge2015")
performTopicModeling(data = cameron, 
                     removeWordVector = removeWordVector,
                     nTopics = 5,
                     nTerms = 4)
