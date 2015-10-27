library(tm)
handleTweetsData <- function(vsrc) { 
  # tweetVector <- VectorSource(conservative$Tweet)
  myCorpus <- Corpus(vsrc)
  camCorpus<- Corpus(cameronVector)
  milCorpus <- Corpus(millibandVector)
  mixedCorpus <- Corpus(mixed)
  # myCorpus <- tm_map(myCorpus, content_transformer(tolower))
  # myCorpus <- tm_map(myCorpus, removePunctuation)
  # myCorpus <- tm_map(myCorpus, removeNumbers)
  # myCorpus <- tm_map(myCorpus, removeURL)
  # myStopwords <- c(stopwords("english"), "available", "via")
  # myStopwords <- setdiff(myStopwords, c("r", "big"))
  # myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
  
  # myCorpusCopy <- myCorpus
  # myCorpus <- tm_map(myCorpus, stemDocument)
  
  # for(i in 1:5) {
    # cat(paste("[[", i, "]]", sep = ""))
    # writeLines(myCorpus[[i]])
  # }
  
  # myCorpus <- tm_map(myCorpus, stemCompletion, dictionary = myCorpusCopy)
  
  # tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))
  
  cam_tdm <- TermDocumentMatrix(camCorpus, control = list());
  mil_tdm <- TermDocumentMatrix(milCorpus, control = list());
  mixed_tdm <- TermDocumentMatrix(mixedCorpus, control = list());
  dtm <- DocumentTermMatrix(myCorpus, control = list(tolower = TRUE,
                                                    removePunctuation = TRUE, 
                                                    stopwords = TRUE,
                                                    removeNumbers = TRUE,
                                                    stemCompletion = TRUE,
                                                    stemDocument = TRUE))
  
  cam_tdm2 <- removeSparseTerms(cam_tdm, sparse = 0.98)
  dtm2 <- removeSparseTerms(dtm, sparse = 0.98)
  # m2 <- as.matrix(tdm2)
  cam_matrix <- as.matrix(cam_tdm2)
  distMatrix <- dist(scale(cam_matrix))
  fit <- hclust(distMatrix)
  plot(fit)
  
  
  mil_dtm2 <- removeSparseTerms(mil_tdm, sparse = 0.98)
  mil_matrix <- as.matrix(mil_dtm2)
  distMatrix2 <- dist(scale(mil_matrix))
  fit2 <- hclust(distMatrix2)
  plot(fit2)
  
  
  mixed_tdm2 <- removeSparseTerms(mixed_tdm, sparse = 0.98)
  mixed_matrix <- as.matrix(mixed_tdm2)
  distMatrix3 <- dist(scale(mixed_matrix))
  fit3 <- hclust(distMatrix3)
  plot(fit3)
  
  
  m3 <- t(mixed_matrix)
  set.seed(1230)
  k <- 2
  kmeansResult <- kmeans(m3, k)
  round(kmeansResult$centers, digits = 3)
  
  for(i in 1:k) {
    cat(paste("cluster", i, ": ", sep = ""))
    s <- sort(kmeansResult$centers[i,], decreasing = T)
    # cat(cameron$Text[names(s)[1:5]], "\n")
    cat(names(s)[1:50], "\n")
  }
}

  pos = scan("~/TUE/IRandDM/SentimentAnalysis/WebIR-Full/tweetsPreProcessing/data/positive-words.txt", what = 'character', comment.char = ';')
  neg = scan("~/TUE/IRandDM/SentimentAnalysis/WebIR-Full/tweetsPreProcessing/data/negative-words.txt", what = 'character', comment.char = ';')
  pos.words <- c(pos, 'upgrade')
  neg.words <- c(neg, 'wtf', 'wait', 'waiting', 'epicfail', 'mechanical')
  conservative <- read.csv("TUE/IRandDM/SentimentAnalysis/WebIR-Full/Data/tweets_conservative_party.csv", sep = "\t")
  conservativeTweets <- conservative["Tweet"]
  cameron <- read.csv("~/TUE/IRandDM/SentimentAnalysis/CAM.csv", stringsAsFactors = FALSE) 
  milliband <- read.csv("~/TUE/IRandDM/SentimentAnalysis/MIL.csv", stringsAsFactors = FALSE) 
  mixed_data <- read.csv("~/TUE/IRandDM/SentimentAnalysis/Mixed.csv", stringsAsFactors = FALSE) 
  cameronVector <- VectorSource(cameron$text)
  millibandVector <- VectorSource(milliband$text)
  mixed <- VectorSource(mixed$text)
  test <- read.csv("TUE/IRandDM/SentimentAnalysis/mixed_test.csv", sep = ",")
  
  sortedCameron <- cameron[order(cameron$UserName, cameron$Favorites, cameron$Retweets), ]
  
  
  cameron <- read.csv("~/TUE/IRandDM/SentimentAnalysis/WebIR-Full/Data/final_dave_tweets.csv", stringsAsFactors = FALSE) 
  milliband <- read.csv("~/TUE/IRandDM/SentimentAnalysis/MIL.csv", stringsAsFactors = FALSE)
  
  camer

removeURL <- function(x) gsub('http\\S+\\s*','', x)

handleTweetsData(cameronVector)

rm(myCorpus) 
rm(myCorpusCopy)
rm(tdm2)
rm(tweets_conservative.df) 
rm(conservative)
rm(m2)
rm(list=ls(all=TRUE))
