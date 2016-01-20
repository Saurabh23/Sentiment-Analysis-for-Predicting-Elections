install.packages('dendextend')
install.packages('mclust')
install.packages('fpc')
library(mclust)
library(ggplot2)
library(dendextend)
library(tm)
rm(list = ls(all = TRUE))

##Load the dataset from the csv, compile manually for applying different methods.

##### Funtion declarations STARTS

#dendrogram using Euclidean distance, unigrams and ward method
getHCDendogram <- function(data, sparseThreshold, removingWords) {
  require(tm)
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
    text <- removeWords(text, removingWords)
    textList[i] <- text
  }
  
  processedTrainingData <- data.frame(matrix(unlist(textList), nrow = k, byrow = T))
  colnames(processedTrainingData) <- c("Text")
  
  
  dataVector <- VectorSource(processedTrainingData$Text)
  myCorpus <- Corpus(dataVector)
  tdm <- TermDocumentMatrix(myCorpus)
  
  tdm <- removeSparseTerms(tdm, sparse = sparseThreshold)
  tdm.matrix <- as.matrix(tdm)
  distMatrix <- dist(scale(tdm.matrix))
  View(distMatrix)
  distance <- VectorSource(distMatrix)
  fit <- hclust(distMatrix, method = "ward")
  plot(fit)
}
  
#Dendrogram using Different Distances, Bigram and Ward method of clustering
getHCDendogram_wd <- function(data, sparseThreshold, removingWords) {
	require(tm)
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
	  text <- removeWords(text, removingWords)
	  textList[i] <- text
	}

	processedTrainingData <- data.frame(matrix(unlist(textList), nrow = k, byrow = T))
	colnames(processedTrainingData) <- c("Text")


	dataVector <- VectorSource(processedTrainingData$Text)
	myCorpus <- Corpus(dataVector)  #Cameron
	myCorpus_m <- Corpus(dataVector) #Miliband
	bigramTokenizer <-
	  function(x)
	    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
	tdm <- TermDocumentMatrix(myCorpus, control = list(tokenize=bigramTokenizer))
  tdm <- removeSparseTerms(tdm, sparse = sparseThreshold)
	tdm.matrix <- as.matrix(tdm)
	distMatrix <- dist(scale(tdm.matrix))           #Euclidean Distance
	distMatrix_man <- dist(scale(tdm.matrix), method = "manhattan") #Manhatan Distance
	distMatrix_max <- dist(scale(tdm.matrix), method = "maximum") #Maximum distance
	#model based clustering
	#fit <- Mclust(distMatrix)
	#plot(fit) # plot results 
	#summary(fit) # display the best model
	#model based clustering ends
	fit <- hclust(distMatrix_max, method = "ward")    ## Note: Change the parameter in 'hclust' according to distance
	plot(fit)
}

#Different variables



#Dendrogram using Different Distances, Bigram and Ward method of clustering
getHCDendogram_av <- function(data, sparseThreshold, removingWords) {
  require(tm)
  require(ggplot2)
  
  k <- length(data$Text)
  textList <- list()
  for(i in 1: k){
    text <- data$Text[i]
    text <- iconv(text, to = 'UTF-8', sub = 'bzyte')
    text <- tolower(text)
    text <- removeHashTag(text)
    text <- removeReference(text)
    text <- removeShortWords(text)
    text <- removeURL(text)
    text <- removePunctuation(text)
    text <- stripWhitespace(text)
    text <- removeWords(text, stopwords("en"))
    text <- removeWords(text, removingWords)
    textList[i] <- text
  }
  
  processedTrainingData <- data.frame(matrix(unlist(textList), nrow = k, byrow = T))
  colnames(processedTrainingData) <- c("Text")
  dataVector <- VectorSource(processedTrainingData$Text)
  myCorpus <- Corpus(dataVector)
  bigramTokenizer <-
    function(x)
      unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
  tdm <- TermDocumentMatrix(myCorpus, control = list(tokenize=bigramTokenizer))
  
  tdm <- removeSparseTerms(tdm, sparse = sparseThreshold)
  tdm.matrix <- as.matrix(tdm)
  distMatrix <- dist(scale(tdm.matrix))                             ##Euclidean distance: Pass this matrix in 'hclust'
  distMatrix_man <- dist(scale(tdm.matrix), method = "manhattan")   ## Manhattan Distance : Pass this in 'hclust' if needed
  distMatrix_max <- dist(scale(tdm.matrix), method = "maximum")     ## Maximum Distance :  Pass when needed
  fit <- hclust(distMatrix, method = "average")     ## Note: Change the parameter in 'hclust' according to distance
  plot(fit)
}


{
#comparing 2 cluster solutions
library(fpc)
cluster.stats(distMatrix, fit, fit2)
}


#### Function declaration ENDS

#data_test <- read.csv("Health.txt", header=FALSE, sep = "\t")
#colnames(data_test) <- c("Text")

## These steps has been executed manually
cameron <- read.csv("tweets_CAM.csv", sep = ",", quote = '\"')
miliband <- read.csv("Milliband_Tweet.csv", sep = ",", quote = '\"')

#remove more words
removeWordVector = c("david", "cameron", "camerons", "miliband", 
                     "will", "can", "ge2015", "david_cameron", "ed_miliband",
                     "the", "davidcameron", "edmiliband", "youtube", "watch",
                     "2015", "tory", "tories", "snp", "via", "don", "just", "com", "news", "stories", "newstories", "savagedformer")

#removeWordVector = c("will", "can", "ge2015","the", "2015", "tory", "tories", "snp", "via", "don", "just", "com")


#removeWordVector2 = c("shall", "could", "are", "this", 
#                    "will", "can", "year", "country", "via",
#                    "the", "those", "that",
#                    "2015", "it", "tories", "snp", "via", "don", "just", "com")

getHCDendogram_wd(cameron, 0.995, removeWordVector) ## Ward method using sparse 0.995
getHCDendogram_av(cameron, 0.995, removeWordVector) ## Average method using sparse 0.995

getHCDendogram_wd(miliband, 0.995, removeWordVector) ## Ward method using sparse 0.995
getHCDendogram_av(miliband, 0.995, removeWordVector) ## Average method using sparse 0.995

## If function does not execute then use below steps manually

data <- cameron  ## Keep the Cameron dataset in the "data"
data <- miliband  ## Keep the Miliband dataset in the "data_m"

sparseThreshold <- 0.993  ## Pass the sparse values here
removingWords <- removeWordVector ## Use this for removing the specific words

## After these steps goto corresponding function and execute the preprocessing
## execute the code line for corpus
## execute the lines for term document matrix
## use the distance, check which one is needed and make distance matrix
## change the parameters in hclust



