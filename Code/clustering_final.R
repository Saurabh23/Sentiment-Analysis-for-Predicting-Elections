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
	fit <- hclust(distMatrix, method = "ward")
	plot(fit)
}

cameron <- read.csv("~/TUE/Quartile1/IRandDM/SentimentAnalysis/WebIR-Full/Data/tweets_CAM.csv", sep = ",", quote = '\"')
miliband <- read.csv("~/TUE/Quartile1/IRandDM/SentimentAnalysis/WebIR-Full/Data/Milliband_Tweet.csv", sep = ",", quote = '\"')
removeWordVector = c("david", "cameron", "camerons", "miliband", 
                     "will", "can", "ge2015", "david_cameron", "ed_miliband",
                     "the", "davidcameron", "edmiliband",
                     "2015", "tory", "tories", "snp", "via", "don", "just", "com")
getHCDendogram(cameron, 0.98, removeWordVector)
