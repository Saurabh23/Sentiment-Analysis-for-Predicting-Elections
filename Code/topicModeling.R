library(tm)
cameron <- read.csv("~/TUE/Quartile1/IRandDM/SentimentAnalysis/WebIR-Full/Data/tweets_CAM.csv", sep = ",")
miliband <- read.csv("~/TUE/Quartile1/IRandDM/SentimentAnalysis/WebIR-Full/Data/Milliband_Tweet.csv", sep = ",")
cameronVector <- VectorSource(cameron$Text)
milibandVector <- VectorSource(miliband$Text)
vector <- VectorSource(list)
myCorpus <- Corpus(vector)
myCorpus <- Corpus(milibandVector)
dtm <- DocumentTermMatrix(myCorpus, control = list(tolower = TRUE,
                                                    removePunctuation = TRUE, 
                                                    stopwords = TRUE,
                                                    removeNumbers = TRUE,
                                                    stemCompletion = TRUE))
                                                    

dtm <- removeSparseTerms(dtm, 0.99)
library(topicmodels)

rowTotals <- apply(dtm, 1, sum)
dtm2.new <- dtm[rowTotals > 0,]
dtm2.matrix <- as.matrix(dtm2.new)
lda <- LDA(dtm2.new, k = 8)
term <- terms(lda, 5)

term <- apply(term, MARGIN = 2, paste, collapse = ", ")
term
topic <- topics(lda,1)

k <- length(rowTotals)
filteredDate <- list()
for(i in 1 : k) {
  if(rowTotals[i] > 0) {
    if(i <= length(cameron$Text)) {
      date <- as.String(cameron$Date[i])
    } else {
      date <- as.String(miliband$Date[i])
    }
    filteredDate[length(filteredDate) + 1] <- date
  }
}

filteredDate <- strptime(filteredDate,format("%a %b %d %H:%M:%M CEST %Y"))
topics <- data.frame(date = as.Date(filteredDate),topic)
library(ggplot2)
qplot(date, ..count..,data = topics, geom = "density",fill = term[topic], xlab = "time", ylab = "density")

today <- Sys.Date()
today
format(today, format("%a %b %d %H:%M:%M CEST %Y"))
help("strptime")

termS
date <- as.factor("Thu May 07 01:59:55 CEST 2015")
date = as.Date(date, format("%a %b %d %H:%M:%M CEST %Y"))
date = as.Date("Aug 07 1989", format("%b %d %Y"))
date

rm(date.df)
