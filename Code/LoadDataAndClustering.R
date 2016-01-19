library(tm)

cameron <- read.csv("~/TUE/Quartile1/IRandDM/SentimentAnalysis/WebIR-Full/Data/tweets_CAM.csv", sep = ",")
miliband <- read.csv("~/TUE/Quartile1/IRandDM/SentimentAnalysis/WebIR-Full/Data/Milliband_Tweet.csv", sep = ",")

k = length(cameron$Text)
textList <- list()
for(i in 1: k){
  #text <- as.String(rawTrainingData$Text[i])
  text <- cameron$Text[i]
  text <- iconv(text, to = 'UTF-8', sub = 'byte')
  text <- removeHashTag(text)
  text <- removeReference(text)
  text <- removeShortWords(text)
  text <- removeURL(text)
  text <- removePunctuation(text)
  text <- stripWhitespace(text)
  text <- removeWords(text, stopwords("en"))
  text <- removeWords(text, removeWordVector)
  text <- tolower(text)
  textList[i] <- text
}

myCorpus <- Corpus(cameronVector)

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
  tm_map(myCorpus, content_transformer(tolower), lazy = T)

myCorpus.copy <- myCorpus
myCorpus = myCorpus.copy
myCorpus <- tm_map(myCorpus, content_transformer(stemDocument), lazy = T)
myCorpus <- tm_map(myCorpus, content_transformer(stemCompletion),
                   dictionary = myCorpus.copy, lazy = T)

tdm <- TermDocumentMatrix(myCorpus)
dtm <- DocumentTermMatrix(
  myCorpus, control = list(
    tolower = TRUE,
    removePunctuation = TRUE,
    stopwords = TRUE,
    removeNumbers = TRUE,
    stemCompletion = TRUE,
    stemDocument = TRUE
  )
)

idx <- which(dimnames(tdm)$Terms == "cameron")
inspect(tdm[idx + (0:5), 101:110])

freq.term <- findFreqTerms(tdm, lowfreq = 15)
freq.term

term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 15)
df <- data.frame(term = names(term.freq), freq = term.freq)
library(ggplot2)
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") +
  xlab("Terms") + ylab("Count") + coord_flip()

findAssocs(tdm, "cameron", 0.1)
library(graph)

tdm <- removeSparseTerms(tdm, sparse = 0.95)
dtm2 <- removeSparseTerms(dtm, sparse = 0.98)
# m2 <- as.matrix(tdm2)
cam_matrix <- as.matrix(tdm)
distMatrix <- dist(scale(cam_matrix))
fit <- hclust(distMatrix, method = "ward")
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

for (i in 1:k) {
  cat(paste("cluster", i, ": ", sep = ""))
  s <- sort(kmeansResult$centers[i,], decreasing = T)
  # cat(cameron$Text[names(s)[1:5]], "\n")
  cat(names(s)[1:50], "\n")
}

pos = scan(
  "~/TUE/IRandDM/SentimentAnalysis/WebIR-Full/tweetsPreProcessing/data/positive-words.txt", what = 'character', comment.char = ';'
)
neg = scan(
  "~/TUE/IRandDM/SentimentAnalysis/WebIR-Full/tweetsPreProcessing/data/negative-words.txt", what = 'character', comment.char = ';'
)
pos.words <- c(pos, 'upgrade')
neg.words <-
  c(neg, 'wtf', 'wait', 'waiting', 'epicfail', 'mechanical')
conservative <-
  read.csv(
    "TUE/IRandDM/SentimentAnalysis/WebIR-Full/Data/tweets_conservative_party.csv", sep = "\t"
  )
conservativeTweets <- conservative["Tweet"]
millibandVector <- VectorSource(milliband$text)
mixed <- VectorSource(mixed$text)
test <-
  read.csv("TUE/IRandDM/SentimentAnalysis/mixed_test.csv", sep = ",")

sortedCameron <-
  cameron[order(cameron$UserName, cameron$Favorites, cameron$Retweets),]

camer


string <- "@eindhoven GLow Festival"
string <- removeReference(string)
rm(string)
handleTweetsData(cameronVector)

rm(list = ls(all = TRUE))
rm(list = ls()[!(
  ls() %in% c(
    'top_frequency_terms','top_frequency_terms_2', 'top_frequency_terms_3'
  )
)])
