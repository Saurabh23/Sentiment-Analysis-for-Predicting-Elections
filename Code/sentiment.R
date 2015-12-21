library(sentiment)
#classify_emotion
milliband <- read.csv("~/TUE/Quartile1/IRandDM/SentimentAnalysis/WebIR-Full/Data/tweets_CAM.csv", quote = "")
milliband <- read.csv("~/TUE/Quartile1/IRandDM/SentimentAnalysis/WebIR-Full/Data/tweets_conservative_party.csv", sep = '\t',quote = "")

miliband.text <- milliband$Text
#preprocessing
miliband.text <- list(miliband.text)
miliband.text <- lapply(miliband.text, removeHashTag)
miliband.text <- lapply(miliband.text, removeReference)
miliband.text <- lapply(miliband.text, removeURL)
miliband.df <- data.frame(miliband.text)
miliband.text <- miliband.df

colnames(miliband.text) <- c('Text')

bjp_class_emo = classify_emotion(milliband, algorithm="bayes", prior=1.0)
bjp_class_emo

#classify_polarity

bjp_class_pol = classify_polarity(miliband.text, algorithm="bayes")
bjp_class_pol
bjp_class_pol <- as.data.frame(bjp_class_pol)

milliband <- data.frame(bjp_class_pol$BEST_FIT, milliband$UserName, milliband$Date, milliband$Favorites, milliband$Retweets, milliband$Text)
colnames(milliband) <- c('Sentiment', 'UserName', 'Date', 'Favorites', 'Retweets', 'Text')

pol_df <- data.frame(bjp_class_pol, miliband.text)
colnames(pol_df) <- c("POS", "NEG", "POS.NEG", "Sentiment", "Text" )

write.csv(milliband, file = "~/TUE/Quartile1/IRandDM/SentimentAnalysis/WebIR-Full/Data/cameron_sentiment_full.csv", sep = ";")


