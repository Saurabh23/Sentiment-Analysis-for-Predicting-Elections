library(sentiment)
#classify_emotion
milliband <- read.csv("~/TUE/IRandDM/SentimentAnalysis/WebIR-Full/Data/sample_milliband_tweets.csv", quote = "")

bjp_class_emo = classify_emotion(milliband, algorithm="bayes", prior=1.0)
bjp_class_emo

#classify_polarity



bjp_class_pol = classify_polarity(milliband, algorithm="bayes")
bjp_class_pol

pol_df <- data.frame(bjp_class_pol, milliband)
colnames(pol_df) <- c("POS", "NEG", "POS.NEG", "Sentiment", "Text" )

write.csv(pol_df, file = "~/TUE/IRandDM/SentimentAnalysis/WebIR-Full/Data/final_ed.csv", sep = ",")


