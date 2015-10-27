# function to calculate the sentiment score
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    sentence <- iconv(sentence, "WINDOWS-1252","UTF-8")
    # and convert to lower case:
    sentence = tolower(sentence)
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress )
}

cameron.tweets <- cameron$Text
milliband.tweets <- milliband$Text

scores <- score.sentiment(cameron.tweets, pos, neg)
scores <- score.sentiment(milliband.tweets, pos, neg)
scores.df <- data.frame(scores)
write.csv(scores.df, "~/TUE/IRandDM/SentimentAnalysis/cameron_scored_based.csv")
write.csv(scores.df, "~/TUE/IRandDM/SentimentAnalysis/milliband_scored_based.csv")

len <- length(scores.df)
count <- 0
for (i in 1 : len) {
  count <- count + scores[i]
}
