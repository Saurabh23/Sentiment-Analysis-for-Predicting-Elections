import spellcheck
import sys;
import re;
import os;
import nltk;
from nltk import sent_tokenize, word_tokenize, pos_tag

reload(sys)
sys.setdefaultencoding('ascii')

#define a list of Stop Words to be removed from token list
STOPWORDS = ["a","an","the","is","are","this","that"]

#define regex patterns to remove from token list
REMOVEPATTERN = ['[: < > ! \. , \"]+','[\d]+','@[.]+']
REMOVETAGS = ['@[\w\n_]*','#[\w\n_].*','https?://[^\s<>"]+|www\.[^\s<>"]+']

#tweet counter
COUNT = 0

CMD = 'rm preprocessedTweets.csv'

def stopWordRemoval(tokenList):
	global COUNT
	COUNT = COUNT+1
	print 'tweet #',COUNT
	for sw in STOPWORDS:
		print "removing ", sw,"..."
		while sw in tokenList:
			tokenList.remove(sw)
	return tokenList

def cleanTokenList(tokenList):
	for regex in REMOVEPATTERN:
		tokenList = [val for val in tokenList if re.match(regex,val)==None]
	return tokenList	

def removeUsernames(tweets):
	for regex in REMOVETAGS:
		tweets = [re.sub(regex,'',t) for t in tweets]
	return tweets

def tokenize_text(text):
	tokenList = word_tokenize(text)
	return tokenList

def writeToFile(processedTweet, fn):
	f = open(fn+'.csv','a')
	print processedTweet
	f.write(processedTweet+'\n')

def removeNonAsciiChar(text):
	stripped = (c for c in text if 0 < ord(c) <127)
	return ''.join(stripped)

if __name__ == "__main__":
	f = open(sys.argv[1],'r')
	text = f.read()
	text = removeNonAsciiChar(text)
	tweetList = text.lower().split('\n')
	print tweetList
	tweetList = removeUsernames(tweetList)
	fn = sys.argv[1].split('.')[0]+'-preprocessed'
	os.system(CMD)
	for tweet in tweetList:
		print tweet
		tokenList = tokenize_text(tweet);
	##remove stop words but do not modify original words list..send a copy of words list
	#	tokenList = stopWordRemoval(list(tokenList))
	##remove unnecessary strings
		tokenListClean = cleanTokenList(tokenList)
		writeToFile(' '.join(tokenListClean),fn)
	#spellcheck.main(fn)
"""
tweets processing
"""



