import sys;
import re;
import os;
import nltk;
from nltk import sent_tokenize, word_tokenize, pos_tag

#define a list of Stop Words to be removed from token list
STOPWORDS = ["a","an","the","is","are"]

#define regex patterns to remove from token list
REMOVEPATTERN = ['[! \. ,]+','[\d]+']
REMOVETAGS = ['@.*','#.*']

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

def tokenize_text(text):
	tokenList = word_tokenize(text)
	return tokenList

def writeToFile(processedTweet):
	f = open('preprocessedTweets.csv','a')
	f.write(processedTweet+'\n')

if __name__ == "__main__":
	#text = '@mycola Hello dere #obama This is  Eindhoven university...lets say somethin abt d city eindhoven.!!!\n it is in nethelands nd  the city is vry nice!,, the weathr is soo cold nowaday!. and v have classes in d mornin. and so many assignmnts which takes most of our time whch realy sucks #barack @mark.'
	f = open(sys.argv[1],'r')
	text = f.read()
	#sys.exit(1)
	tweetList = text.lower().split('\n')
	os.system(CMD)
	for tweet in tweetList:
		print tweet
		tokenList = tokenize_text(tweet);
	##remove stop words but do not modify original words list..send a copy of words list
		tokenListNoSW = stopWordRemoval(list(tokenList))
	##remove unnecessary strings
		tokenListClean = cleanTokenList(tokenListNoSW)
		writeToFile(' '.join(tokenListClean))
"""
tweets processing
"""



