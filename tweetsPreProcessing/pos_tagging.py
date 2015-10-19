import sys
import nltk
from nltk import word_tokenize, pos_tag

TAGFILE = 'tags.csv'

def collectPOSTAGS():
	f = open(TAGFILE,'r+')
	tags = f.read().split('\n')
	print tags
	return tags

def main(filename):
	f = open(filename, 'r+')
	text = f.read()
	text = ''.join((c for c in text if 0 < ord(c) < 127))
	tweets = text.split('\n')
	tags = collectPOSTAGS()
	fwtags = open(filename.split('.')[0]+'-selectedtags.csv','w')
	for tweet in tweets:
		words = word_tokenize(tweet)
		taggedWords = pos_tag(words)
		taggedWords = [tag for tag in taggedWords if tag[1] in tags]
		bagofwords = ' '.join([w[0] for w in taggedWords])
		if tweet != '':
			fwtags.write(tweet+'>'+bagofwords+'\n')
	f.close()
	fwtags.close()

if __name__ == "__main__":
	main(sys.argv[1])
