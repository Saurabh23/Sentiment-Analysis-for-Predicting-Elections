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
	tweets = text.split('/n')
	tags = collectPOSTAGS()
	print tags
	for tweet in tweets:
		words = word_tokenize(tweet)
		taggedWords = pos_tag(words)
		print taggedWords
		taggedWords = [tag for tag in taggedWords if tag[1] in tags]
		print taggedWords

if __name__ == "__main__":
	main(sys.argv[1])
