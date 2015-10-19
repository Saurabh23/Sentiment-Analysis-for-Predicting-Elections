import sys

def main(filename,filenameTweetText):
	f = open(filename, 'r+')
	fw = open(filenameTweetText,'w+')
	tweets = f.read().split('\n')
	for tweet in tweets:
		t = tweet.split(';',4)
		if len(t)==5:
			fw.write(t[4]+'\n')
	

if __name__ == "__main__":
	main(sys.argv[1],sys.argv[2])
		
