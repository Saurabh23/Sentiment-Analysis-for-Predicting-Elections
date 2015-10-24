import sys

def main(filename,filenameTweetText,colName):
	f = open(filename, 'r+')
	fw = open(filenameTweetText,'w+')
	tweets = f.read().split('\n')

        cols = tweets[0].split(',')
        print cols
        index = 0
        for i in range(0, len(cols) - 1):
            print cols[i]
            if cols[i] == colName:
                index = i
                break

        print index
	for tweet in tweets:
            t = tweet.split(',',len(cols) - 1)
            print t
            print len(t)
	    if len(t) > index:
	    	fw.write(t[index]+'\n')

if __name__ == "__main__":
	main(sys.argv[1],sys.argv[2],sys.argv[3])
		
