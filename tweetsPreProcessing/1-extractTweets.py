import sys

def main(filename,filenameTweetText,colName):
	f = open(filename, 'r+')
	fw = open(filenameTweetText,'w+')
	tweets = f.read().split('\n')

        cols = tweets[0].split(',')
        print len(cols)
        index = 0
        for i in range(len(cols)):
            print cols[i]
            print colName
            c = cols[i]
            if c[1:len(c)-1] == colName:
                
                print "matched"
                index = i
                break

        print index
	for tweet in tweets:
            t = tweet.split(',',len(cols) - 1)
            #print t
            #print len(t)
	    if len(t) > index:
	    	fw.write(t[index]+'\n')

if __name__ == "__main__":
	main(sys.argv[1],sys.argv[2],sys.argv[3])
		
