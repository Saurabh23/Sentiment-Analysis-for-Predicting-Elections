from stemming.porter2 import stem
import sys

file = open(sys.argv[1], 'r')
text = file.read()
document = text.split('\n')

document = [[stem(word) for word in sentence.split(" ")] for sentence in document]

outFileName = sys.argv[1].split('.')[0] + '-stemmed'

f = open(outFileName + '.csv', 'a')

for sentence in document:
    for word in sentence:
        f.write(word + " ")
    f.write('\n')

print "Finished Stemming."

f.close()
file.close()
