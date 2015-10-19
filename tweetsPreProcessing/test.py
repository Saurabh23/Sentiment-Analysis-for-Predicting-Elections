f = open('tweetsTextDavidCameron.csv','r')
text = f.read()
stripped = (c for c in text if 0 < ord(c) < 127)
print ''.join(stripped)
