import unirest
from pprint import pprint
import json
import sys 

# These code snippets use an open-source library. http://unirest.io/python

word = sys.argv[1] 
url = "https://wordsapiv1.p.mashape.com/words/" + word + "/hasInstances"

# The second argument to this file should be the number of words returned
numberReturned = int(sys.argv[2])

response = unirest.get(url,
  headers={
    "X-Mashape-Key": "n4Ye8UfPWqmshLYGxmsdYcetqn6op1izWqojsnzFkaGBI5KiEA",
    "Accept": "application/json"
  }
)

html = response.body
resultArr = html['hasInstances']

# result = (html['hasInstances'])[numberReturned - 1]

if resultArr:
	# An instance can be more than one word 
	for i in range(0, numberReturned):
		print resultArr[i]
else:
	print "result has no response"