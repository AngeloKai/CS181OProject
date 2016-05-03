import unirest
from pprint import pprint
import json
import sys 

# These code snippets use an open-source library. http://unirest.io/python

# The first argument to this file should always be the word 
word = sys.argv[1] 
url = "https://wordsapiv1.p.mashape.com/words/" + word + "/synonyms"

# The second argument to this file should be the number of words returned
numberReturned = int(sys.argv[2])

response = unirest.get(url,
  headers={
    "X-Mashape-Key": "n4Ye8UfPWqmshLYGxmsdYcetqn6op1izWqojsnzFkaGBI5KiEA",
    "Accept": "application/json"
  }
)

html = response.body

result = (html['synonyms'])[numberReturned - 1]

if result:
	print result 
else:
	print "result has no response"