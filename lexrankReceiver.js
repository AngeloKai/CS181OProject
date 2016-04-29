// Getting this to work took almost a full day. Javascript gets really freaky
// when using it on terminal. 


/* Import necessary modules. */ 
var lexrank = require('./Lexrank/lexrank.js');
const readline = require('readline');
// var Type = require('type-of-is');
// var utf8 = require('utf8');


// Create readline interface, which needs to be closed in the end.
const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

// Set stdin and stdout to be encoded in utf8. Haskell passes string as basic 
// 8-bit unsigned integer array. The output also needs to be encoded so that 
// Haskell can read them
process.stdin.setEncoding('utf8');
process.stdout.setEncoding('utf8');

// If a string is readable, start reading. 
process.stdin.on('readable', () => {
  var chunk = process.stdin.read();

  if (chunk !== null) {

	var originalText = chunk;

	var topLines = lexrank.summarize(originalText, 5, function (err, toplines, text) {
	  if (err) {
	    console.log(err);
	  }

	  // Loop through the result to form a new paragraph consisted of most 
	  // important sentences in ascending order. Had to split the 0 index and
	  // the rest indices otherwise the first thing in newParagraphs will be
	  // undefined. 
	  var newParagraphs = (toplines[0])['text'];

	  for (var i = 1; i < toplines.length; i++) {
	  	newParagraphs += (toplines[i])['text'];
	  }

	  console.log(newParagraphs);

	});
  }
});

// After the output is finished, set end of file. 
// TODO: write a handler for end of writing output.
process.stdin.on('end', () => {
  process.stdout.write('\n');
});

// It is incredibly important to close readline. Otherwise, input doesn't 
// get sent out. 
rl.close();


