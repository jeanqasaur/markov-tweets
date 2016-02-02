markov-tweets
=============
This code uses a simple Markov chain algorithm to generate Tweets from emails, or really any kind of text. (You may be interested in using this with this (Python script)[https://github.com/jeanqasaur/csail-archive-scraper] I wrote to collect all emails from MIT CSAIL's csail-related mailing list.) Generating Tweets is a little more tricky than generating regular text because of the character limitation. Because of this we have extra functionality for cleaning up Tweets, especially at the end, to help them make a little more sense.

To run, you need GHC and Cabal installed. To build:

```
cabal configure && cabal build
```

Example of arguments for running:

```
cabal run -- -iin.txt -oout.txt -n50 -p3
```
General usage:

```
Usage: ic [OPTION...] files...
  -i FILE        --input=FILE                  A input FILE
  -o FILE        --output=FILE                 A output FILE
  -c[NUMCHARS]   --numchars[=NUMCHARS]         The number of characters in the output
  -p[PREFIXLEN]  --prefixlen[=PREFIXLEN]       prefix len PREFIXLEN
  -s             --strip punctuation           strip punctuation
  -n NUMTWEETS   --number of tweets=NUMTWEETS  number of tweets NUMTWEETS
```

## Donations
Would you like to buy me a beer? Send bitcoin to 3JjxJydvoJjTrhLL86LGMc8cNB16pTAF3y
