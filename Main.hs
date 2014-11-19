{-# LANGUAGE RecordWildCards #-}
import Data.Maybe ( fromMaybe )
import System.Console.GetOpt
import System.Environment ( getArgs )
import System.Random

import Chain
import Token

data Options = Options { oInputFile:: String, oOutputFile:: String
                       , oNumChars:: Int, oPrefixLen:: Int
                       , oStripPunctuation:: Bool }
  deriving Show

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['i']  ["input"]   (ReqArg (\f opts -> opts { oInputFile = f })
                                "FILE") "input FILE"
  , Option ['o']  ["output"]  (ReqArg (\f opts -> opts { oOutputFile = f })
                                "FILE")  "output FILE"
  , Option ['c']  ["numchars"] (OptArg (\n opts -> opts {
                                  oNumChars = read (fromMaybe "200" n) })
                                  "NUMCHARS") "numchars NUMCHARS"
  , Option ['p']  ["prefixlen"] (OptArg (\p opts -> opts {
                                  oPrefixLen = read (fromMaybe "2" p) })
                                  "PREFIXLEN") "prefix len PREFIXLEN"
  , Option ['s']  ["strip punctuation"] (NoArg (\opts -> opts {oStripPunctuation = True} )) "strip punctuation" ]

compilerOpts :: [String] -> IO ([Options -> Options], [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: ic [OPTION...] files..."

main :: IO ()
main = do
  -- Get command-line flags.
  (opts, unconsumed) <- compilerOpts =<< getArgs
  let Options{..} = foldr ($) (Options { oInputFile="", oOutputFile="", oNumChars=200, oPrefixLen=2, oStripPunctuation=False} ) opts

  -- Create a new random number generator.
  g <- newStdGen

  -- Read file.
  f <- readFile oInputFile
  
  -- Initialize a new chain from input.
  let chain = buildChain (tokenize f oStripPunctuation)
  -- Generate text.
  --
  -- Print text to output file.
  print "end"
