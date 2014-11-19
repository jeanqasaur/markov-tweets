{-# LANGUAGE RecordWildCards #-}
import Data.Maybe ( fromMaybe )
import System.Console.GetOpt
import System.Environment ( getArgs )
import System.Random

import MarkovTweets.Chain
import MarkovTweets.Token

data Options = Options { oInputFile:: String, oOutputFile:: String
                       , oNumChars:: Int, oPrefixLen:: Int
                       , oStripPunctuation:: Bool }
  deriving Show

options :: [OptDescr (Options -> Options)]
options = [ Option "i" ["input"]
              (ReqArg
                  (\f opts -> opts { oInputFile = f }) "FILE")
              "A input FILE"
          , Option "o"  ["output"]
              (ReqArg
                  (\f opts -> opts { oOutputFile = f }) "FILE")
              "A output FILE"
          , Option "c"  ["numchars"]
              (OptArg
                  (\n opts -> opts { oNumChars = read (fromMaybe "200" n) })
                  "NUMCHARS")
              "The number of characters in the output"
          , Option "p"  ["prefixlen"]
              (OptArg
                  (\p opts -> opts { oPrefixLen = read (fromMaybe "2" p) })
                  "PREFIXLEN")
              "prefix len PREFIXLEN"
          , Option "s"  ["strip punctuation"]
              (NoArg
                  (\opts -> opts {oStripPunctuation = True} ))
                  "strip punctuation" ]

compilerOpts :: [String] -> IO ([Options -> Options], [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o, n, [])   -> return (o, n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: ic [OPTION...] files..."

defaultOptions :: Options
defaultOptions = Options { oInputFile=""
                         , oOutputFile=""
                         , oNumChars=200
                         , oPrefixLen=2
                         , oStripPunctuation=False
                         }

main :: IO ()
main = do
  -- Get command-line flags
  (opts, _) <- compilerOpts =<< getArgs
  let Options{..} = foldr ($) defaultOptions opts

  -- Read file.
  input <- readFile oInputFile

  -- Initialize a new chain from input.
  let chain = buildChain (tokenize input oStripPunctuation) oPrefixLen

  -- Create a new random number generator.
  generator <- newStdGen

  -- Generate text.
  let output = generate chain oNumChars generator

  -- Print text to output file.
  putStrLn output
  writeFile oOutputFile output
