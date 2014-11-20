{-# LANGUAGE RecordWildCards #-}
import Control.Monad ( foldM_ )
import Data.Maybe ( fromMaybe )
import System.Console.GetOpt
import System.Environment ( getArgs )
import System.Random

import MarkovTweets.Chain
import MarkovTweets.CleanTweets
import MarkovTweets.Token

data Options = Options { oInputFile:: String, oOutputFile:: String
                       , oNumChars:: Int, oPrefixLen:: Int
                       , oStripPunctuation:: Bool
                       , oNumTweets:: Int }
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
                  (\n opts -> opts { oNumChars = read (fromMaybe "140" n) })
                  "NUMCHARS")
              "The number of characters in the output"
          , Option "p"  ["prefixlen"]
              (OptArg
                  (\p opts -> opts { oPrefixLen = read (fromMaybe "2" p) })
                  "PREFIXLEN")
              "prefix len PREFIXLEN"
          , Option "s"  ["strip punctuation"]
              (NoArg
                  (\opts -> opts { oStripPunctuation = True } ))
                  "strip punctuation"
          , Option "n"  ["number of tweets"]
              (ReqArg
                (\n opts -> opts { oNumTweets = read n } )
                "NUMTWEETS")
              "number of tweets NUMTWEETS" ]

compilerOpts :: [String] -> IO ([Options -> Options], [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o, n, [])   -> return (o, n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: ic [OPTION...] files..."

defaultOptions :: Options
defaultOptions = Options { oInputFile=""
                         , oOutputFile=""
                         , oNumChars=140
                         , oPrefixLen=2
                         , oStripPunctuation=False
                         , oNumTweets=1
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

  -- Generate text and write output.
  foldM_
    (\g _ ->
      let (output, g') = generate chain oNumChars g
          cleanOutput  = cleanTweet output in
        do  putStrLn cleanOutput
            appendFile oOutputFile (cleanOutput ++ "\n")
            return g')
    generator
    [1..oNumTweets]
