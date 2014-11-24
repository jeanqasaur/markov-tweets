{-# LANGUAGE RecordWildCards #-}
import Control.Monad ( foldM_, unless, when )
import Data.Maybe ( fromMaybe )
import System.Console.GetOpt ( ArgDescr(..), ArgOrder(..), OptDescr(..), getOpt,
                               usageInfo )
import System.Environment ( getArgs )
import System.Exit ( ExitCode(..), exitWith )
import System.Random (newStdGen )

import MarkovTweets.Chain
import MarkovTweets.CleanTweets

data Options = Options { oInputFile :: String
                       , oOutputFile :: String
                       , oNumChars :: Int
                       , oPrefixLen :: Int
                       , oStripPunctuation :: Bool
                       , oNumTweets :: Int
                       , oHelp :: Bool
                       , oSilent :: Bool
                       }
  deriving(Show, Eq, Ord)

options :: [OptDescr (Options -> Options)]
options = [ Option "i" ["input"]
                (ReqArg
                    (\f opts -> opts { oInputFile = f })
                    "FILE")
                "A input file"
            , Option "o"  ["output"]
                  (ReqArg
                      (\f opts -> opts { oOutputFile = f })
                      "FILE")
                "A output file"
            , Option "c"  ["numchars"]
                  (OptArg
                      (\n opts -> opts { oNumChars = read (fromMaybe "140" n) })
                      "NUMCHARS")
                  "The number of characters in the output"
            , Option "p"  ["prefixlen"]
                  (OptArg
                      (\p opts -> opts { oPrefixLen = read (fromMaybe "2" p) })
                      "PREFIXLEN")
                  "The length of the chain's prefixes"
            , Option "s"  ["strip"]
                  (NoArg
                      (\opts -> opts { oStripPunctuation = True }))
                  "Strip punctuation"
            , Option "n"  ["number"]
                  (ReqArg
                      (\n opts -> opts { oNumTweets = read n })
                      "NUMTWEETS")
                  "The number of tweets to generate"
            , Option [] ["silent"]
                  (NoArg
                      (\opts -> opts { oSilent = True }))
                  "Don't print generated text to standard output"
            , Option "h"  ["help"]
                  (NoArg
                      (\opts -> opts { oHelp = True }))
                  "Display this message"
            ]

compilerOpts :: [String] -> IO ([Options -> Options], [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o, n, [])   -> return (o, n)
    (_, _, errs) -> ioError $ userError $
                      concat errs ++ usageInfo usageHeader options

usageHeader :: String
usageHeader = "Usage: ic [OPTIONS...] FILES..."

defaultOptions :: Options
defaultOptions = Options { oInputFile = error "No input file was provided"
                         , oOutputFile = error "No ouput file was provided"
                         , oNumChars = 140
                         , oPrefixLen = 2
                         , oStripPunctuation = False
                         , oNumTweets = 1
                         , oHelp = False
                         , oSilent = False
                         }

main :: IO ()
main = do
    -- Get command-line flags
    (opts, _) <- compilerOpts =<< getArgs
    let Options{..} = foldr ($) defaultOptions opts

    when oHelp $ do
        putStrLn (usageInfo usageHeader options)
        exitWith (ExitFailure 10)

    -- Read file
    input <- readFile oInputFile

    -- Initialize a new chain from input
    let chain = buildChain (tokenize input oStripPunctuation) oPrefixLen

    -- Create a new random number generator
    generator <- newStdGen

    -- Generate text and write output
    foldM_
        (\g _ -> do
            let (output, g') = generate chain oNumChars g
                cleanOutput  = cleanTweet output
            unless oSilent $ putStrLn cleanOutput
            appendFile oOutputFile (cleanOutput ++ "\n")
            return g')
        generator
        [1..oNumTweets]
