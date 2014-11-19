import Data.Maybe ( fromMaybe )
import System.Console.GetOpt
import System.Environment ( getArgs )

data Options = Options { outputFile:: String, numChars:: Int }
  deriving Show

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['o']  ["output"]  (ReqArg (\f opts -> opts { outputFile = f })
                                "FILE")  "output FILE"
  , Option ['c']  ["numchars"] (OptArg (\n opts -> opts {
                                  numChars = read (fromMaybe "200" n) })
                                  "NUM CHARS") "numchars NUM CHARS" ]

compilerOpts :: [String] -> IO ([Options -> Options], [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: ic [OPTION...] files..."

main :: IO ()
main = do
  (opts, unconsumed) <- compilerOpts =<< getArgs
  let options = foldr ($) (Options { outputFile="", numChars=200} ) opts
  print options
