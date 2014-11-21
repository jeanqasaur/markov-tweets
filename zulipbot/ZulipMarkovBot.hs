-- |
-- Author: Pedro Tacla Yamada
--
-- This is a Markov Chain persona bot. Given an input file, passed in as an
-- argument, it should spawn a Zulip Bot, connected to zulip through
-- credentials set to the `ZULIP_USER` and `ZULIP_KEY` environment
-- variables.
--
-- It can be built with:
-- @
--     cabal install --only-dep -fzulipbot`
--     cabal configure -fzulipbot`
--     cabal build
-- @
--
-- And then run with:
-- @
--     export ZULIP_USER="someuser"
--     export ZULIP_KEY="somekey"
--     export ZULIP_MESSAGE_PREFIX="@markovmeajoke"
--     cabal run -- someinput.txt
-- @
module Main
  where

import Control.Concurrent.Async.Lifted (async)
import Control.Exception (SomeException)
import Control.Monad (void, when)
import Control.Monad.Catch (catchAll)
import Data.List (intercalate, isPrefixOf)
import System.Environment (getArgs, getEnv)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr)
import System.Random (getStdGen, setStdGen)

import MarkovTweets.Chain
import MarkovTweets.CleanTweets
import Web.HZulip

main :: IO ()
main = withZulipEnv $ do
    lift $ putStrLn "Subscribing to all streams..."
    void addAllSubscriptions

    fs <- lift $ putStrLn "Loading input files" >>
                 loadInputFiles

    lift $ when (null fs) $ do
        hPutStrLn stderr "Usage: markov-zulip files..."
        exitWith $ ExitFailure 1

    lift $ putStrLn "Building chain..."
    let tokens = concatMap (`tokenize` False) fs
        chain = buildChain tokens 2

    lift $ putStrLn "Markov chaining..."

    messagePrefix <- lift $ getEnv "ZULIP_MESSAGE_PREFIX"
    catchAll (startRmser messagePrefix chain) onZulipError

  where loadInputFiles = getArgs >>= mapM readFile

startRmser :: String -> Chain -> ZulipM ()
startRmser messagePrefix chain = onNewMessage $ \msg -> do
    nr <- nonRecursive msg

    g  <- lift getStdGen
    let (dirtyGeneratedText, g') = generate chain 100 g
        generatedText            = cleanTweet dirtyGeneratedText
        content = messageContent msg
    lift $ setStdGen g'

    when (nr && messagePrefix `isPrefixOf` content) $ void $ async $ do
        r <- case messageType msg of
            "stream" ->
                let Left stream = messageDisplayRecipient msg
                    topic = messageSubject msg
                  in sendStreamMessage stream topic generatedText >>
                     return topic
            "private" ->
                let Right users = messageDisplayRecipient msg
                    recipients = map userEmail users
                  in sendPrivateMessage recipients generatedText >>
                     return (intercalate ", " recipients)
            t -> fail $ "Unrecognized message type " ++ t
        lift $ putStrLn $ "Sent " ++ generatedText ++ " to " ++ r

onZulipError :: SomeException -> ZulipM ()
onZulipError ex = lift $ putStrLn "Zulip Client errored:" >> print ex

nonRecursive :: Message -> ZulipM Bool
nonRecursive msg = do
    z <- ask
    return $ clientEmail z /= userEmail (messageSender msg)

withZulipEnv :: ZulipM a -> IO a
withZulipEnv action = do
    user <- getEnv "ZULIP_USER"
    key  <- getEnv "ZULIP_KEY"
    withZulipCreds user key action
