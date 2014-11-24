-- |
-- Builds markov chains out of lists of words and generates text given
-- built chains and random generators
module MarkovTweets.Chain (
                            Chain
                          , NGram
                          , buildChain
                          , buildChainFromString
                          , capitalKeys
                          , generate
                          , generate'
                          , tokenize
                          ) where

import Data.Char ( isAlphaNum, isUpper )
import qualified Data.Map.Strict as Map ( Map, empty, insertWith, keys, lookup )
import System.Random ( RandomGen, StdGen, randomR )

type Chain = Map.Map NGram [String]
type NGram = [String]

-- |
-- Builds a Markov chain out of a list of words and a prefix length
buildChain :: [String] -> Int -> Chain
buildChain bodyList prefixLen =
    let b  = replicate prefixLen "" ++ bodyList
        ls = buildChainHelper [] b (length b)
      in foldr insertInChain  Map.empty ls
  where
    insertInChain (k, e) = Map.insertWith (\[x] xs -> x:xs) k [e]

    buildChainHelper acc [] _ = acc
    buildChainHelper acc curBodyList l =
      if l < prefixLen + 1
        then updateAcc acc (init curBodyList) (last curBodyList)
        else
          let newMap = updateAcc acc
                (take prefixLen curBodyList)
                (curBodyList !! prefixLen) in
            buildChainHelper newMap (tail curBodyList) (l - 1)

    updateAcc acc key elt = (key, elt):acc

-- |
-- Short-hand for @buildChain . (`tokenize` False)@
buildChainFromString :: String -> Int -> Chain
buildChainFromString = buildChain . (`tokenize` False)

-- |
-- Generates text by randomly walking throgh a Markov chain.
-- Returns the generated `String` and the new random generator
generate :: Chain  -- ^ A markov chain, representing "likely" sequences of text
         -> Int    -- ^ A character limit for the output
         -> StdGen -- ^ A random generator
                   -- (could be generalized to `RandomGen g => g` but yeah...)
         -> (String, StdGen)
generate chain = generate' (capitalKeys chain) chain

-- |
-- Version of `generate` which takes the sentence starting NGrams
-- from a map as an argument, for generating several text nodes more
-- efficiently
generate' :: [NGram] -> Chain -> Int -> StdGen -> (String, StdGen)
generate' ckeys chain maxlen generator =
    let (firstNg, generator') = getFirstNGram generator
        startStr = unwords firstNg
      in loop firstNg generator' startStr
  where
    loop _ g acc | length acc > maxlen = (unwords $ init $ words acc, g)
    loop ng g acc = case getNextWord ng g of
        Just (word, ng', g') ->
            if length acc + length word > maxlen
                then (acc, g')
                else loop ng' g' (acc ++ " " ++ word)
        Nothing -> (acc, g)

    -- Gets the seed ngram, we should start with, by randomly choosing
    -- a ngram corresponding to the start of a sentence
    getFirstNGram g = randomElem g ckeys

    -- Gets the next word and ngram to be used to generate text, given the
    -- previous ngram and a random generator.
    getNextWord ng g =
        let nextWord = randomElem g `fmap` Map.lookup ng chain
          in case nextWord of
              Just (word, g') -> Just (word, tail ng ++ [word], g')
              Nothing -> Nothing

-- |
-- Gets all keys corresponding to sentence starting ngrams
capitalKeys :: Map.Map NGram a -> [NGram]
capitalKeys chain = filter isStartNGram (Map.keys chain)
  where
    isStartNGram [] = False
    isStartNGram (s:_) = not (null s) && isUpper (head s)


-- |
-- Randomly chooses an element from a list.
-- Returns the choosen element and the new random generator
randomElem :: RandomGen g
           => g      -- ^ A random generator
           -> [a]    -- ^ A list
           -> (a, g)
randomElem g xs = let (i, g') = randomR (0, length xs - 1) g
                    in (xs !! i, g')

-- |
-- Helper for building a list of words from a string and maybe filtering non
-- alphanumeric characters from the output
tokenize :: String -- ^ An input string
         -> Bool   -- ^ Whether to strip non-alphanumeric chars
         -> [String]
tokenize s True  = map (filter isAlphaNum) $ words s
tokenize s False = words s
