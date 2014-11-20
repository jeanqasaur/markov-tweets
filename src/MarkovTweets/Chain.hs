module MarkovTweets.Chain (Chain, buildChain, generate) where

import Data.Char (isUpper)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import System.Random

type Chain = Map.Map NGram [String]
type NGram = [String]

buildChain :: [String] -> Int -> Chain
buildChain bodyList prefixLen =
  buildChainHelper Map.empty (replicate prefixLen "" ++ bodyList)
  where
    buildChainHelper :: Chain -> [String] -> Chain
    buildChainHelper curMap [] = curMap
    buildChainHelper curMap curBodyList =
      if length curBodyList < prefixLen + 1
        then updateMap curMap (init curBodyList) (last curBodyList)
        else
          let newMap = updateMap curMap
                (take prefixLen curBodyList)
                (curBodyList !! prefixLen) in
            buildChainHelper newMap (tail curBodyList)
    updateMap :: Chain -> [String] -> String -> Chain
    updateMap curMap key elt =
      Map.insert key (elt:(fromMaybe [] (Map.lookup key curMap))) curMap

generate :: Chain -> Int -> StdGen -> (String, StdGen)
generate chain maxlen g = loop fstNg g' (unwords fstNg)
  where
    (fstNg, g') = getFirstNGram g
    loop :: NGram -> StdGen -> String -> (String, StdGen)
    loop _ g acc | length acc > maxlen = (unwords $ init $ words acc, g)
    loop ng g acc = case getNextNGram ng g of
        Just (word, ng', g') ->
            if length acc + length word > maxlen
                 then (acc, g')
                 else loop ng' g' (acc ++ " " ++ word)
        Nothing -> (acc, g)

    capitalKeys = filter isStartNGram (Map.keys chain)
      where
        isStartNGram (s:ss) = not (null s) && isUpper (head s)

    getFirstNGram :: StdGen -> (NGram, StdGen)
    getFirstNGram g = randomElem g capitalKeys

    getNextNGram :: NGram -> StdGen -> Maybe (String, NGram, StdGen)
    getNextNGram ng g = case getNextWord ng g of
        Just (word, g') -> Just (word, tail ng ++ [word], g')
        Nothing -> Nothing

      where
        getNextWord :: NGram -> StdGen -> Maybe (String, StdGen)
        getNextWord ng g = randomElem g `fmap` Map.lookup ng chain

randomElem :: RandomGen g => g -> [a] -> (a, g)
randomElem g xs = let (i, g') = randomR (0, length xs - 1) g
                    in (xs !! i, g')

{-

g <- newStdGen

let c = buildChain (words "People are cool and all here cla cla I am here Pedro Hacker School ") 2

generate c 100 g

-}
