module Chain (Chain, buildChain) where

import Data.Char (isUpper)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import System.Random

type ChainMapping = Map.Map [String] [String]
data Chain = Chain { cMap:: ChainMapping, cPrefixLen:: Int }
  deriving Show

buildChain :: [String] -> Int -> Chain
buildChain bodyList prefixLen =
  Chain { cMap = buildChainHelper
                  Map.empty (replicate prefixLen "" ++ bodyList)
        , cPrefixLen = prefixLen }
  where
    buildChainHelper :: ChainMapping -> [String] -> ChainMapping
    buildChainHelper curMap [] = curMap
    buildChainHelper curMap curBodyList =
      if length curBodyList < prefixLen + 1
        then updateMap curMap (init curBodyList) (last curBodyList)
        else
          let newMap = updateMap curMap
                (take prefixLen curBodyList)
                (curBodyList !! prefixLen) in
            buildChainHelper newMap (tail curBodyList)
    updateMap :: ChainMapping -> [String] -> String -> ChainMapping
    updateMap curMap key elt =
      Map.insert key (elt:(fromMaybe [] (Map.lookup key curMap))) curMap

generate :: Chain -> Int -> StdGen -> String
generate chain maxlen g = undefined
  where
    capitalKeys = filter (isUpper . head . head) (Map.keys (cMap chain))
