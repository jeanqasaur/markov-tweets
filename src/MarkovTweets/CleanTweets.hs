module MarkovTweets.CleanTweets
  where

import Data.Char (isAlpha)
import Data.List (findIndex, splitAt)
import Text.Regex (subRegex, mkRegex)
import Text.Regex.Posix ((=~))

cleanTweet :: String -> String
cleanTweet = trimEnding . stripUnclosedParens

stripUnclosedParens :: String -> String
stripUnclosedParens str = subRegex re2 (subRegex re1 str "\\1\\2")
                                   "\\1\\2"
  where re1 = mkRegex "([^(]*)\\(([^)]+)?$"
        re2 = mkRegex "^([^(]*)\\)(.*)"

-- | If string ends with punctuation, we leave it be.
-- Otherwise we add "..." maybe taking off the last word if we are running out
-- of room.
trimEnding :: String -> String
trimEnding str =
  if endsWithSentenceEnd str || endsWithLink str
    then str
    else
      let (beginning, end) = lastSentence str in
        if (length (words end)) < 3
          then if end == ""
                then fixEndingSyntax beginning
                else beginning
          else fixEndingSyntax str
  where
    endsWithSentenceEnd = isSentenceEnd . last
    endsWithPunctuation w = endsWithPeriod w || endsWithComma w
    endsWithPeriod = (== '.') . last
    endsWithComma = (== ',') . last
    endsWithLink = isLink . reverse . takeWhile (/= ' ') . reverse
      where
        isLink :: String -> Bool
        isLink str = str =~ "(https?://|www.).*"

    -- | Find the last sentence.
    lastSentence str =
      case findIndex isSentenceEnd (reverse str) of
        Just i -> splitAt ((length str) - i) str
        Nothing -> (str, "")
    isSentenceEnd c =
      c /= ',' && c /= ' ' && c /= '\'' && not (isAlpha c)

    -- Makes sure ends with valid sentence ender, link, or "..."
    fixEndingSyntax str =
      let lastc = last str in
        if lastc == ','
          then init str ++ "."
          else
            if length str > 137
              then fixEndingSyntax (unwords (init (words str)))
              else str ++ "..."
