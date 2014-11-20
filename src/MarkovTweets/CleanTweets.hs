module MarkovTweets.CleanTweets
  where

import Text.Regex (subRegex, mkRegex)
import Text.Regex.Posix ((=~))

cleanTweet :: String -> String
cleanTweet = undefined

stripUnclosedParens :: String -> String
stripUnclosedParens str = subRegex re2 (subRegex re1 str "\\1\\2")
                                   "\\1\\2"
  where re1 = mkRegex "([^(]*)\\(([^)]+)?$"
        re2 = mkRegex "^([^(]*)\\)(.*)"

trimEnding :: String -> String
trimEnding str = undefined -- case findIndex endsWithPunctuation ws of
  where
    endsWithPunctuation w = endsWithPeriod w || endsWithComma w
    endsWithPeriod = (== '.') . last
    endsWithComma = (== ',') . last

    lastSentence = undefined

isLink :: String -> Bool
isLink str = str =~ "(https?://|www.).*"
