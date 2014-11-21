-- |
-- Cleans generated text for happy tweeting.
--
-- (This module isn't really designed to be general, so the only reason
-- these aren't general text cleaning functions is the expected length of
-- the output string is 140 @chars@ and that fact is used on a cleaning
-- rule. TODO - fix that.)
module MarkovTweets.CleanTweets
  where

import Data.Char ( isAlpha )
import Data.List ( findIndex )
import Text.Regex ( subRegex, mkRegex )
import Text.Regex.Posix ( (=~) )

-- |
-- Cleans a generated tweet
cleanTweet :: String -> String
cleanTweet = trimEnding . stripUnclosedParens

-- |
-- Removes unopened/unclosed parens from a string.
-- TODO - Generalize this to all symbols; should be as simple as tweaking
-- the RegExps used.
stripUnclosedParens :: String -> String
stripUnclosedParens str = subRegex re2 (subRegex re1 str "\\1\\2") "\\1\\2"
  where re1 = mkRegex "([^(]*)\\(([^)]+)?$"
        re2 = mkRegex "^([^(]*)\\)(.*)"

-- |
-- If string ends with punctuation, we leave it be.
-- Otherwise we add "..." maybe taking off the last word if we are running out
-- of room.
trimEnding :: String -> String
trimEnding str | strHasGoodEnding = str
               | endIsTooShort    = beginning
               | otherwise        = fixEndingSyntax str
  where
    strHasGoodEnding = endsWithSentenceEnd str || endsWithLink str
    endIsTooShort = not (null end) && length (words end) < 3

    -- Splits @str@ into @end@ (the last sentence) and @beginning@ the
    -- text preceding it
    (beginning, end) = case findIndex isSentenceEnd (reverse str) of
        Just i -> splitAt (length str - i) str
        Nothing -> (str, "")

    -- Makes sure a string ends with valid sentence ender, link, or "..."
    fixEndingSyntax s | last s == ','   = init s ++ "."
                      | length s <= 137 = s ++ "..."
                      | otherwise       = fixEndingSyntax $ trimLastWord s
      where
        trimLastWord = unwords . init . words

    endsWithLink = isLink . reverse . takeWhile (/= ' ') . reverse
    endsWithSentenceEnd = isSentenceEnd . last

    isLink = (=~ "(https?://|www.).*")
    isSentenceEnd c = c /= ',' && c /= ' ' && c /= '\'' && not (isAlpha c)
