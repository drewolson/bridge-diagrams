module Bridge.Data.Unknown
  ( Unknown (..),
    areAllKnown,
    duplicates,
    hasUniqueEntries,
    knowns,
    showDuplicates,
  )
where

import Data.List (intercalate, nub, (\\))

class Unknown a where
  isUnknown :: a -> Bool

areAllKnown :: Unknown a => [a] -> Bool
areAllKnown = not . any isUnknown

knowns :: Unknown a => [a] -> [a]
knowns = filter (not . isUnknown)

hasUniqueEntries :: (Eq a, Unknown a) => [a] -> Bool
hasUniqueEntries as =
  let knownEntries = knowns as
   in knownEntries == nub knownEntries

duplicates :: (Eq a, Unknown a) => [a] -> [a]
duplicates as =
  let knownEntries = knowns as
   in nub $ knownEntries \\ nub knownEntries

showDuplicates :: (Eq a, Show a, Unknown a) => [a] -> String
showDuplicates = intercalate ", " . fmap show . duplicates
