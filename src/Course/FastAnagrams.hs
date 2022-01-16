{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Chars
  -> FilePath
  -> IO (List Chars)
fastAnagrams cs fp = getAllAnagrams <$> readFile fp where
  getAllAnagrams f = listh . S.toList . S.map ncString $ S.intersection allCharPermutations (allDictWords f)
  allCharPermutations = setFromList $ NoCaseString <$> permutations cs
  allDictWords dict = setFromList $ NoCaseString <$> lines dict
  setFromList = S.fromList . hlist

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  } deriving Ord

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
