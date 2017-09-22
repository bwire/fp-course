{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams :: Chars -> Filename -> IO (List Chars)
fastAnagrams word file = (<$>) ncString
    . flip filter (NoCaseString <$> permutations word) 
    . flip S.member
    . S.fromList 
    . hlist
    . (<$>) NoCaseString  
    . lines <$> readFile file
  

newtype NoCaseString = NoCaseString { ncString :: Chars } deriving Ord

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
