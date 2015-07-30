{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S
import qualified Data.List as L

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Chars
  -> Filename
  -> IO (List Chars)
fastAnagrams s f = filter ((== k) . NoCaseString . listh . L.sort . hlist) . lines <$> readFile f
                   where k = NoCaseString $ listh $ L.sort (hlist s)


-- tonymorris' implementation, for comparison
--
--   λ> fastAnagrams "leaf" "/usr/share/dict/words"
--   ["alef","feal","flea","leaf"]
--   (3.22 secs, 2,134,406,464 bytes)
--   λ> tfastAnagrams "leaf" "/usr/share/dict/words"
--   ["leaf","alef","feal","flea"]
--   (8.20 secs, 1,342,759,448 bytes)

tfastAnagrams ::
  Chars
  -> Filename
  -> IO (List Chars)
tfastAnagrams name f =
  (flip (filter . flip S.member) (permutations name) . S.fromList . hlist . lines) <$> readFile f


newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
