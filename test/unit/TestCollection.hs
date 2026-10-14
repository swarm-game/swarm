{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Swarm unit tests for Collections
module TestCollection where

import Control.Lens.Indexed
import Data.Map.Ordered qualified as OM
import Swarm.ResourceLoading
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

newtype Blind a = Blind a

instance Show (Blind a) where
  show _ = "<blind>"

instance Eq a => Eq (Blind a) where
  (Blind x) == (Blind y) = x == y

testCollection :: TestTree
testCollection =
  testGroup
    "Collections"
    [ testCase "imap Collection" (assertEqual "imap failed" (Blind (imap (\k _ -> length k) coll)) (Blind coll))
    , testCase "itoList Collection" (assertEqual "itoList failed" (map fst (itoList coll)) ["foo.txt", "baz.txt", "dir1" </> "a", "dir1" </> "b", "dir1" </> "c", "dir2" </> "x", "dir2" </> "z" </> "zz"])
    ]
 where
  coll :: Collection Int
  coll =
    Collection . OM.fromList $
      [ ("foo.txt", Single 7)
      , ("baz.txt", Single 7)
      , ("dir1", SubCollection "stuff" coll1)
      , ("dir2", SubCollection "more stuff" coll2)
      ]

  coll1 :: Collection Int
  coll1 =
    Collection . OM.fromList $
      [ ("a", Single 6)
      , ("b", Single 6)
      , ("c", Single 6)
      ]

  coll2 :: Collection Int
  coll2 =
    Collection . OM.fromList $
      [ ("x", Single 6)
      , ("z", SubCollection "yet more" (Collection . OM.fromList $ [("zz", Single 9)]))
      ]
