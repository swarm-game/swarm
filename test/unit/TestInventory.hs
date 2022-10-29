{-# LANGUAGE OverloadedStrings #-}

-- | Swarm unit tests
module TestInventory where

import Control.Lens ((^.))
import Data.Hashable
import Swarm.Game.Display
import Swarm.Game.Entity (Number (..))
import Swarm.Game.Entity qualified as E
import Test.QuickCheck (NonNegative (..), (===))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)

testInventory :: TestTree
testInventory =
  testGroup
    "Inventory"
    [ testCase
        "insert 0 / hash"
        ( assertEqual
            "insertCount 0 x empty has same hash as (1+1+0)*x"
            (2 * x ^. E.entityHash)
            (hash (E.insertCount 0 x E.empty))
        )
    , testCase
        "insert 1 / hash"
        ( assertEqual
            "insert x empty has same hash as (1+1+1)*x"
            (3 * (x ^. E.entityHash))
            (hash (E.insert x E.empty))
        )
    , testCase
        "insert 2 / hash"
        ( assertEqual
            "insertCount 2 x empty has same hash as (1+1+2)*x"
            (4 * x ^. E.entityHash)
            (hash (E.insertCount 2 x E.empty))
        )
    , testCase
        "insert / insert"
        ( assertEqual
            "insert x y gives same hash as insert y x"
            (hash (E.insert x (E.insert y E.empty)))
            (hash (E.insert y (E.insert x E.empty)))
        )
    , testProperty
        "insert i x (insert j x) has same hash as insert (i+j) x"
        ( \(NonNegative i) (NonNegative j) ->
            hash (E.insertCount (Integer i) x (E.insertCount (Integer j) x E.empty))
              === hash (E.insertCount (Integer $ i + j) x E.empty)
        )
    , testProperty
        "insert i x (insert infinity x) has same hash as insert infinity x"
        ( \(NonNegative i) ->
            hash (E.insertCount (Integer i) x (E.insertCount PosInfinity x E.empty))
              === hash (E.insertCount PosInfinity x E.empty)
        )
    , testCase
        "insert 2 / delete"
        ( assertEqual
            "insert 2, delete 1 gives same hash as insert 1"
            (hash (E.insert x E.empty))
            (hash (E.delete x (E.insertCount 2 x E.empty)))
        )
    , testCase
        "insert 2 / delete 3"
        ( assertEqual
            "insert 2, delete 3 has the same hash as (1+1)*x"
            (2 * x ^. E.entityHash)
            (hash (E.deleteCount 3 x (E.insertCount 2 x E.empty)))
        )
    , testProperty
        "delete i x (insert infinity x) has same hash as insert infinity x"
        ( \(NonNegative i) ->
            hash (E.deleteCount (Integer i) x (E.insertCount PosInfinity x E.empty))
              === hash (E.insertCount PosInfinity x E.empty)
        )
    , testCase
        "deleteAll x (insert x) is same as insertCount 0 empty"
        ( assertEqual
            "insert 2 x, deleteAll x same hash as insert 0 x"
            (hash (E.insertCount 0 x E.empty))
            (hash (E.deleteAll x (E.insertCount 2 x E.empty)))
        )
    , testCase
        "deleteAll x (insertCount infinity x) is same as insertCount 0 empty"
        ( assertEqual
            "insert infinity x, deleteAll x same hash as insert 0 x"
            (hash (E.insertCount 0 x E.empty))
            (hash (E.deleteAll x (E.insertCount PosInfinity x E.empty)))
        )
    , testCase
        "deleteAll x [x: 2, y: 2] --> [x: 0, y: 2]"
        ( assertEqual
            "insert 2 x, insert 2 y, deleteAll x same hash as insert 2 y, insert 0 x"
            (hash (E.insertCount 0 x (E.insertCount 2 y E.empty)))
            (hash (E.deleteAll x (E.insertCount 2 y (E.insertCount 2 x E.empty))))
        )
    , testCase
        "union"
        ( assertEqual
            "insert 2 x union insert 3 x same as insert 5 x"
            (hash (E.insertCount 5 x E.empty))
            (hash (E.union (E.insertCount 2 x E.empty) (E.insertCount 3 x E.empty)))
        )
    , testCase
        "difference"
        ( assertEqual
            "{(2,x),(3,y)} difference {(3,x),(1,y)} = {(0,x), (2,y)}"
            ( hash
                ( E.insertCount 2 x (E.insertCount 3 y E.empty)
                    `E.difference` E.insertCount 3 x (E.insertCount 1 y E.empty)
                )
            )
            (hash (E.insertCount 0 x (E.insertCount 2 y E.empty)))
        )
    , testCase
        "subset / yes"
        ( assertBool
            "{(0,x),(3,y),(2,z)} isSubsetOf {(3,y),(4,z)}"
            ( E.insertCount 0 x (E.insertCount 3 y (E.insertCount 2 z E.empty))
                `E.isSubsetOf` E.insertCount 3 y (E.insertCount 4 z E.empty)
            )
        )
    , testCase
        "subset / no"
        ( assertBool
            "{(2,x),(3,y)} isSubsetOf {(1,x),(4,y)}"
            ( not
                ( E.insertCount 2 x (E.insertCount 3 y E.empty)
                    `E.isSubsetOf` E.insertCount 1 x (E.insertCount 4 y E.empty)
                )
            )
        )
    , testCase
        "isEmpty / yes"
        ( assertBool
            "isEmpty {(0,x),(0,y)}"
            (E.isEmpty (E.insertCount 0 x (E.insertCount 0 y E.empty)))
        )
    , testCase
        "isEmpty / no"
        ( assertBool
            "isEmpty {(0,x),(1,y)}"
            (not (E.isEmpty (E.insertCount 0 x (E.insertCount 1 y E.empty))))
        )
    ]
 where
  x = E.mkEntity (defaultEntityDisplay 'X') "fooX" [] [] []
  y = E.mkEntity (defaultEntityDisplay 'Y') "fooY" [] [] []
  z = E.mkEntity (defaultEntityDisplay 'Z') "fooZ" [] [] []
