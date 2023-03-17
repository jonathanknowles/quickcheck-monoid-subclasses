{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{- HLINT ignore "Redundant bracket" -}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
module Internal.Semigroup.Tuple
    where

import Data.List.NonEmpty
    ( NonEmpty (..) )
import GHC.Generics
    ( Generic )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , applyArbitrary4
    , choose
    , genericShrink
    , shuffle
    , suchThatMap
    )
import Text.Show.Pretty
    ( ppShow )

import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Semigroup.Foldable as F1

--------------------------------------------------------------------------------
-- Tuple selectors
--------------------------------------------------------------------------------

data Variable
    = A
    | B
    | C
    | D
    deriving (Bounded, Enum, Eq, Ord, Show)

bindVariable :: BindingSet s -> Variable -> s
bindVariable BindingSet {bindingForA} A = bindingForA
bindVariable BindingSet {bindingForB} B = bindingForB
bindVariable BindingSet {bindingForC} C = bindingForC
bindVariable BindingSet {bindingForD} D = bindingForD

--------------------------------------------------------------------------------
-- Semigroup combinations
--------------------------------------------------------------------------------

newtype VariableSum = VariableSum (NonEmpty Variable)
    deriving (Eq, Ord, Show)

a = VariableSum (A :| [])
b = VariableSum (B :| [])
c = VariableSum (C :| [])
d = VariableSum (D :| [])

arbitraryVariableSum :: Gen VariableSum
arbitraryVariableSum =
    VariableSum <$> arbitraryVariableList `suchThatMap` NE.nonEmpty
  where
    arbitraryVariableList :: Gen [Variable]
    arbitraryVariableList = do
        itemCount <- choose (1, 4)
        take itemCount <$> shuffle universe

bindVariableSum :: BindingSet s -> VariableSum -> NonEmpty s
bindVariableSum tuple (VariableSum selectors) =
    bindVariable tuple <$> selectors

evalVariableSum :: Semigroup s => BindingSet s -> VariableSum -> s
evalVariableSum = (F1.fold1 .) . bindVariableSum

showVariableSum :: Show s => (BindingSet s) -> VariableSum -> String
showVariableSum tuple =
    F1.intercalateMap1 " <> " show . bindVariableSum tuple

--------------------------------------------------------------------------------
-- Semigroup tuples
--------------------------------------------------------------------------------

data BindingSet s = BindingSet
    { bindingForA :: s
    , bindingForB :: s
    , bindingForC :: s
    , bindingForD :: s
    }
    deriving (Eq, Generic, Ord)

instance Arbitrary s => Arbitrary (BindingSet s) where
    arbitrary = applyArbitrary4 BindingSet
    shrink = genericShrink

data Tuple1 s = Tuple1 VariableSum (BindingSet s)
    deriving (Eq, Ord)

data Tuple2 s = Tuple2 VariableSum VariableSum (BindingSet s)
    deriving (Eq, Ord)

data Tuple3 s = Tuple3 VariableSum VariableSum VariableSum (BindingSet s)
    deriving (Eq, Ord)

instance Arbitrary a => Arbitrary (Tuple1 a) where
    arbitrary = arbitraryTuple1
    shrink = shrinkTuple1

instance Arbitrary a => Arbitrary (Tuple2 a) where
    arbitrary = arbitraryTuple2
    shrink = shrinkTuple2

instance Arbitrary a => Arbitrary (Tuple3 a) where
    arbitrary = arbitraryTuple3
    shrink = shrinkTuple3

instance (Show s, Semigroup s) => Show (Tuple1 s) where
    show = showTuple1

instance (Show s, Semigroup s) => Show (Tuple2 s) where
    show = showTuple2

instance (Show s, Semigroup s) => Show (Tuple3 s) where
    show = showTuple3

arbitraryTuple1 :: Arbitrary a => Gen (Tuple1 a)
arbitraryTuple1 = Tuple1
    <$> arbitraryVariableSum
    <*> arbitrary

arbitraryTuple2 :: Arbitrary a => Gen (Tuple2 a)
arbitraryTuple2 = Tuple2
    <$> arbitraryVariableSum
    <*> arbitraryVariableSum
    <*> arbitrary

arbitraryTuple3 :: Arbitrary a => Gen (Tuple3 a)
arbitraryTuple3 = Tuple3
    <$> arbitraryVariableSum
    <*> arbitraryVariableSum
    <*> arbitraryVariableSum
    <*> arbitrary

evalTuple1 :: Semigroup s => Tuple1 s -> s
evalTuple1 (Tuple1 c1 t) =
    ( evalVariableSum t c1
    )

evalTuple2 :: Semigroup s => Tuple2 s -> (s, s)
evalTuple2 (Tuple2 c1 c2 t) =
    ( evalVariableSum t c1
    , evalVariableSum t c2
    )

evalTuple3 :: Semigroup s => Tuple3 s -> (s, s, s)
evalTuple3 (Tuple3 c1 c2 c3 t) =
    ( evalVariableSum t c1
    , evalVariableSum t c2
    , evalVariableSum t c3
    )

showTuple1 :: (Semigroup a, Show a) => Tuple1 a -> String
showTuple1 (evalTuple1 -> va) = unlines
    [ mempty, "a:", showWrap va
    ]

showTuple2 :: (Semigroup a, Show a) => Tuple2 a -> String
showTuple2 (evalTuple2 -> (va, vb)) = unlines
    [ mempty, "a:", showWrap va
    , mempty, "b:", showWrap vb
    ]

showTuple3 :: (Semigroup a, Show a) => Tuple3 a -> String
showTuple3 (evalTuple3 -> (va, vb, vc)) = unlines
    [ mempty, "a:", showWrap va
    , mempty, "b:", showWrap vb
    , mempty, "c:", showWrap vc
    ]

shrinkTuple1 :: Arbitrary a => Tuple1 a -> [Tuple1 a]
shrinkTuple1 (Tuple1 c1 t) = Tuple1 c1 <$> shrink t

shrinkTuple2 :: Arbitrary a => Tuple2 a -> [Tuple2 a]
shrinkTuple2 (Tuple2 c1 c2 t) = Tuple2 c1 c2 <$> shrink t

shrinkTuple3 :: Arbitrary a => Tuple3 a -> [Tuple3 a]
shrinkTuple3 (Tuple3 c1 c2 c3 t) = Tuple3 c1 c2 c3 <$> shrink t

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

showWrap :: Show a => a -> String
showWrap x
    | singleLineMaxLengthExceeded =
        multiLine
    | otherwise =
        singleLine
  where
    multiLine = ppShow x
    singleLine = show x
    singleLineMaxLength = 80
    singleLineMaxLengthExceeded = F.length singleLine > singleLineMaxLength

universe :: (Bounded a, Enum a) => [a]
universe = [minBound .. maxBound]
