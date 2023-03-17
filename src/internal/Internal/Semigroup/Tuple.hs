{- HLINT ignore "Redundant bracket" -}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
module Internal.Semigroup.Tuple
    where

import Data.List.NonEmpty
    ( NonEmpty (..) )
import Test.QuickCheck
    ( Arbitrary (..), Gen, choose, shuffle, suchThatMap )
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
    deriving (Bounded, Enum, Eq, Ord, Show)

evalVariable :: (s, s, s) -> Variable -> s
evalVariable (a, b, c) = \case
    A -> a
    B -> b
    C -> c

--------------------------------------------------------------------------------
-- Semigroup combinations
--------------------------------------------------------------------------------

newtype VariableSum = VariableSum (NonEmpty Variable)
    deriving (Eq, Ord, Show)

arbitraryVariableSum :: Gen VariableSum
arbitraryVariableSum =
    VariableSum <$> arbitraryTupleLensList `suchThatMap` NE.nonEmpty
  where
    arbitraryTupleLensList :: Gen [Variable]
    arbitraryTupleLensList = do
        itemCount <- choose (1, 3)
        take itemCount <$> shuffle universe

evalVariableSum :: (s, s, s) -> VariableSum -> NonEmpty s
evalVariableSum tuple (VariableSum selectors) =
    evalVariable tuple <$> selectors

showVariableSum :: Show s => (s, s, s) -> VariableSum -> String
showVariableSum tuple =
    F1.intercalateMap1 " <> " show . evalVariableSum tuple

--------------------------------------------------------------------------------
-- Semigroup tuples
--------------------------------------------------------------------------------

data Tuple1 s = Tuple1 VariableSum (s, s, s)
    deriving (Eq, Ord)

data Tuple2 s = Tuple2 VariableSum VariableSum (s, s, s)
    deriving (Eq, Ord)

data Tuple3 s = Tuple3 VariableSum VariableSum VariableSum (s, s, s)
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
    ( F1.fold1 $ evalVariableSum t c1
    )

evalTuple2 :: Semigroup s => Tuple2 s -> (s, s)
evalTuple2 (Tuple2 c1 c2 t) =
    ( F1.fold1 $ evalVariableSum t c1
    , F1.fold1 $ evalVariableSum t c2
    )

evalTuple3 :: Semigroup s => Tuple3 s -> (s, s, s)
evalTuple3 (Tuple3 c1 c2 c3 t) =
    ( F1.fold1 $ evalVariableSum t c1
    , F1.fold1 $ evalVariableSum t c2
    , F1.fold1 $ evalVariableSum t c3
    )

showTuple1 :: (Semigroup a, Show a) => Tuple1 a -> String
showTuple1 (evalTuple1 -> a) = unlines
    [ mempty, "a:", showWrap a
    ]

showTuple2 :: (Semigroup a, Show a) => Tuple2 a -> String
showTuple2 (evalTuple2 -> (a, b)) = unlines
    [ mempty, "a:", showWrap a
    , mempty, "b:", showWrap b
    ]

showTuple3 :: (Semigroup a, Show a) => Tuple3 a -> String
showTuple3 (evalTuple3 -> (a, b, c)) = unlines
    [ mempty, "a:", showWrap a
    , mempty, "b:", showWrap b
    , mempty, "c:", showWrap c
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
