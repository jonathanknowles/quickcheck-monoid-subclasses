{- HLINT ignore "Avoid lambda" -}
{- HLINT ignore "Redundant bracket" -}
{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Use const" -}
{- HLINT ignore "Use null" -}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
-- This module provides 'Laws' definitions for classes exported by
-- "Data.Semigroup.Factorial".
--
module Test.QuickCheck.Classes.Semigroup.Factorial
    ( factorialLaws
    )
    where

import Prelude hiding
    ( foldl, foldr )

import Data.Function
    ( (&) )
import Data.List.NonEmpty
    ( nonEmpty )
import Data.Proxy
    ( Proxy )
import Data.Semigroup
    ( Semigroup (sconcat) )
import Data.Semigroup.Factorial
    ( Factorial (factors, foldl, foldl', foldr, primePrefix) )
import Internal
    ( cover, makeLaw1, makeLaw2, makeProperty, report )
import Test.QuickCheck
    ( Arbitrary, Gen, Property, elements, forAllBlind )
import Test.QuickCheck.Classes
    ( Laws (Laws) )

import qualified Data.Foldable as F

--------------------------------------------------------------------------------
-- Factorial
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'Factorial'.
--
-- Includes the following laws:
--
-- @
-- 'maybe' a 'sconcat' ('nonEmpty' ('factors' a)) '==' a
-- @
--
-- @
-- 'all' (\\f -> 'factors' f '==' [f]) ('factors' a)
-- @
--
-- @
-- 'primePrefix' a '==' 'foldr' 'const' a a
-- @
--
-- @
-- 'foldl'  f x a '==' "Data.Foldable".'F.foldl'  f x ('factors' a)
-- 'foldl'' f x a '==' "Data.Foldable".'F.foldl'' f x ('factors' a)
-- 'foldr'  f x a '==' "Data.Foldable".'F.foldr'  f x ('factors' a)
-- @
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'Test.QuickCheck.Classes.semigroupLaws'
--
factorialLaws
    :: forall a. (Arbitrary a, Show a, Eq a, Factorial a)
    => Proxy a
    -> Laws
factorialLaws _ = Laws "Factorial"
    [ makeLaw1 @a
        "factorialLaw_coverage"
        (factorialLaw_coverage)
    , makeLaw1 @a
        "factorialLaw_maybe_sconcat_nonEmpty_factors"
        (factorialLaw_maybe_sconcat_nonEmpty_factors)
    , makeLaw1 @a
        "factorialLaw_all_factors_prime"
        (factorialLaw_all_factors_prime)
    , makeLaw1 @a
        "factorialLaw_primePrefix_foldr"
        (factorialLaw_primePrefix_foldr)
    , makeLaw2 @a
        "factorialLaw_factors_foldl"
        (factorialLaw_factors_foldl)
    , makeLaw2 @a
        "factorialLaw_factors_foldl'"
        (factorialLaw_factors_foldl')
    , makeLaw2 @a
        "factorialLaw_factors_foldr"
        (factorialLaw_factors_foldr)
    ]

factorialLaw_coverage
    :: (Eq a, Show a, Factorial a) => a -> Property
factorialLaw_coverage a =
    makeProperty
        "True"
        (True)
    & cover
        "length (factors a) == 0"
        (length (factors a) == 0)
    & cover
        "length (factors a) == 1"
        (length (factors a) == 1)
    & cover
        "length (factors a) >= 2"
        (length (factors a) >= 2)

factorialLaw_maybe_sconcat_nonEmpty_factors
    :: (Eq a, Show a, Factorial a) => a -> Property
factorialLaw_maybe_sconcat_nonEmpty_factors a =
    makeProperty
        "maybe a sconcat (nonEmpty (factors a)) == a"
        (maybe a sconcat (nonEmpty (factors a)) == a)
    & report
        "factors a"
        (factors a)
    & report
        "nonEmpty (factors a)"
        (nonEmpty (factors a))
    & report
        "fmap sconcat (nonEmpty (factors a))"
        (fmap sconcat (nonEmpty (factors a)))

factorialLaw_all_factors_prime
    :: (Eq a, Show a, Factorial a) => a -> Property
factorialLaw_all_factors_prime a =
    makeProperty
        "all (λf -> factors f == [f]) (factors a)"
        (all (\f -> factors f == [f]) (factors a))
    & report
        "factors a"
        (factors a)
    & report
        "factors <$> factors a"
        (factors <$> factors a)

factorialLaw_primePrefix_foldr
    :: (Eq a, Show a, Factorial a) => a -> Property
factorialLaw_primePrefix_foldr a =
    makeProperty
        "primePrefix a == foldr const a a"
        (primePrefix a == foldr const a a)
    & report
        "primePrefix a"
        (primePrefix a)
    & report
        "foldr const a a"
        (foldr const a a)

factorialLaw_factors_foldl
    :: (Eq a, Show a, Factorial a) => a -> a -> Property
factorialLaw_factors_foldl x a =
    forAllBlind genAccumulatorFn $ \(fDefinition, f) ->
        makeProperty
            "foldl f x a == F.foldl f x (factors a)"
            (foldl f x a == F.foldl f x (factors a))
        & report
            "f"
            (fDefinition)
        & report
            "foldl f x a"
            (foldl f x a)
        & report
            "F.foldl f x (factors a)"
            (F.foldl f x (factors a))

factorialLaw_factors_foldl'
    :: (Eq a, Show a, Factorial a) => a -> a -> Property
factorialLaw_factors_foldl' x a =
    forAllBlind genAccumulatorFn $ \(fDefinition, f) ->
        makeProperty
            "foldl' f x a == F.foldl' f x (factors a)"
            (foldl' f x a == F.foldl' f x (factors a))
        & report
            "f"
            (fDefinition)
        & report
            "foldl' f x a"
            (foldl' f x a)
        & report
            "F.foldl' f x (factors a)"
            (F.foldl' f x (factors a))

factorialLaw_factors_foldr
    :: (Eq a, Show a, Factorial a) => a -> a -> Property
factorialLaw_factors_foldr x a =
    forAllBlind genAccumulatorFn $ \(fDefinition, f) ->
        makeProperty
            "foldr f x a == F.foldr f x (factors a)"
            (foldr f x a == F.foldr f x (factors a))
        & report
            "f"
            (fDefinition)
        & report
            "foldr f x a"
            (foldr f x a)
        & report
            "F.foldr f x (factors a)"
            (F.foldr f x (factors a))

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

genAccumulatorFn :: Semigroup a => Gen (String, a -> a -> a)
genAccumulatorFn = elements
    [ ( "λa _ -> a"
      , (\a _ -> a)
      )
    , ( "λ_ b -> b"
      , (\_ b -> b)
      )
    , ( "λa b -> a <> b"
      , (\a b -> a <> b)
      )
    , ( "λa b -> b <> a"
      , (\a b -> b <> a)
      )
    ]
