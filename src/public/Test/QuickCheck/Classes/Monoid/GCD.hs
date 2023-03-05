{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Redundant bracket" -}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
-- This module provides 'Laws' definitions for classes exported by
-- "Data.Monoid.GCD".
--
module Test.QuickCheck.Classes.Monoid.GCD
    ( gcdMonoidLaws
    , cancellativeGCDMonoidLaws
    , leftGCDMonoidLaws
    , rightGCDMonoidLaws
    , overlappingGCDMonoidLaws
    )
    where

import Prelude hiding
    ( gcd )

import Data.Function
    ( (&) )
import Data.Maybe
    ( isJust )
import Data.Monoid.GCD
    ( GCDMonoid (..)
    , LeftGCDMonoid (..)
    , OverlappingGCDMonoid (..)
    , RightGCDMonoid (..)
    )
import Data.Proxy
    ( Proxy (..) )
import Data.Semigroup.Cancellative
    ( Cancellative, LeftReductive (..), Reductive (..), RightReductive (..) )
import Internal
    ( cover, makeLaw1, makeLaw2, makeLaw3, makeProperty, report, (==>) )
import Test.QuickCheck
    ( Arbitrary (..), Property )
import Test.QuickCheck.Classes
    ( Laws (..) )

--------------------------------------------------------------------------------
-- CancellativeGCDMonoid
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'Cancellative' and 'GCDMonoid'.
--
-- Tests the following laws:
--
-- @
-- 'gcd' (a '<>' b) (a '<>' c) '==' a '<>' 'gcd' b c
-- @
--
-- @
-- 'gcd' (a '<>' c) (b '<>' c) '==' 'gcd' a b '<>' c
-- @
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'Test.QuickCheck.Classes.Semigroup.Cancellative.cancellativeLaws'
-- * 'Test.QuickCheck.Classes.Monoid.GCD.gcdMonoidLaws'
--
cancellativeGCDMonoidLaws
    :: forall a. (Arbitrary a, Show a, Eq a, Cancellative a, GCDMonoid a)
    => Proxy a
    -> Laws
cancellativeGCDMonoidLaws _ = Laws "CancellativeGCDMonoid"
    [ makeLaw3 @a
        "cancellativeGCDMonoidLaw_prefix"
        (cancellativeGCDMonoidLaw_prefix)
    , makeLaw3 @a
        "cancellativeGCDMonoidLaw_suffix"
        (cancellativeGCDMonoidLaw_suffix)
    ]

cancellativeGCDMonoidLaw_prefix
    :: (Eq a, Show a, Cancellative a, GCDMonoid a) => a -> a -> a -> Property
cancellativeGCDMonoidLaw_prefix a b c =
    makeProperty
        "gcd (a <> b) (a <> c) == a <> gcd b c"
        (gcd (a <> b) (a <> c) == a <> gcd b c)
    & report
        "a <> b"
        (a <> b)
    & report
        "a <> c"
        (a <> c)
    & report
        "gcd (a <> b) (a <> c)"
        (gcd (a <> b) (a <> c))
    & report
        "gcd b c"
        (gcd b c)
    & report
        "a <> gcd b c"
        (a <> gcd b c)
    & cover
        "a /= mempty && gcd b c /= mempty && a /= gcd b c"
        (a /= mempty && gcd b c /= mempty && a /= gcd b c)

cancellativeGCDMonoidLaw_suffix
    :: (Eq a, Show a, Cancellative a, GCDMonoid a) => a -> a -> a -> Property
cancellativeGCDMonoidLaw_suffix a b c =
    makeProperty
        "gcd (a <> c) (b <> c) == gcd a b <> c"
        (gcd (a <> c) (b <> c) == gcd a b <> c)
    & report
        "a <> c"
        (a <> c)
    & report
        "b <> c"
        (b <> c)
    & report
        "gcd (a <> c) (b <> c)"
        (gcd (a <> c) (b <> c))
    & report
        "gcd a b"
        (gcd a b)
    & report
        "gcd a b <> c"
        (gcd a b <> c)
    & cover
        "c /= mempty && gcd a b /= mempty && c /= gcd a b"
        (c /= mempty && gcd a b /= mempty && c /= gcd a b)

--------------------------------------------------------------------------------
-- GCDMonoid
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'GCDMonoid'.
--
-- Includes the following laws:
--
-- __/Reductivity/__
--
-- @
-- 'isJust' (a '</>' 'gcd' a b)
-- @
-- @
-- 'isJust' (b '</>' 'gcd' a b)
-- @
--
-- __/Uniqueness/__
--
-- @
-- 'all' 'isJust'
--     [ a '</>' c
--     , b '</>' c
--     , c '</>' 'gcd' a b
--     ]
-- ==>
--     (c '==' 'gcd' a b)
-- @
--
-- __/Idempotence/__
--
-- @
-- 'gcd' a a '==' a
-- @
--
-- __/Identity/__
--
-- @
-- 'gcd' 'mempty' a '==' 'mempty'
-- @
-- @
-- 'gcd' a 'mempty' '==' 'mempty'
-- @
--
-- __/Commutativity/__
--
-- @
-- 'gcd' a b '==' 'gcd' b a
-- @
--
-- __/Associativity/__
--
-- @
-- 'gcd' ('gcd' a b) c '==' 'gcd' a ('gcd' b c)
-- @
--
-- __/Distributivity/__
--
-- @
-- 'gcd' (a '<>' b) (a '<>' c) '==' a '<>' 'gcd' b c
-- @
-- @
-- 'gcd' (a '<>' c) (b '<>' c) '==' 'gcd' a b '<>' c
-- @
--
-- __/Equivalence/__
--
-- @
-- 'gcd' a b '==' 'commonPrefix' a b
-- @
-- @
-- 'gcd' a b '==' 'commonSuffix' a b
-- @
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'Test.QuickCheck.Classes.monoidLaws'
-- * 'Test.QuickCheck.Classes.Semigroup.Cancellative.commutativeLaws'
-- * 'Test.QuickCheck.Classes.Semigroup.Cancellative.reductiveLaws'
-- * 'Test.QuickCheck.Classes.Monoid.GCD.leftGCDMonoidLaws'
-- * 'Test.QuickCheck.Classes.Monoid.GCD.rightGCDMonoidLaws'
-- * 'Test.QuickCheck.Classes.Monoid.GCD.overlappingGCDMonoidLaws'
--
gcdMonoidLaws
    :: forall a. (Arbitrary a, Show a, Eq a, GCDMonoid a)
    => Proxy a
    -> Laws
gcdMonoidLaws _ = Laws "GCDMonoid"
    [ makeLaw2 @a
        "gcdMonoidLaw_reductivity_left"
        (gcdMonoidLaw_reductivity_left)
    , makeLaw2 @a
        "gcdMonoidLaw_reductivity_right"
        (gcdMonoidLaw_reductivity_right)
    , makeLaw3 @a
        "gcdMonoidLaw_uniqueness"
        (gcdMonoidLaw_uniqueness)
    , makeLaw1 @a
        "gcdMonoidLaw_idempotence"
        (gcdMonoidLaw_idempotence)
    , makeLaw1 @a
        "gcdMonoidLaw_identity_left"
        (gcdMonoidLaw_identity_left)
    , makeLaw1 @a
        "gcdMonoidLaw_identity_right"
        (gcdMonoidLaw_identity_right)
    , makeLaw2 @a
        "gcdMonoidLaw_commutativity"
        (gcdMonoidLaw_commutativity)
    , makeLaw3 @a
        "gcdMonoidLaw_associativity"
        (gcdMonoidLaw_associativity)
    , makeLaw3 @a
        "gcdMonoidLaw_distributivity_left"
        (gcdMonoidLaw_distributivity_left)
    , makeLaw3 @a
        "gcdMonoidLaw_distributivity_right"
        (gcdMonoidLaw_distributivity_right)
    , makeLaw2 @a
        "gcdMonoidLaw_equivalence_commonPrefix"
        (gcdMonoidLaw_equivalence_commonPrefix)
    , makeLaw2 @a
        "gcdMonoidLaw_equivalence_commonSuffix"
        (gcdMonoidLaw_equivalence_commonSuffix)
    ]

gcdMonoidLaw_reductivity_left
    :: (Eq a, Show a, GCDMonoid a) => a -> a -> Property
gcdMonoidLaw_reductivity_left a b =
    makeProperty
        "isJust (a </> gcd a b)"
        (isJust (a </> gcd a b))
    & cover
        "gcd a b /= mempty"
        (gcd a b /= mempty)
    & cover
        "(a </> gcd a b) /= mempty"
        ((a </> gcd a b) /= mempty)
    & report
        "gcd a b"
        (gcd a b)
    & report
        "a </> gcd a b"
        (a </> gcd a b)

gcdMonoidLaw_reductivity_right
    :: (Eq a, Show a, GCDMonoid a) => a -> a -> Property
gcdMonoidLaw_reductivity_right a b =
    makeProperty
        "isJust (b </> gcd a b)"
        (isJust (b </> gcd a b))
    & cover
        "gcd a b /= mempty"
        (gcd a b /= mempty)
    & cover
        "(b </> gcd a b) /= mempty"
        ((b </> gcd a b) /= mempty)
    & report
        "gcd a b"
        (gcd a b)
    & report
        "b </> gcd a b"
        (b </> gcd a b)

gcdMonoidLaw_uniqueness
    :: (Eq a, Show a, GCDMonoid a) => a -> a -> a -> Property
gcdMonoidLaw_uniqueness a b c =
    makeProperty
        "all isJust [a </> c, b </> c, c </> gcd a b] ==> (c == gcd a b)"
        (all isJust [a </> c, b </> c, c </> gcd a b] ==> (c == gcd a b))
    & cover
        "all isJust [a </> c, b </> c, c </> gcd a b]"
        (all isJust [a </> c, b </> c, c </> gcd a b])
    & cover
        "not (all isJust [a </> c, b </> c, c </> gcd a b])"
        (not (all isJust [a </> c, b </> c, c </> gcd a b]))
    & cover
        "c == gcd a b"
        (c == gcd a b)
    & cover
        "c /= gcd a b"
        (c /= gcd a b)
    & report
        "gcd a b"
        (gcd a b)
    & report
        "c </> gcd a b"
        (c </> gcd a b)
    & report
        "a </> c"
        (a </> c)
    & report
        "b </> c"
        (b </> c)

gcdMonoidLaw_idempotence
    :: (Eq a, Show a, GCDMonoid a) => a -> Property
gcdMonoidLaw_idempotence a =
    makeProperty
        "gcd a a == a"
        (gcd a a == a)
    & report
        "gcd a a"
        (gcd a a)

gcdMonoidLaw_identity_left
    :: (Eq a, Show a, GCDMonoid a) => a -> Property
gcdMonoidLaw_identity_left a =
    makeProperty
        "gcd mempty a == mempty"
        (gcd mempty a == mempty)
    & cover
        "a /= mempty"
        (a /= mempty)
    & report
        "gcd mempty a"
        (gcd mempty a)

gcdMonoidLaw_identity_right
    :: (Eq a, Show a, GCDMonoid a) => a -> Property
gcdMonoidLaw_identity_right a =
    makeProperty
        "gcd a mempty == mempty"
        (gcd a mempty == mempty)
    & cover
        "a /= mempty"
        (a /= mempty)
    & report
        "gcd a mempty"
        (gcd a mempty)

gcdMonoidLaw_commutativity
    :: (Eq a, Show a, GCDMonoid a) => a -> a -> Property
gcdMonoidLaw_commutativity a b =
    makeProperty
        "gcd a b == gcd b a"
        (gcd a b == gcd b a)
    & cover
        "gcd a b == mempty"
        (gcd a b == mempty)
    & cover
        "gcd a b /= mempty"
        (gcd a b /= mempty)
    & report
        "gcd a b"
        (gcd a b)
    & report
        "gcd b a"
        (gcd b a)

gcdMonoidLaw_associativity
    :: (Eq a, Show a, GCDMonoid a) => a -> a -> a -> Property
gcdMonoidLaw_associativity a b c =
    makeProperty
        "gcd (gcd a b) c == gcd a (gcd b c)"
        (gcd (gcd a b) c == gcd a (gcd b c))
    & cover
        "gcd a b /= mempty"
        (gcd a b /= mempty)
    & cover
        "gcd b c /= mempty"
        (gcd b c /= mempty)
    & report
        "gcd a b"
        (gcd a b)
    & report
        "gcd (gcd a b) c"
        (gcd (gcd a b) c)
    & report
        "gcd b c"
        (gcd b c)
    & report
        "gcd a (gcd b c)"
        (gcd a (gcd b c))

gcdMonoidLaw_distributivity_left
    :: (Eq a, Show a, GCDMonoid a) => a -> a -> a -> Property
gcdMonoidLaw_distributivity_left a b c =
    makeProperty
        "gcd (a <> b) (a <> c) == a <> gcd b c"
        (gcd (a <> b) (a <> c) == a <> gcd b c)
    & report
        "a <> b"
        (a <> b)
    & report
        "a <> c"
        (a <> c)
    & report
        "gcd (a <> b) (a <> c)"
        (gcd (a <> b) (a <> c))
    & report
        "gcd b c"
        (gcd b c)
    & report
        "a <> gcd b c"
        (a <> gcd b c)

gcdMonoidLaw_distributivity_right
    :: (Eq a, Show a, GCDMonoid a) => a -> a -> a -> Property
gcdMonoidLaw_distributivity_right a b c =
    makeProperty
        "gcd (a <> c) (b <> c) == gcd a b <> c"
        (gcd (a <> c) (b <> c) == gcd a b <> c)
    & report
        "a <> c"
        (a <> c)
    & report
        "b <> c"
        (b <> c)
    & report
        "gcd (a <> c) (b <> c)"
        (gcd (a <> c) (b <> c))
    & report
        "gcd a b"
        (gcd a b)
    & report
        "gcd a b <> c"
        (gcd a b <> c)

gcdMonoidLaw_equivalence_commonPrefix
    :: (Eq a, Show a, GCDMonoid a) => a -> a -> Property
gcdMonoidLaw_equivalence_commonPrefix a b =
    makeProperty
        "gcd a b == commonPrefix a b"
        (gcd a b == commonPrefix a b)
    & cover
        "gcd a b /= mempty"
        (gcd a b /= mempty)
    & cover
        "commonPrefix a b /= mempty"
        (commonPrefix a b /= mempty)
    & report
        "gcd a b"
        (gcd a b)
    & report
        "commonPrefix a b"
        (commonPrefix a b)

gcdMonoidLaw_equivalence_commonSuffix
    :: (Eq a, Show a, GCDMonoid a) => a -> a -> Property
gcdMonoidLaw_equivalence_commonSuffix a b =
    makeProperty
        "gcd a b == commonSuffix a b"
        (gcd a b == commonSuffix a b)
    & cover
        "gcd a b /= mempty"
        (gcd a b /= mempty)
    & cover
        "commonSuffix a b /= mempty"
        (commonSuffix a b /= mempty)
    & report
        "gcd a b"
        (gcd a b)
    & report
        "commonSuffix a b"
        (commonSuffix a b)

--------------------------------------------------------------------------------
-- LeftGCDMonoid
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'LeftGCDMonoid'.
--
-- Tests the following laws:
--
-- @
-- 'stripCommonPrefix' a b '&' \\(p, _, _) -> p '==' 'commonPrefix' a b
-- @
--
-- @
-- 'stripCommonPrefix' a b '&' \\(p, x, _) -> p '<>' x '==' a
-- @
--
-- @
-- 'stripCommonPrefix' a b '&' \\(p, _, x) -> p '<>' x '==' b
-- @
--
-- @
-- 'stripCommonPrefix' a b '&' \\(p, x, _) -> 'Just' x '==' 'stripPrefix' p a
-- @
--
-- @
-- 'stripCommonPrefix' a b '&' \\(p, _, x) -> 'Just' x '==' 'stripPrefix' p b
-- @
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'Test.QuickCheck.Classes.monoidLaws'
-- * 'Test.QuickCheck.Classes.Semigroup.Cancellative.leftReductiveLaws'
--
leftGCDMonoidLaws
    :: forall a. (Arbitrary a, Show a, Eq a, LeftGCDMonoid a)
    => Proxy a
    -> Laws
leftGCDMonoidLaws _ = Laws "LeftGCDMonoid"
    [ makeLaw2 @a
        "leftGCDMonoidLaw_stripCommonPrefix_commonPrefix"
        (leftGCDMonoidLaw_stripCommonPrefix_commonPrefix)
    , makeLaw2 @a
        "leftGCDMonoidLaw_stripCommonPrefix_mappend_1"
        (leftGCDMonoidLaw_stripCommonPrefix_mappend_1)
    , makeLaw2 @a
        "leftGCDMonoidLaw_stripCommonPrefix_mappend_2"
        (leftGCDMonoidLaw_stripCommonPrefix_mappend_2)
    , makeLaw2 @a
        "leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_1"
        (leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_1)
    , makeLaw2 @a
        "leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_2"
        (leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_2)
    ]

leftGCDMonoidLaw_stripCommonPrefix_commonPrefix
    :: (Eq a, Show a, LeftGCDMonoid a) => a -> a -> Property
leftGCDMonoidLaw_stripCommonPrefix_commonPrefix a b =
    makeProperty
        "stripCommonPrefix a b & λ(p, _, _) -> p == commonPrefix a b"
        (stripCommonPrefix a b & \(p, _, _) -> p == commonPrefix a b)
    & cover
        "commonPrefix a b /= mempty"
        (commonPrefix a b /= mempty)
    & report
        "stripCommonPrefix a b"
        (stripCommonPrefix a b)
    & report
        "commonPrefix a b"
        (commonPrefix a b)

leftGCDMonoidLaw_stripCommonPrefix_mappend_1
    :: (Eq a, Show a, LeftGCDMonoid a) => a -> a -> Property
leftGCDMonoidLaw_stripCommonPrefix_mappend_1 a b =
    makeProperty
        "stripCommonPrefix a b & λ(p, x, _) -> p <> x == a"
        (stripCommonPrefix a b & \(p, x, _) -> p <> x == a)
    & cover
        "stripCommonPrefix a b & λ(p, x, _) -> p /= mempty && x /= mempty"
        (stripCommonPrefix a b & \(p, x, _) -> p /= mempty && x /= mempty)
    & report
        "stripCommonPrefix a b"
        (stripCommonPrefix a b)
    & report
        "stripCommonPrefix a b & λ(p, x, _) -> p <> x"
        (stripCommonPrefix a b & \(p, x, _) -> p <> x)

leftGCDMonoidLaw_stripCommonPrefix_mappend_2
    :: (Eq a, Show a, LeftGCDMonoid a) => a -> a -> Property
leftGCDMonoidLaw_stripCommonPrefix_mappend_2 a b =
    makeProperty
        "stripCommonPrefix a b & λ(p, _, x) -> p <> x == b"
        (stripCommonPrefix a b & \(p, _, x) -> p <> x == b)
    & cover
        "stripCommonPrefix a b & λ(p, _, x) -> p /= mempty && x /= mempty"
        (stripCommonPrefix a b & \(p, _, x) -> p /= mempty && x /= mempty)
    & report
        "stripCommonPrefix a b"
        (stripCommonPrefix a b)
    & report
        "stripCommonPrefix a b & λ(p, _, x) -> p <> x"
        (stripCommonPrefix a b & \(p, _, x) -> p <> x)

leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_1
    :: (Eq a, Show a, LeftGCDMonoid a) => a -> a -> Property
leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_1 a b =
    makeProperty
        "stripCommonPrefix a b & λ(p, x, _) -> Just x == stripPrefix p a"
        (stripCommonPrefix a b & \(p, x, _) -> Just x == stripPrefix p a)
    & cover
        "stripCommonPrefix a b & λ(p, x, _) -> p /= mempty && x /= mempty"
        (stripCommonPrefix a b & \(p, x, _) -> p /= mempty && x /= mempty)
    & report
        "stripCommonPrefix a b"
        (stripCommonPrefix a b)
    & report
        "stripCommonPrefix a b & λ(p, _, _) -> stripPrefix p a"
        (stripCommonPrefix a b & \(p, _, _) -> stripPrefix p a)

leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_2
    :: (Eq a, Show a, LeftGCDMonoid a) => a -> a -> Property
leftGCDMonoidLaw_stripCommonPrefix_stripPrefix_2 a b =
    makeProperty
        "stripCommonPrefix a b & λ(p, _, x) -> Just x == stripPrefix p b"
        (stripCommonPrefix a b & \(p, _, x) -> Just x == stripPrefix p b)
    & cover
        "stripCommonPrefix a b & λ(p, _, x) -> p /= mempty && x /= mempty"
        (stripCommonPrefix a b & \(p, _, x) -> p /= mempty && x /= mempty)
    & report
        "stripCommonPrefix a b"
        (stripCommonPrefix a b)
    & report
        "stripCommonPrefix a b & λ(p, _, _) -> stripPrefix p b"
        (stripCommonPrefix a b & \(p, _, _) -> stripPrefix p b)

--------------------------------------------------------------------------------
-- OverlappingGCDMonoid
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'OverlappingGCDMonoid'.
--
-- Tests the following laws:
--
-- @
-- 'overlap' a b '<>' 'stripPrefixOverlap' a b '==' b
-- @
--
-- @
-- 'stripSuffixOverlap' b a '<>' 'overlap' a b '==' a
-- @
--
-- @
-- 'stripOverlap' a b '&' \\(_, x, _) -> x '==' 'overlap' a b
-- @
--
-- @
-- 'stripOverlap' a b '&' \\(_, _, x) -> x '==' 'stripPrefixOverlap' a b
-- @
--
-- @
-- 'stripOverlap' a b '&' \\(x, _, _) -> x '==' 'stripSuffixOverlap' b a
-- @
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'Test.QuickCheck.Classes.monoidLaws'
-- * 'Test.QuickCheck.Classes.Semigroup.Cancellative.leftReductiveLaws'
-- * 'Test.QuickCheck.Classes.Semigroup.Cancellative.rightReductiveLaws'
--
overlappingGCDMonoidLaws
    :: forall a. (Arbitrary a, Show a, Eq a, OverlappingGCDMonoid a)
    => Proxy a
    -> Laws
overlappingGCDMonoidLaws _ = Laws "OverlappingGCDMonoid"
    [ makeLaw2 @a
        "overlappingGCDMonoidLaw_overlap_stripPrefixOverlap"
        (overlappingGCDMonoidLaw_overlap_stripPrefixOverlap)
    , makeLaw2 @a
        "overlappingGCDMonoidLaw_overlap_stripSuffixOverlap"
        (overlappingGCDMonoidLaw_overlap_stripSuffixOverlap)
    , makeLaw2 @a
        "overlappingGCDMonoidLaw_stripOverlap_overlap"
        (overlappingGCDMonoidLaw_stripOverlap_overlap)
    , makeLaw2 @a
        "overlappingGCDMonoidLaw_stripOverlap_stripPrefixOverlap"
        (overlappingGCDMonoidLaw_stripOverlap_stripPrefixOverlap)
    , makeLaw2 @a
        "overlappingGCDMonoidLaw_stripOverlap_stripSuffixOverlap"
        (overlappingGCDMonoidLaw_stripOverlap_stripSuffixOverlap)
    ]

overlappingGCDMonoidLaw_overlap_stripPrefixOverlap
    :: (Eq a, Show a, OverlappingGCDMonoid a) => a -> a -> Property
overlappingGCDMonoidLaw_overlap_stripPrefixOverlap a b =
    makeProperty
        "overlap a b <> stripPrefixOverlap a b == b"
        (overlap a b <> stripPrefixOverlap a b == b)
    & cover
        "overlap a b /= mempty && stripPrefixOverlap a b /= mempty"
        (overlap a b /= mempty && stripPrefixOverlap a b /= mempty)
    & report
        "overlap a b"
        (overlap a b)
    & report
        "stripPrefixOverlap a b"
        (stripPrefixOverlap a b)
    & report
        "overlap a b <> stripPrefixOverlap a b"
        (overlap a b <> stripPrefixOverlap a b)

overlappingGCDMonoidLaw_overlap_stripSuffixOverlap
    :: (Eq a, Show a, OverlappingGCDMonoid a) => a -> a -> Property
overlappingGCDMonoidLaw_overlap_stripSuffixOverlap a b =
    makeProperty
        "stripSuffixOverlap b a <> overlap a b == a"
        (stripSuffixOverlap b a <> overlap a b == a)
    & cover
        "stripSuffixOverlap b a /= mempty && overlap a b /= mempty"
        (stripSuffixOverlap b a /= mempty && overlap a b /= mempty)
    & report
        "stripSuffixOverlap b a"
        (stripSuffixOverlap b a)
    & report
        "overlap a b"
        (overlap a b)
    & report
        "stripSuffixOverlap b a <> overlap a b"
        (stripSuffixOverlap b a <> overlap a b)

overlappingGCDMonoidLaw_stripOverlap_overlap
    :: (Eq a, Show a, OverlappingGCDMonoid a) => a -> a -> Property
overlappingGCDMonoidLaw_stripOverlap_overlap a b =
    makeProperty
        "stripOverlap a b & λ(_, x, _) -> x == overlap a b"
        (stripOverlap a b & \(_, x, _) -> x == overlap a b)
    & cover
        "overlap a b /= mempty"
        (overlap a b /= mempty)
    & report
        "stripOverlap a b"
        (stripOverlap a b)
    & report
        "overlap a b"
        (overlap a b)

overlappingGCDMonoidLaw_stripOverlap_stripPrefixOverlap
    :: (Eq a, Show a, OverlappingGCDMonoid a) => a -> a -> Property
overlappingGCDMonoidLaw_stripOverlap_stripPrefixOverlap a b =
    makeProperty
        "stripOverlap a b & λ(_, _, x) -> x == stripPrefixOverlap a b"
        (stripOverlap a b & \(_, _, x) -> x == stripPrefixOverlap a b)
    & cover
        "stripPrefixOverlap a b /= mempty"
        (stripPrefixOverlap a b /= mempty)
    & report
        "stripOverlap a b"
        (stripOverlap a b)
    & report
        "stripPrefixOverlap a b"
        (stripPrefixOverlap a b)

overlappingGCDMonoidLaw_stripOverlap_stripSuffixOverlap
    :: (Eq a, Show a, OverlappingGCDMonoid a) => a -> a -> Property
overlappingGCDMonoidLaw_stripOverlap_stripSuffixOverlap a b =
    makeProperty
        "stripOverlap a b & λ(x, _, _) -> x == stripSuffixOverlap b a"
        (stripOverlap a b & \(x, _, _) -> x == stripSuffixOverlap b a)
    & cover
        "stripSuffixOverlap b a /= mempty"
        (stripSuffixOverlap b a /= mempty)
    & report
        "stripOverlap a b"
        (stripOverlap a b)
    & report
        "stripSuffixOverlap b a"
        (stripSuffixOverlap b a)

--------------------------------------------------------------------------------
-- RightGCDMonoid
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'RightGCDMonoid'.
--
-- Tests the following laws:
--
-- @
-- 'stripCommonSuffix' a b '&' \\(_, _, s) -> s '==' 'commonSuffix' a b
-- @
--
-- @
-- 'stripCommonSuffix' a b '&' \\(x, _, s) -> x '<>' s '==' a
-- @
--
-- @
-- 'stripCommonSuffix' a b '&' \\(_, x, s) -> x '<>' s '==' b
-- @
--
-- @
-- 'stripCommonSuffix' a b '&' \\(x, _, s) -> 'Just' x '==' 'stripSuffix' s a
-- @
--
-- @
-- 'stripCommonSuffix' a b '&' \\(_, x, s) -> 'Just' x '==' 'stripSuffix' s b
-- @
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'Test.QuickCheck.Classes.monoidLaws'
-- * 'Test.QuickCheck.Classes.Semigroup.Cancellative.rightReductiveLaws'
--
rightGCDMonoidLaws
    :: forall a. (Arbitrary a, Show a, Eq a, RightGCDMonoid a)
    => Proxy a
    -> Laws
rightGCDMonoidLaws _ = Laws "RightGCDMonoid"
    [ makeLaw2 @a
        "rightGCDMonoidLaw_stripCommonSuffix_commonSuffix"
        (rightGCDMonoidLaw_stripCommonSuffix_commonSuffix)
    , makeLaw2 @a
        "rightGCDMonoidLaw_stripCommonSuffix_mappend_1"
        (rightGCDMonoidLaw_stripCommonSuffix_mappend_1)
    , makeLaw2 @a
        "rightGCDMonoidLaw_stripCommonSuffix_mappend_2"
        (rightGCDMonoidLaw_stripCommonSuffix_mappend_2)
    , makeLaw2 @a
        "rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_1"
        (rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_1)
    , makeLaw2 @a
        "rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_2"
        (rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_2)
    ]

rightGCDMonoidLaw_stripCommonSuffix_commonSuffix
    :: (Eq a, Show a, RightGCDMonoid a) => a -> a -> Property
rightGCDMonoidLaw_stripCommonSuffix_commonSuffix a b =
    makeProperty
        "stripCommonSuffix a b & λ(_, _, s) -> s == commonSuffix a b"
        (stripCommonSuffix a b & \(_, _, s) -> s == commonSuffix a b)
    & cover
        "commonSuffix a b /= mempty"
        (commonSuffix a b /= mempty)
    & report
        "stripCommonSuffix a b"
        (stripCommonSuffix a b)
    & report
        "commonSuffix a b"
        (commonSuffix a b)

rightGCDMonoidLaw_stripCommonSuffix_mappend_1
    :: (Eq a, Show a, RightGCDMonoid a) => a -> a -> Property
rightGCDMonoidLaw_stripCommonSuffix_mappend_1 a b =
    makeProperty
        "stripCommonSuffix a b & λ(x, _, s) -> x <> s == a"
        (stripCommonSuffix a b & \(x, _, s) -> x <> s == a)
    & cover
        "stripCommonSuffix a b & λ(x, _, s) -> x /= mempty && s /= mempty"
        (stripCommonSuffix a b & \(x, _, s) -> x /= mempty && s /= mempty)
    & report
        "stripCommonSuffix a b"
        (stripCommonSuffix a b)
    & report
        "stripCommonSuffix a b & λ(x, _, s) -> x <> s"
        (stripCommonSuffix a b & \(x, _, s) -> x <> s)

rightGCDMonoidLaw_stripCommonSuffix_mappend_2
    :: (Eq a, Show a, RightGCDMonoid a) => a -> a -> Property
rightGCDMonoidLaw_stripCommonSuffix_mappend_2 a b =
    makeProperty
        "stripCommonSuffix a b & λ(_, x, s) -> x <> s == b"
        (stripCommonSuffix a b & \(_, x, s) -> x <> s == b)
    & cover
        "stripCommonSuffix a b & λ(_, x, s) -> x /= mempty && s /= mempty"
        (stripCommonSuffix a b & \(_, x, s) -> x /= mempty && s /= mempty)
    & report
        "stripCommonSuffix a b"
        (stripCommonSuffix a b)
    & report
        "stripCommonSuffix a b & λ(_, x, s) -> x <> s"
        (stripCommonSuffix a b & \(_, x, s) -> x <> s)

rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_1
    :: (Eq a, Show a, RightGCDMonoid a) => a -> a -> Property
rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_1 a b =
    makeProperty
        "stripCommonSuffix a b & λ(x, _, s) -> Just x == stripSuffix s a"
        (stripCommonSuffix a b & \(x, _, s) -> Just x == stripSuffix s a)
    & cover
        "stripCommonSuffix a b & λ(x, _, s) -> x /= mempty && s /= mempty"
        (stripCommonSuffix a b & \(x, _, s) -> x /= mempty && s /= mempty)
    & report
        "stripCommonSuffix a b"
        (stripCommonSuffix a b)
    & report
        "stripCommonSuffix a b & λ(_, _, s) -> stripSuffix s a"
        (stripCommonSuffix a b & \(_, _, s) -> stripSuffix s a)

rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_2
    :: (Eq a, Show a, RightGCDMonoid a) => a -> a -> Property
rightGCDMonoidLaw_stripCommonSuffix_stripSuffix_2 a b =
    makeProperty
        "stripCommonSuffix a b & λ(_, x, s) -> Just x == stripSuffix s b"
        (stripCommonSuffix a b & \(_, x, s) -> Just x == stripSuffix s b)
    & cover
        "stripCommonSuffix a b & λ(_, x, s) -> x /= mempty && s /= mempty"
        (stripCommonSuffix a b & \(_, x, s) -> x /= mempty && s /= mempty)
    & report
        "stripCommonSuffix a b"
        (stripCommonSuffix a b)
    & report
        "stripCommonSuffix a b & λ(_, _, s) -> stripSuffix s b"
        (stripCommonSuffix a b & \(_, _, s) -> stripSuffix s b)
