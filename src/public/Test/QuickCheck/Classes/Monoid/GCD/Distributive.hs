{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Redundant bracket" -}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
-- This module provides 'Laws' definitions for classes exported by
-- "Data.Monoid.GCD.Distributive".
--
module Test.QuickCheck.Classes.Monoid.GCD.Distributive
    ( distributiveGCDMonoidLaws
    , leftDistributiveGCDMonoidLaws
    , rightDistributiveGCDMonoidLaws
    )
    where

import Prelude hiding
    ( gcd )

import Data.Function
    ( (&) )
import Data.Monoid.GCD
    ( GCDMonoid (gcd)
    , LeftGCDMonoid (commonPrefix)
    , RightGCDMonoid (commonSuffix)
    )
import Data.Monoid.GCD.Distributive
    ( DistributiveGCDMonoid
    , LeftDistributiveGCDMonoid
    , RightDistributiveGCDMonoid
    )
import Data.Proxy
    ( Proxy (..) )
import Internal
    ( cover, makeLaw1, makeLaw2, makeLaw3, makeProperty, report, (==>) )
import Test.QuickCheck
    ( Arbitrary (..), Property )
import Test.QuickCheck.Classes
    ( Laws (..) )

--------------------------------------------------------------------------------
-- DistributiveGCDMonoid
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'DistributiveGCDMonoid'.
--
-- Includes the following laws:
--
-- __/Left-distributivity/__
--
-- @
-- 'gcd' (a '<>' b) (a '<>' c) '==' a '<>' 'gcd' b c
-- @
--
-- __/Right-distributivity/__
--
-- @
-- 'gcd' (a '<>' c) (b '<>' c) '==' 'gcd' a b '<>' c
-- @
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'Test.QuickCheck.Classes.Monoid.GCD.gcdMonoidLaws'
-- * 'leftDistributiveGCDMonoidLaws'
-- * 'rightDistributiveGCDMonoidLaws'
--
distributiveGCDMonoidLaws
    :: forall a. (Arbitrary a, Show a, Eq a, DistributiveGCDMonoid a)
    => Proxy a
    -> Laws
distributiveGCDMonoidLaws _ = Laws "DistributiveGCDMonoid"
    [ makeLaw3 @a
        "distributiveGCDMonoidLaw_distributivity_left"
        (distributiveGCDMonoidLaw_distributivity_left)
    , makeLaw3 @a
        "distributiveGCDMonoidLaw_distributivity_right"
        (distributiveGCDMonoidLaw_distributivity_right)
    ]

distributiveGCDMonoidLaw_distributivity_left
    :: (Eq a, Show a, DistributiveGCDMonoid a) => a -> a -> a -> Property
distributiveGCDMonoidLaw_distributivity_left a b c =
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

distributiveGCDMonoidLaw_distributivity_right
    :: (Eq a, Show a, DistributiveGCDMonoid a) => a -> a -> a -> Property
distributiveGCDMonoidLaw_distributivity_right a b c =
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

--------------------------------------------------------------------------------
-- LeftDistributiveGCDMonoid
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'LeftDistributiveGCDMonoid'.
--
-- Includes the following law:
--
-- __/Distributivity/__
--
-- @
-- 'commonPrefix' (a '<>' b) (a '<>' c) '==' a '<>' 'commonPrefix' b c
-- @
leftDistributiveGCDMonoidLaws
    :: forall a. (Arbitrary a, Show a, Eq a, LeftDistributiveGCDMonoid a)
    => Proxy a
    -> Laws
leftDistributiveGCDMonoidLaws _ = Laws "LeftDistributiveGCDMonoid"
    [ makeLaw3 @a
        "leftDistributiveGCDMonoidLaw_distributivity"
        (leftDistributiveGCDMonoidLaw_distributivity)
    ]

leftDistributiveGCDMonoidLaw_distributivity
    :: (Eq a, Show a, LeftDistributiveGCDMonoid a) => a -> a -> a -> Property
leftDistributiveGCDMonoidLaw_distributivity a b c =
    makeProperty
        "commonPrefix (a <> b) (a <> c) == a <> commonPrefix b c"
        (commonPrefix (a <> b) (a <> c) == a <> commonPrefix b c)
    & cover
        "commonPrefix b c /= mempty && a /= mempty"
        (commonPrefix b c /= mempty && a /= mempty)
    & report
        "a <> b"
        (a <> b)
    & report
        "a <> c"
        (a <> c)
    & report
        "commonPrefix (a <> b) (a <> c)"
        (commonPrefix (a <> b) (a <> c))
    & report
        "commonPrefix b c"
        (commonPrefix b c)
    & report
        "a <> commonPrefix b c"
        (a <> commonPrefix b c)

--------------------------------------------------------------------------------
-- RightDistributiveGCDMonoid
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'RightDistributiveGCDMonoid'.
--
-- Includes the following law:
--
-- __/Distributivity/__
--
-- @
-- 'commonSuffix' (a '<>' c) (b '<>' c) '==' 'commonSuffix' a b '<>' c
-- @
--
rightDistributiveGCDMonoidLaws
    :: forall a. (Arbitrary a, Show a, Eq a, RightDistributiveGCDMonoid a)
    => Proxy a
    -> Laws
rightDistributiveGCDMonoidLaws _ = Laws "RightDistributiveGCDMonoid"
    [ makeLaw3 @a
        "rightDistributiveGCDMonoidLaw_distributivity"
        (rightDistributiveGCDMonoidLaw_distributivity)
    ]

rightDistributiveGCDMonoidLaw_distributivity
    :: (Eq a, Show a, RightDistributiveGCDMonoid a) => a -> a -> a -> Property
rightDistributiveGCDMonoidLaw_distributivity a b c =
    makeProperty
        "commonSuffix (a <> c) (b <> c) == commonSuffix a b <> c"
        (commonSuffix (a <> c) (b <> c) == commonSuffix a b <> c)
    & cover
        "commonSuffix a b /= mempty && c /= mempty"
        (commonSuffix a b /= mempty && c /= mempty)
    & report
        "a <> c"
        (a <> c)
    & report
        "b <> c"
        (b <> c)
    & report
        "commonSuffix (a <> c) (b <> c)"
        (commonSuffix (a <> c) (b <> c))
    & report
        "commonSuffix a b"
        (commonSuffix a b)
    & report
        "commonSuffix a b <> c"
        (commonSuffix a b <> c)
