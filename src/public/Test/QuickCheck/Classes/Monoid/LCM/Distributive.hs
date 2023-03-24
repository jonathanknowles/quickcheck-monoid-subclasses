{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Redundant bracket" -}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
-- This module provides 'Laws' definitions for classes exported by
-- "Data.Monoid.LCM.Distributive".
--
module Test.QuickCheck.Classes.Monoid.LCM.Distributive
    ( distributiveLCMMonoidLaws
    )
    where

import Prelude hiding
    ( gcd, lcm )

import Data.Function
    ( (&) )
import Data.Maybe
    ( isJust )
import Data.Monoid.GCD
    ( GCDMonoid (gcd) )
import Data.Monoid.LCM
    ( LCMMonoid (lcm) )
import Data.Monoid.LCM.Distributive
    ( DistributiveLCMMonoid (..) )
import Data.Proxy
    ( Proxy (..) )
import Internal
    ( cover, makeLaw1, makeLaw2, makeLaw3, makeProperty, report, (==>) )
import Test.QuickCheck
    ( Arbitrary (..), Property )
import Test.QuickCheck.Classes
    ( Laws (..) )

--------------------------------------------------------------------------------
-- DistributiveLCMMonoid
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'DistributiveLCMMonoid'.
--
-- Includes the following laws:
--
-- @
-- 'lcm' (a '<>' b) (a '<>' c) '==' a '<>' 'lcm' b c
-- @
-- @
-- 'lcm' (a '<>' c) (b '<>' c) '==' 'lcm' a b '<>' c
-- @
-- @
-- 'lcm' a ('gcd' b c) '==' 'gcd' ('lcm' a b) ('lcm' a c)
-- @
-- @
-- 'gcd' a ('lcm' b c) '==' 'lcm' ('gcd' a b) ('gcd' a c)
-- @
--
distributiveLCMMonoidLaws
    :: forall a. (Arbitrary a, Show a, Eq a, DistributiveLCMMonoid a)
    => Proxy a
    -> Laws
distributiveLCMMonoidLaws _ = Laws "DistributiveLCMMonoid"
    [ makeLaw3 @a
        "distributiveLCMMonoidLaw_distributivity_left"
        (distributiveLCMMonoidLaw_distributivity_left)
    , makeLaw3 @a
        "distributiveLCMMonoidLaw_distributivity_right"
        (distributiveLCMMonoidLaw_distributivity_right)
    , makeLaw3 @a
        "distributiveLCMMonoidLaw_distributivity_gcd_lcm"
        (distributiveLCMMonoidLaw_distributivity_gcd_lcm)
    , makeLaw3 @a
        "distributiveLCMMonoidLaw_distributivity_lcm_gcd"
        (distributiveLCMMonoidLaw_distributivity_lcm_gcd)
    ]

distributiveLCMMonoidLaw_distributivity_left
    :: (Eq a, Show a, DistributiveLCMMonoid a) => a -> a -> a -> Property
distributiveLCMMonoidLaw_distributivity_left a b c =
    makeProperty
        "lcm (a <> b) (a <> c) == a <> lcm b c"
        (lcm (a <> b) (a <> c) == a <> lcm b c)
    & report
        "a <> b"
        (a <> b)
    & report
        "a <> c"
        (a <> c)
    & report
        "lcm (a <> b) (a <> c)"
        (lcm (a <> b) (a <> c))
    & report
        "lcm b c"
        (lcm b c)
    & report
        "a <> lcm b c"
        (a <> lcm b c)

distributiveLCMMonoidLaw_distributivity_right
    :: (Eq a, Show a, DistributiveLCMMonoid a) => a -> a -> a -> Property
distributiveLCMMonoidLaw_distributivity_right a b c =
    makeProperty
        "lcm (a <> c) (b <> c) == lcm a b <> c"
        (lcm (a <> c) (b <> c) == lcm a b <> c)
    & report
        "a <> c"
        (a <> c)
    & report
        "b <> c"
        (b <> c)
    & report
        "lcm (a <> c) (b <> c)"
        (lcm (a <> c) (b <> c))
    & report
        "lcm a b"
        (lcm a b)
    & report
        "lcm a b <> c"
        (lcm a b <> c)

distributiveLCMMonoidLaw_distributivity_gcd_lcm
    :: (Eq a, Show a, DistributiveLCMMonoid a) => a -> a -> a -> Property
distributiveLCMMonoidLaw_distributivity_gcd_lcm a b c =
    makeProperty
        "lcm a (gcd b c) == gcd (lcm a b) (lcm a c)"
        (lcm a (gcd b c) == gcd (lcm a b) (lcm a c))

distributiveLCMMonoidLaw_distributivity_lcm_gcd
    :: (Eq a, Show a, DistributiveLCMMonoid a) => a -> a -> a -> Property
distributiveLCMMonoidLaw_distributivity_lcm_gcd a b c =
    makeProperty
        "gcd a (lcm b c) == lcm (gcd a b) (gcd a c)"
        (gcd a (lcm b c) == lcm (gcd a b) (gcd a c))
