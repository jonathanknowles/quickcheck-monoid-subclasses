{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Redundant bracket" -}

-- |
-- Copyright: © 2022 Jonathan Knowles
-- License: Apache-2.0
--
-- This module provides:
--
-- * Support for testing 'Semigroup' subclass instances with QuickCheck.
-- * Reusable properties in the form of 'Laws' definitions.
--
-- In general:
--
-- * Each 'Laws' definition includes properties that relate to __just one__
--   __class__.
-- * Laws for superclasses are __not__ transitively included.
--
-- Therefore, when testing laws for a particular class, you should make sure to
-- also test laws for all superclasses.
--
module Test.QuickCheck.Classes.Semigroup
    (
    -- * Monus
      monusLaws
    )
    where

import Prelude hiding
    ( gcd, null )

import Data.Monoid.GCD
    ( OverlappingGCDMonoid (..) )
import Data.Monoid.Monus
    ( Monus (..) )
import Data.Proxy
    ( Proxy (..) )
import Test.QuickCheck
    ( Arbitrary (..), Property )
import Test.QuickCheck.Classes
    ( Laws (..) )
import Test.QuickCheck.Classes.Semigroup.Internal
    ( makeLaw2, makeProperty )

--------------------------------------------------------------------------------
-- Monus
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'Monus'.
--
-- Tests the following properties:
--
-- prop> a <\> b == stripPrefixOverlap b a
-- prop> a <\> b == stripSuffixOverlap b a
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'commutativeLaws'
-- * 'overlappingGCDMonoidLaws'
--
monusLaws
    :: forall a. (Arbitrary a, Show a, Eq a, Monus a)
    => Proxy a
    -> Laws
monusLaws _ = Laws "Monus"
    [ makeLaw2 @a
        "monusLaw_stripPrefixOverlap"
        (monusLaw_stripPrefixOverlap)
    , makeLaw2 @a
        "monusLaw_stripSuffixOverlap"
        (monusLaw_stripSuffixOverlap)
    ]

monusLaw_stripPrefixOverlap
    :: (Eq a, Monus a) => a -> a -> Property
monusLaw_stripPrefixOverlap a b =
    makeProperty
        "a <\\> b == stripPrefixOverlap b a"
        (a <\\> b == stripPrefixOverlap b a)
  where
    (<\\>) = (<\>)

monusLaw_stripSuffixOverlap
    :: (Eq a, Monus a) => a -> a -> Property
monusLaw_stripSuffixOverlap a b =
    makeProperty
        "a <\\> b == stripSuffixOverlap b a"
        (a <\\> b == stripSuffixOverlap b a)
  where
    (<\\>) = (<\>)
