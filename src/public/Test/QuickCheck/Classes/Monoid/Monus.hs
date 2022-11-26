{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Redundant bracket" -}

-- |
-- Copyright: © 2022 Jonathan Knowles
-- License: Apache-2.0
--
module Test.QuickCheck.Classes.Monoid.Monus
    (
    -- * Monus
      monusLaws
    )
    where

import Prelude hiding
    ( gcd, null )

import Data.Function
    ( (&) )
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
    ( makeLaw1, makeLaw2, makeLaw3, makeProperty, report )

--------------------------------------------------------------------------------
-- Monus
--------------------------------------------------------------------------------

-- | 'Laws' for instances of 'Monus'.
--
-- Tests the following properties:
--
-- prop> a <\> a == mempty
-- prop> mempty <\> a == mempty
-- prop> a <> (b <\> a) == b <> (a <\> b)
-- prop> (a <\> b) <\> c == a <\> (b <> c)
-- prop> a <\> b == stripPrefixOverlap b a
-- prop> a <\> b == stripSuffixOverlap b a
--
-- Note that the following superclass laws are __not__ included:
--
-- * 'Test.QuickCheck.Classes.Semigroup.Cancellative.commutativeLaws'
-- * 'Test.QuickCheck.Classes.Monoid.GCD.overlappingGCDMonoidLaws'
--
monusLaws
    :: forall a. (Arbitrary a, Show a, Eq a, Monus a)
    => Proxy a
    -> Laws
monusLaws _ = Laws "Monus"
    [ makeLaw1 @a
        "monusLaw_axiom_1"
        (monusLaw_axiom_1)
    , makeLaw1 @a
        "monusLaw_axiom_2"
        (monusLaw_axiom_2)
    , makeLaw2 @a
        "monusLaw_axiom_3"
        (monusLaw_axiom_3)
    , makeLaw3 @a
        "monusLaw_axiom_4"
        (monusLaw_axiom_4)
    , makeLaw2 @a
        "monusLaw_stripPrefixOverlap"
        (monusLaw_stripPrefixOverlap)
    , makeLaw2 @a
        "monusLaw_stripSuffixOverlap"
        (monusLaw_stripSuffixOverlap)
    ]

monusLaw_axiom_1
    :: (Eq a, Monus a, Show a) => a -> Property
monusLaw_axiom_1 a =
    makeProperty
        "a <\\> a == mempty"
        (a <\\> a == mempty)
  where
    (<\\>) = (<\>)

monusLaw_axiom_2
    :: (Eq a, Monus a, Show a) => a -> Property
monusLaw_axiom_2 a =
    makeProperty
        "mempty <\\> a == mempty"
        (mempty <\\> a == mempty)
  where
    (<\\>) = (<\>)

monusLaw_axiom_3
    :: (Eq a, Monus a, Show a) => a -> a -> Property
monusLaw_axiom_3 a b =
    makeProperty
        "a <> (b <\\> a) == b <> (a <\\> b)"
        (a <> (b <\\> a) == b <> (a <\\> b))
    & report
        "b <\\> a"
        (b <\\> a)
    & report
        "a <> (b <\\> a)"
        (a <> (b <\\> a))
    & report
        "a <\\> b"
        (a <\\> b)
    & report
        "b <> (a <\\> b)"
        (b <> (a <\\> b))
  where
    (<\\>) = (<\>)

monusLaw_axiom_4
    :: (Eq a, Monus a, Show a) => a -> a -> a -> Property
monusLaw_axiom_4 a b c =
    makeProperty
        "(a <\\> b) <\\> c == a <\\> (b <> c)"
        ((a <\\> b) <\\> c == a <\\> (b <> c))
    & report
        "a <\\> b"
        (a <\\> b)
    & report
        "(a <\\> b) <\\> c"
        ((a <\\> b) <\\> c)
    & report
        "b <> c"
        (b <> c)
    & report
        "a <\\> (b <> c)"
        (a <\\> (b <> c))
  where
    (<\\>) = (<\>)

monusLaw_stripPrefixOverlap
    :: (Eq a, Monus a, Show a) => a -> a -> Property
monusLaw_stripPrefixOverlap a b =
    makeProperty
        "a <\\> b == stripPrefixOverlap b a"
        (a <\\> b == stripPrefixOverlap b a)
    & report
        "a <\\> b"
        (a <\\> b)
    & report
        "stripPrefixOverlap b a"
        (stripPrefixOverlap b a)
  where
    (<\\>) = (<\>)

monusLaw_stripSuffixOverlap
    :: (Eq a, Monus a, Show a) => a -> a -> Property
monusLaw_stripSuffixOverlap a b =
    makeProperty
        "a <\\> b == stripSuffixOverlap b a"
        (a <\\> b == stripSuffixOverlap b a)
    & report
        "a <\\> b"
        (a <\\> b)
    & report
        "stripSuffixOverlap b a"
        (stripSuffixOverlap b a)
  where
    (<\\>) = (<\>)
