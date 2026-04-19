{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2022–2026 Jonathan Knowles
-- License: Apache-2.0
--
module ClassSpec where

import Prelude

import Data.ByteString.Lazy
    ( ByteString )
import Data.IntMap.Strict
    ( IntMap )
import Data.Map.Strict
    ( Map )
import Data.Monoid
    ( Dual (..), Product (..), Sum (..) )
import Data.Monoid.Factorial
    ( FactorialMonoid )
import Data.Monoid.GCD
    ( DistributiveGCDMonoid
    , GCDMonoid
    , LeftDistributiveGCDMonoid
    , LeftGCDMonoid
    , OverlappingGCDMonoid
    , RightDistributiveGCDMonoid
    , RightGCDMonoid
    )
import Data.Monoid.LCM
    ( DistributiveLCMMonoid, LCMMonoid )
import Data.Monoid.Monus
    ( Monus )
import Data.Monoid.Null
    ( MonoidNull, PositiveMonoid )
import Data.Semigroup.Cancellative
    ( Cancellative
    , Commutative
    , LeftCancellative
    , LeftReductive
    , Reductive
    , RightCancellative
    , RightReductive
    , SumCancellative
    )
import Data.Semigroup.Factorial
    ( Factorial, StableFactorial )
import Data.Sequence
    ( Seq )
import Data.Set
    ( Set )
import Data.Text
    ( Text )
import Data.Vector
    ( Vector )
import Numeric.Natural
    ( Natural )
import Numeric.Product.Commutative
    ( CommutativeProduct )
import Test.Hspec
    ( Spec )
import Test.Hspec.QuickCheck.Classes
    ( testLaws )
import Test.QuickCheck
    ( Arbitrary (..)
    , Confidence
    , Property
    , elements
    , frequency
    , listOf
    , scale
    , shrinkIntegral
    , shrinkMap
    , shrinkMapBy
    )
import Test.QuickCheck.Classes
    ( Laws (..) )
import Test.QuickCheck.Classes.Monoid.Factorial
    ( factorialMonoidLaws )
import Test.QuickCheck.Classes.Monoid.GCD
    ( distributiveGCDMonoidLaws
    , gcdMonoidLaws
    , leftDistributiveGCDMonoidLaws
    , leftGCDMonoidLaws
    , overlappingGCDMonoidLaws
    , rightDistributiveGCDMonoidLaws
    , rightGCDMonoidLaws
    )
import Test.QuickCheck.Classes.Monoid.LCM
    ( distributiveLCMMonoidLaws, lcmMonoidLaws )
import Test.QuickCheck.Classes.Monoid.Monus
    ( monusLaws )
import Test.QuickCheck.Classes.Monoid.Null
    ( monoidNullLaws, positiveMonoidLaws )
import Test.QuickCheck.Classes.Semigroup.Cancellative
    ( cancellativeLaws
    , commutativeLaws
    , leftCancellativeLaws
    , leftReductiveLaws
    , reductiveLaws
    , rightCancellativeLaws
    , rightReductiveLaws
    )
import Test.QuickCheck.Classes.Semigroup.Factorial
    ( factorialLaws, stableFactorialLaws )
import Test.QuickCheck.Property
    ( Result (..), mapTotalResult )

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text as Text
import qualified Data.Vector as Vector

spec :: Spec
spec = do
    testLaws @() $ fmap disableCoverageCheck <$>
        [ cancellativeLaws
        , commutativeLaws
        , distributiveGCDMonoidLaws
        , distributiveLCMMonoidLaws
        , factorialLaws
        , factorialMonoidLaws
        , gcdMonoidLaws
        , lcmMonoidLaws
        , leftCancellativeLaws
        , leftDistributiveGCDMonoidLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , monusLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , reductiveLaws
        , rightCancellativeLaws
        , rightDistributiveGCDMonoidLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        , stableFactorialLaws
        ]
    testLaws @ByteString
        [ factorialLaws
        , factorialMonoidLaws
        , leftCancellativeLaws
        , leftDistributiveGCDMonoidLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , rightCancellativeLaws
        , rightDistributiveGCDMonoidLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        , stableFactorialLaws
        ]
    testLaws @(Dual ByteString)
        [ factorialLaws
        , factorialMonoidLaws
        , leftCancellativeLaws
        , leftDistributiveGCDMonoidLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , rightCancellativeLaws
        , rightDistributiveGCDMonoidLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        , stableFactorialLaws
        ]
    testLaws @Text
        [ factorialLaws
        , factorialMonoidLaws
        , leftCancellativeLaws
        , leftDistributiveGCDMonoidLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , rightCancellativeLaws
        , rightDistributiveGCDMonoidLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        , stableFactorialLaws
        ]
    testLaws @(Dual Text)
        [ factorialLaws
        , factorialMonoidLaws
        , leftCancellativeLaws
        , leftDistributiveGCDMonoidLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , rightCancellativeLaws
        , rightDistributiveGCDMonoidLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        , stableFactorialLaws
        ]
    testLaws @[Small Int]
        [ factorialLaws
        , factorialMonoidLaws
        , leftCancellativeLaws
        , leftDistributiveGCDMonoidLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , rightCancellativeLaws
        , rightDistributiveGCDMonoidLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        , stableFactorialLaws
        ]
    testLaws @(Dual [Small Int])
        [ factorialLaws
        , factorialMonoidLaws
        , leftCancellativeLaws
        , leftDistributiveGCDMonoidLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , rightCancellativeLaws
        , rightDistributiveGCDMonoidLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        , stableFactorialLaws
        ]
    testLaws @(Seq (Small Int))
        [ factorialLaws
        , factorialMonoidLaws
        , leftCancellativeLaws
        , leftDistributiveGCDMonoidLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , rightCancellativeLaws
        , rightDistributiveGCDMonoidLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        , stableFactorialLaws
        ]
    testLaws @(Dual (Seq (Small Int)))
        [ factorialLaws
        , factorialMonoidLaws
        , leftCancellativeLaws
        , leftDistributiveGCDMonoidLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , rightCancellativeLaws
        , rightDistributiveGCDMonoidLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        , stableFactorialLaws
        ]
    testLaws @(Vector (Small Int))
        [ factorialLaws
        , factorialMonoidLaws
        , leftCancellativeLaws
        , leftDistributiveGCDMonoidLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , rightCancellativeLaws
        , rightDistributiveGCDMonoidLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        , stableFactorialLaws
        ]
    testLaws @(Dual (Vector (Small Int)))
        [ factorialLaws
        , factorialMonoidLaws
        , leftCancellativeLaws
        , leftDistributiveGCDMonoidLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , rightCancellativeLaws
        , rightDistributiveGCDMonoidLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        , stableFactorialLaws
        ]
    testLaws @(Set (Small Int))
        [ commutativeLaws
        , distributiveGCDMonoidLaws
        , distributiveLCMMonoidLaws
        , factorialLaws
        , factorialMonoidLaws
        , gcdMonoidLaws
        , lcmMonoidLaws
        , leftDistributiveGCDMonoidLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , monusLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , reductiveLaws
        , rightDistributiveGCDMonoidLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        ]
    testLaws @(Set (Small Natural))
        [ commutativeLaws
        , distributiveGCDMonoidLaws
        , distributiveLCMMonoidLaws
        , factorialLaws
        , factorialMonoidLaws
        , gcdMonoidLaws
        , lcmMonoidLaws
        , leftDistributiveGCDMonoidLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , monusLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , reductiveLaws
        , rightDistributiveGCDMonoidLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        ]
    testLaws @(Product (Small Int))
        [ commutativeLaws
        , factorialLaws
        , factorialMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , reductiveLaws
        , rightReductiveLaws
        ]
    testLaws @(Small (Product Natural))
        [ commutativeLaws
        , distributiveGCDMonoidLaws
        , distributiveLCMMonoidLaws
        , factorialLaws
        , factorialMonoidLaws
        , gcdMonoidLaws
        , lcmMonoidLaws
        , leftDistributiveGCDMonoidLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , monusLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , reductiveLaws
        , rightDistributiveGCDMonoidLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        ]
    testLaws @(Sum (Small Int))
        [ cancellativeLaws
        , commutativeLaws
        , factorialLaws
        , factorialMonoidLaws
        , leftCancellativeLaws
        , leftReductiveLaws
        , monoidNullLaws
        , reductiveLaws
        , rightCancellativeLaws
        , rightReductiveLaws
        ]
    testLaws @(Small (Sum Natural))
        [ cancellativeLaws
        , commutativeLaws
        , distributiveGCDMonoidLaws
        , distributiveLCMMonoidLaws
        , factorialLaws
        , factorialMonoidLaws
        , gcdMonoidLaws
        , lcmMonoidLaws
        , leftCancellativeLaws
        , leftDistributiveGCDMonoidLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , monusLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , reductiveLaws
        , rightCancellativeLaws
        , rightDistributiveGCDMonoidLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        , stableFactorialLaws
        ]
    testLaws @(IntMap (Small Int))
        [ factorialLaws
        , factorialMonoidLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , rightReductiveLaws
        ]
    testLaws @(IntMap (Small Natural))
        [ factorialLaws
        , factorialMonoidLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , rightReductiveLaws
        ]
    testLaws @(Map Int (Small Int))
        [ factorialLaws
        , factorialMonoidLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , rightReductiveLaws
        ]
    testLaws @(Map Int (Small Natural))
        [ factorialLaws
        , factorialMonoidLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , rightReductiveLaws
        ]
    testLaws @(Maybe ()) $ fmap disableCoverageCheck <$>
        [ commutativeLaws
        , factorialLaws
        , factorialMonoidLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , monusLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , reductiveLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        ]
    testLaws @(Maybe (Product (Small Int)))
        [ commutativeLaws
        , factorialLaws
        , factorialMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , positiveMonoidLaws
        , reductiveLaws
        , rightReductiveLaws
        ]
    testLaws @(Maybe (Small (Product Natural)))
        [ commutativeLaws
        , factorialLaws
        , factorialMonoidLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , monusLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , reductiveLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        ]
    testLaws @(Maybe (Sum (Small Int)))
        [ commutativeLaws
        , factorialLaws
        , factorialMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , positiveMonoidLaws
        , reductiveLaws
        , rightReductiveLaws
        ]
    testLaws @(Maybe (Small (Sum Natural)))
        [ commutativeLaws
        , factorialLaws
        , factorialMonoidLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , monusLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , reductiveLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        ]

--------------------------------------------------------------------------------
-- Notes
--------------------------------------------------------------------------------

{- All laws tested in this module:

        [ cancellativeLaws
        , commutativeLaws
        , distributiveGCDMonoidLaws
        , distributiveLCMMonoidLaws
        , factorialLaws
        , factorialMonoidLaws
        , gcdMonoidLaws
        , lcmMonoidLaws
        , leftCancellativeLaws
        , leftDistributiveGCDMonoidLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , monusLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , reductiveLaws
        , rightCancellativeLaws
        , rightDistributiveGCDMonoidLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        , stableFactorialLaws
        ]
-}

--------------------------------------------------------------------------------
-- Utility types
--------------------------------------------------------------------------------

newtype Small a = Small {getSmall :: a}
    deriving stock Functor
    deriving newtype
        ( Cancellative
        , Commutative
        , CommutativeProduct
        , DistributiveGCDMonoid
        , DistributiveLCMMonoid
        , Enum
        , Eq
        , Factorial
        , FactorialMonoid
        , GCDMonoid
        , Integral
        , LCMMonoid
        , LeftCancellative
        , LeftDistributiveGCDMonoid
        , LeftGCDMonoid
        , LeftReductive
        , Monoid
        , MonoidNull
        , Monus
        , Num
        , Ord
        , OverlappingGCDMonoid
        , PositiveMonoid
        , Real
        , Reductive
        , RightCancellative
        , RightDistributiveGCDMonoid
        , RightGCDMonoid
        , RightReductive
        , Semigroup
        , Show
        , StableFactorial
        , SumCancellative
        )

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary (Small Int) where
    arbitrary = Small <$> scale (`div` 2) arbitrary
    shrink = shrinkMap Small getSmall

instance Arbitrary ByteString where
    arbitrary = ByteString.pack <$> listOf genByte
      where
        genByte = frequency
            [ (64, pure 0)
            , (16, pure 1)
            , ( 4, pure 2)
            , ( 1, pure 3)
            ]
    shrink = shrinkMap ByteString.pack ByteString.unpack

instance Arbitrary Text where
    arbitrary = Text.pack <$> listOf genChar
      where
        genChar = frequency
            [ (64, pure 'a')
            , (16, pure 'b')
            , ( 4, pure 'c')
            , ( 1, pure 'd')
            ]
    shrink = shrinkMap Text.pack Text.unpack

instance Arbitrary (Small Natural) where
    arbitrary = Small <$> elements [0 .. 3]
    shrink = shrinkMapBy Small getSmall shrinkIntegral

instance Arbitrary (Small (Product Natural)) where
    arbitrary = fmap Product <$> arbitrary
    shrink = shrinkMap (fmap Product) (fmap getProduct)

instance Arbitrary (Small (Sum Natural)) where
    arbitrary = fmap Sum <$> arbitrary
    shrink = shrinkMap (fmap Sum) (fmap getSum)

instance Arbitrary a => Arbitrary (Vector a) where
    arbitrary = Vector.fromList <$> arbitrary
    shrink = shrinkMap Vector.fromList Vector.toList

--------------------------------------------------------------------------------
-- Coverage checks
--------------------------------------------------------------------------------

class HasCoverageCheck p where
    disableCoverageCheck :: p -> p

instance HasCoverageCheck Laws where
    disableCoverageCheck = mapLawsProperties disableCoverageCheck

instance HasCoverageCheck Property where
    disableCoverageCheck = mapPropertyCheckCoverage (const Nothing)

mapLawsProperties
    :: (Property -> Property) -> Laws -> Laws
mapLawsProperties f (Laws t ps) = Laws t $ fmap f <$> ps

mapPropertyCheckCoverage
    :: (Maybe Confidence -> Maybe Confidence) -> Property -> Property
mapPropertyCheckCoverage f =
    mapTotalResult $ \r -> r {maybeCheckCoverage = f (maybeCheckCoverage r)}
