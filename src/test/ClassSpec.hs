{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fplugin=IfSat.Plugin #-}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
module ClassSpec where

import Control.Monad
    ( forM_ )
import Data.ByteString.Lazy
    ( ByteString )
import Data.Constraint.If
    ( IfSat, IsSat, ifSat )
import Data.Data
    ( Typeable )
import Data.IntMap.Strict
    ( IntMap )
import Data.Map.Strict
    ( Map )
import Data.Monoid
    ( Product (..), Sum (..) )
import Data.Monoid.GCD
    ( GCDMonoid, LeftGCDMonoid, OverlappingGCDMonoid, RightGCDMonoid )
import Data.Monoid.LCM
    ( LCMMonoid )
import Data.Monoid.Monus
    ( Monus )
import Data.Monoid.Null
    ( MonoidNull, PositiveMonoid )
import Data.Proxy
    ( Proxy (..) )
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
import Test.Hspec.Laws
    ( testLaws )
import Test.QuickCheck
    ( Arbitrary (..), Confidence, Property, scale, shrinkMap )
import Test.QuickCheck.Classes
    ( Laws (..) )
import Test.QuickCheck.Classes.Monoid.GCD
    ( gcdMonoidLaws
    , leftGCDMonoidLaws
    , overlappingGCDMonoidLaws
    , rightGCDMonoidLaws
    )
import Test.QuickCheck.Classes.Monoid.LCM
    ( lcmMonoidLaws )
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
import Test.QuickCheck.Instances.ByteString
    ()
import Test.QuickCheck.Instances.Natural
    ()
import Test.QuickCheck.Instances.Text
    ()
import Test.QuickCheck.Instances.Vector
    ()
import Test.QuickCheck.Property
    ( Result (..), mapTotalResult )

spec :: Spec
spec = forM_ testTypes $ \(TestType p) -> testLawsAll p

type TestConstraint a c = (Arbitrary a, Eq a, Show a, Typeable a, IfSat (c a))

type TestConstraints a =
    ( TestConstraint a Cancellative
    , TestConstraint a Commutative
    , TestConstraint a Factorial
    , TestConstraint a GCDMonoid
    , TestConstraint a LCMMonoid
    , TestConstraint a LeftCancellative
    , TestConstraint a LeftGCDMonoid
    , TestConstraint a LeftReductive
    , TestConstraint a MonoidNull
    , TestConstraint a Monus
    , TestConstraint a OverlappingGCDMonoid
    , TestConstraint a PositiveMonoid
    , TestConstraint a Reductive
    , TestConstraint a RightCancellative
    , TestConstraint a RightGCDMonoid
    , TestConstraint a RightReductive
    , TestConstraint a StableFactorial
    )

data TestType = forall a. TestConstraints a => TestType (Proxy a)

testTypes :: [TestType]
testTypes =
    [ TestType (Proxy @(IntMap Bool))
    , TestType (Proxy @(IntMap SmallInt))
    , TestType (Proxy @(IntMap Integer))
    , TestType (Proxy @(IntMap Natural))
    , TestType (Proxy @(Map Bool Bool))
    , TestType (Proxy @(Map Bool SmallInt))
    , TestType (Proxy @(Map Bool Integer))
    , TestType (Proxy @(Map Bool Natural))
    , TestType (Proxy @(Map SmallInt Bool))
    , TestType (Proxy @(Map SmallInt SmallInt))
    , TestType (Proxy @(Map SmallInt Integer))
    , TestType (Proxy @(Map SmallInt Natural))
    , TestType (Proxy @(Map Integer Bool))
    , TestType (Proxy @(Map Integer SmallInt))
    , TestType (Proxy @(Map Integer Integer))
    , TestType (Proxy @(Map Integer Natural))
    , TestType (Proxy @(Map Natural Bool))
    , TestType (Proxy @(Map Natural SmallInt))
    , TestType (Proxy @(Map Natural Integer))
    , TestType (Proxy @(Map Natural Natural))
    , TestType (Proxy @(Product SmallInt))
    , TestType (Proxy @(Product Integer))
    , TestType (Proxy @(Product Natural))
    , TestType (Proxy @(Seq ()))
    , TestType (Proxy @(Seq SmallInt))
    , TestType (Proxy @(Seq Integer))
    , TestType (Proxy @(Seq Natural))
    , TestType (Proxy @(Set ()))
    , TestType (Proxy @(Set SmallInt))
    , TestType (Proxy @(Set Integer))
    , TestType (Proxy @(Set Natural))
    , TestType (Proxy @(Sum SmallInt))
    , TestType (Proxy @(Sum Integer))
    , TestType (Proxy @(Sum Natural))
    , TestType (Proxy @(Vector ()))
    , TestType (Proxy @(Vector SmallInt))
    , TestType (Proxy @(Vector Integer))
    , TestType (Proxy @(Vector Natural))
    , TestType (Proxy @ByteString)
    , TestType (Proxy @Text)
    , TestType (Proxy @[()])
    , TestType (Proxy @[Int])
    , TestType (Proxy @[Integer])
    , TestType (Proxy @[Natural])
    ]

testLawsAll :: forall a. TestConstraints a => Proxy a -> Spec
testLawsAll _ = do
    testLawsSat @a @Cancellative         cancellativeLaws
    testLawsSat @a @Commutative          commutativeLaws
    testLawsSat @a @Factorial            factorialLaws
    testLawsSat @a @GCDMonoid            gcdMonoidLaws
    testLawsSat @a @LCMMonoid            lcmMonoidLaws
    testLawsSat @a @LeftCancellative     leftCancellativeLaws
    testLawsSat @a @LeftGCDMonoid        leftGCDMonoidLaws
    testLawsSat @a @LeftReductive        leftReductiveLaws
    testLawsSat @a @MonoidNull           monoidNullLaws
    testLawsSat @a @Monus                monusLaws
    testLawsSat @a @OverlappingGCDMonoid overlappingGCDMonoidLaws
    testLawsSat @a @PositiveMonoid       positiveMonoidLaws
    testLawsSat @a @Reductive            reductiveLaws
    testLawsSat @a @RightCancellative    rightCancellativeLaws
    testLawsSat @a @RightGCDMonoid       rightGCDMonoidLaws
    testLawsSat @a @RightReductive       rightReductiveLaws
    testLawsSat @a @StableFactorial      stableFactorialLaws

testLawsSat
    :: forall a c1. (IfSat (c1 a), TestConstraints a)
    => ((IsSat (c1 a) ~ 'True, c1 a) => (Proxy a -> Laws))
    -> Spec
testLawsSat laws =
    ifSat @(c1 a)
        (testLaws @a (fmap disableCoverageCheck laws))
        (pure ())

--------------------------------------------------------------------------------
-- Utility types
--------------------------------------------------------------------------------

type SmallInt = Small Int

newtype Small a = Small {getSmall :: a}
    deriving newtype
        ( Cancellative
        , Commutative
        , CommutativeProduct
        , Enum
        , Eq
        , GCDMonoid
        , Integral
        , LeftCancellative
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
        , RightGCDMonoid
        , RightReductive
        , Semigroup
        , Show
        , SumCancellative
        )

instance Arbitrary a => Arbitrary (Small a) where
    arbitrary = Small <$> scale (`div` 2) arbitrary
    shrink = shrinkMap Small getSmall

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
