{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fplugin=IfSat.Plugin #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2022 Jonathan Knowles
-- License: Apache-2.0
--
module Test.QuickCheck.Classes.SemigroupSpec where

import Control.Monad
    ( forM_ )
import Data.ByteString.Lazy
    ( ByteString )
import Data.Kind
    ( Constraint, Type )
import Data.Constraint.If
    ( IfSat, ifSat )
import Data.IntMap.Strict
    ( IntMap )
import Data.Map.Strict
    ( Map )
import Data.Monoid
    ( Product (..), Sum (..) )
import Data.Monoid.GCD
    ( GCDMonoid, LeftGCDMonoid, RightGCDMonoid, OverlappingGCDMonoid )
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
    )
import Data.Sequence
    ( Seq )
import Data.Set
    ( Set )
import Data.Text
    ( Text )
import Data.Typeable
    ( Typeable )
import Data.Vector
    ( Vector )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec )
import Test.QuickCheck
    ( Arbitrary, Property )
import Test.QuickCheck.Classes
    ( Laws (..) )
import Test.QuickCheck.Classes.Hspec
    ( testLaws )
import Test.QuickCheck.Classes.Monoid.GCD
    ( cancellativeGCDMonoidLaws
    , gcdMonoidLaws
    , leftGCDMonoidLaws
    , overlappingGCDMonoidLaws
    , rightGCDMonoidLaws
    )
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

data TestType = forall a. TestConstraints a => TestType (Proxy a)

data TestLaw =
    forall (c :: Type -> Constraint). TestLaw (forall a. Proxy a -> Laws)

alltestLaws :: [TestLaw]
alltestLaws =
    [ TestLaw @LeftReductive (leftReductiveLaws)
    ]

testTypes :: [TestType]
testTypes =
    [ TestType (Proxy @(IntMap ()))
    , TestType (Proxy @(IntMap Int))
    , TestType (Proxy @(IntMap Natural))
    , TestType (Proxy @(Map Int Int))
    , TestType (Proxy @(Map Int Natural))
    , TestType (Proxy @(Product Int))
    , TestType (Proxy @(Product Natural))
    , TestType (Proxy @(Seq Int))
    , TestType (Proxy @(Seq Natural))
    , TestType (Proxy @(Set Int))
    , TestType (Proxy @(Set Natural))
    , TestType (Proxy @(Sum Int))
    , TestType (Proxy @(Sum Natural))
    , TestType (Proxy @(Vector Int))
    , TestType (Proxy @(Vector Natural))
    , TestType (Proxy @ByteString)
    , TestType (Proxy @Text)
    , TestType (Proxy @[Int])
    , TestType (Proxy @[Natural])
    ]

type TestConstraints a =
    ( TestConstraint a Cancellative
    , TestConstraint a Commutative
    , TestConstraint a GCDMonoid
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
    )

testLawsAll
    :: forall a. TestConstraints a
    => Proxy a
    -> Spec
testLawsAll p = do
    testCancellativeGCDMonoidLaws p
    testCancellativeLaws p
    testCommutativeLaws p
    testGCDMonoidLaws p
    testLeftCancellativeLaws p
    testLeftGCDMonoidLaws p
    testLeftReductiveLaws p
    testMonoidNullLaws p
    testMonusLaws p
    testOverlappingGCDMonoidLaws p
    testPositiveMonoidLaws p
    testReductiveLaws p
    testRightCancellativeLaws p
    testRightGCDMonoidLaws p
    testRightReductiveLaws p

testCancellativeLaws
    :: forall a. TestConstraints a => Proxy a -> Spec
testCancellativeLaws _ =
    ifSat @(Cancellative a)
        (testLaws @a cancellativeLaws)
        (pure ())

testCommutativeLaws
    :: forall a. TestConstraints a => Proxy a -> Spec
testCommutativeLaws _ =
    ifSat @(Commutative a)
        (testLaws @a commutativeLaws)
        (pure ())

testCancellativeGCDMonoidLaws
    :: forall a. TestConstraints a => Proxy a -> Spec
testCancellativeGCDMonoidLaws _ =
    ifSat @(Cancellative a)
        (ifSat @(GCDMonoid a) (testLaws @a cancellativeGCDMonoidLaws) (pure ()))
        (pure ())

testGCDMonoidLaws
    :: forall a. TestConstraints a => Proxy a -> Spec
testGCDMonoidLaws _ =
    ifSat @(GCDMonoid a)
        (testLaws @a gcdMonoidLaws)
        (pure ())

testLeftGCDMonoidLaws
    :: forall a. TestConstraints a => Proxy a -> Spec
testLeftGCDMonoidLaws _ =
    ifSat @(LeftGCDMonoid a)
        (testLaws @a leftGCDMonoidLaws)
        (pure ())

testRightGCDMonoidLaws
    :: forall a. TestConstraints a => Proxy a -> Spec
testRightGCDMonoidLaws _ =
    ifSat @(RightGCDMonoid a)
        (testLaws @a rightGCDMonoidLaws)
        (pure ())

testLeftCancellativeLaws
    :: forall a. TestConstraints a => Proxy a -> Spec
testLeftCancellativeLaws _ =
    ifSat @(LeftCancellative a)
        (testLaws @a leftCancellativeLaws)
        (pure ())

testLeftReductiveLaws
    :: forall a. TestConstraints a => Proxy a -> Spec
testLeftReductiveLaws _ =
    ifSat @(LeftReductive a)
        (testLaws @a leftReductiveLaws)
        (pure ())

testRightCancellativeLaws
    :: forall a. TestConstraints a => Proxy a -> Spec
testRightCancellativeLaws _ =
    ifSat @(RightCancellative a)
        (testLaws @a rightCancellativeLaws)
        (pure ())

testRightReductiveLaws
    :: forall a. TestConstraints a => Proxy a -> Spec
testRightReductiveLaws _ =
    ifSat @(RightReductive a)
        (testLaws @a rightReductiveLaws)
        (pure ())

testReductiveLaws
    :: forall a. TestConstraints a => Proxy a -> Spec
testReductiveLaws _ =
    ifSat @(Reductive a)
        (testLaws @a reductiveLaws)
        (pure ())

testMonoidNullLaws
    :: forall a. TestConstraints a => Proxy a -> Spec
testMonoidNullLaws _ =
    ifSat @(MonoidNull a)
        (testLaws @a monoidNullLaws)
        (pure ())

testMonusLaws
    :: forall a. TestConstraints a => Proxy a -> Spec
testMonusLaws _ =
    ifSat @(Monus a)
        (testLaws @a monusLaws)
        (pure ())

testPositiveMonoidLaws
    :: forall a. TestConstraints a => Proxy a -> Spec
testPositiveMonoidLaws _ =
    ifSat @(PositiveMonoid a)
        (testLaws @a positiveMonoidLaws)
        (pure ())

testOverlappingGCDMonoidLaws
    :: forall a. TestConstraints a => Proxy a -> Spec
testOverlappingGCDMonoidLaws _ =
    ifSat @(OverlappingGCDMonoid a)
        (testLaws @a overlappingGCDMonoidLaws)
        (pure ())

--------------------------------------------------------------------------------
-- Coverage checks
--------------------------------------------------------------------------------

class HasCoverageCheck p where
    disableCoverageCheck :: p -> p

instance HasCoverageCheck Laws where
    disableCoverageCheck (Laws title laws) =
        Laws title $ fmap disableCoverageCheck <$> laws

instance HasCoverageCheck Property where
    disableCoverageCheck =
        mapTotalResult (\r -> r {maybeCheckCoverage = Nothing})

instance (Functor f, HasCoverageCheck p) => HasCoverageCheck (f p) where
    disableCoverageCheck =
        fmap disableCoverageCheck
