{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

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
import Data.Constraint.If
    ( IfSat, IsSat, ifSat )
import Data.IntMap.Strict
    ( IntMap )
import Data.Map.Strict
    ( Map )
import Data.Monoid
    ( Product (..), Sum (..) )
import Data.Monoid.GCD
    ( GCDMonoid, LeftGCDMonoid, OverlappingGCDMonoid, RightGCDMonoid )
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
    ( Arbitrary )
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

spec :: Spec
spec = forM_ testTypes $ \(TestType p) -> testLawsAll p

type TestConstraint a c = (Arbitrary a, Eq a, Show a, Typeable a, IfSat (c a))

data TestType = forall a. TestConstraints a => TestType (Proxy a)

testTypes :: [TestType]
testTypes =
    [ TestType (Proxy @(IntMap Int))
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

testLawsAll :: forall a. TestConstraints a => Proxy a -> Spec
testLawsAll _ = do
    testLawsSat1 @a @Cancellative            cancellativeLaws
    testLawsSat1 @a @Commutative             commutativeLaws
    testLawsSat1 @a @GCDMonoid               gcdMonoidLaws
    testLawsSat1 @a @LeftCancellative        leftCancellativeLaws
    testLawsSat1 @a @LeftGCDMonoid           leftGCDMonoidLaws
    testLawsSat1 @a @LeftReductive           leftReductiveLaws
    testLawsSat1 @a @MonoidNull              monoidNullLaws
    testLawsSat1 @a @Monus                   monusLaws
    testLawsSat1 @a @OverlappingGCDMonoid    overlappingGCDMonoidLaws
    testLawsSat1 @a @PositiveMonoid          positiveMonoidLaws
    testLawsSat1 @a @Reductive               reductiveLaws
    testLawsSat1 @a @RightCancellative       rightCancellativeLaws
    testLawsSat1 @a @RightGCDMonoid          rightGCDMonoidLaws
    testLawsSat1 @a @RightReductive          rightReductiveLaws
    testLawsSat2 @a @Cancellative @GCDMonoid cancellativeGCDMonoidLaws

testLawsSat1
    :: forall a c1. (IfSat (c1 a), TestConstraints a)
    => ((IsSat (c1 a) ~ 'True, c1 a) => (Proxy a -> Laws))
    -> Spec
testLawsSat1 laws = ifSat @(c1 a) (testLaws @a laws) (pure ())

testLawsSat2
    :: forall a c1 c2.
        ( IfSat (c1 a)
        , IfSat (c2 a)
        , TestConstraints a
        )
    =>  ( ( IsSat (c1 a) ~ 'True
          , IsSat (c2 a) ~ 'True
          , c1 a
          , c2 a
          )
          => (Proxy a -> Laws)
        )
    -> Spec
testLawsSat2 laws = ifSat @(c1 a)
    (ifSat @(c2 a) (testLaws @a laws) (pure ()))
    (pure ())
