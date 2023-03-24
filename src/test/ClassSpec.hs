{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2022–2023 Jonathan Knowles
-- License: Apache-2.0
--
module ClassSpec where

import Data.ByteString.Lazy
    ( ByteString )
import Data.IntMap.Strict
    ( IntMap )
import Data.Map.Strict
    ( Map )
import Data.Monoid
    ( Dual (..), Product (..), Sum (..) )
import Data.Monoid.GCD
    ( GCDMonoid, LeftGCDMonoid, OverlappingGCDMonoid, RightGCDMonoid )
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
    ( testLawsMany )
import Test.QuickCheck
    ( Arbitrary (..)
    , Confidence
    , Property
    , arbitrarySizedIntegral
    , scale
    , shrinkMap
    )
import Test.QuickCheck.Classes
    ( Laws (..) )
import Test.QuickCheck.Classes.Monoid.GCD
    ( gcdMonoidLaws
    , leftGCDMonoidLaws
    , overlappingGCDMonoidLaws
    , rightGCDMonoidLaws
    )
import Test.QuickCheck.Classes.Monoid.GCD.Distributive
    ( distributiveGCDMonoidLaws
    , leftDistributiveGCDMonoidLaws
    , rightDistributiveGCDMonoidLaws
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
spec = do
    testLawsMany @() $ fmap disableCoverageCheck <$>
        [ cancellativeLaws
        , commutativeLaws
        , distributiveGCDMonoidLaws
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
        ]
    testLawsMany @ByteString
        [ leftCancellativeLaws
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
        ]
    testLawsMany @(Dual ByteString)
        [ leftCancellativeLaws
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
        ]
    testLawsMany @Text
        [ leftCancellativeLaws
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
        ]
    testLawsMany @(Dual Text)
        [ leftCancellativeLaws
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
        ]
    testLawsMany @[SmallInt]
        [ leftCancellativeLaws
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
        ]
    testLawsMany @(Dual [SmallInt])
        [ leftCancellativeLaws
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
        ]
    testLawsMany @(Seq SmallInt)
        [ leftCancellativeLaws
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
        ]
    testLawsMany @(Dual (Seq SmallInt))
        [ leftCancellativeLaws
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
        ]
    testLawsMany @(Vector SmallInt)
        [ leftCancellativeLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , rightCancellativeLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        ]
    testLawsMany @(Dual (Vector SmallInt))
        [ leftCancellativeLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , rightCancellativeLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        ]
    testLawsMany @(Set SmallInt)
        [ commutativeLaws
        , distributiveGCDMonoidLaws
        , gcdMonoidLaws
        , leftDistributiveGCDMonoidLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , lcmMonoidLaws
        , monoidNullLaws
        , monusLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , reductiveLaws
        , rightDistributiveGCDMonoidLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        ]
    testLawsMany @(Set Natural)
        [ commutativeLaws
        , distributiveGCDMonoidLaws
        , gcdMonoidLaws
        , leftDistributiveGCDMonoidLaws
        , leftGCDMonoidLaws
        , leftReductiveLaws
        , lcmMonoidLaws
        , monoidNullLaws
        , monusLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , reductiveLaws
        , rightDistributiveGCDMonoidLaws
        , rightGCDMonoidLaws
        , rightReductiveLaws
        ]
    testLawsMany @(Product SmallInt)
        [ commutativeLaws
        , leftReductiveLaws
        , monoidNullLaws
        , reductiveLaws
        , rightReductiveLaws
        ]
    testLawsMany @(Product Natural)
        [ commutativeLaws
        , distributiveGCDMonoidLaws
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
        , rightGCDMonoidLaws
        , rightDistributiveGCDMonoidLaws
        , rightReductiveLaws
        ]
    testLawsMany @(Sum SmallInt)
        [ cancellativeLaws
        , commutativeLaws
        , leftCancellativeLaws
        , leftReductiveLaws
        , monoidNullLaws
        , reductiveLaws
        , rightCancellativeLaws
        , rightReductiveLaws
        ]
    testLawsMany @(Sum Natural)
        [ cancellativeLaws
        , commutativeLaws
        , distributiveGCDMonoidLaws
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
        ]
    testLawsMany @(IntMap SmallInt)
        [ leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , rightReductiveLaws
        ]
    testLawsMany @(IntMap Natural)
        [ leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , rightReductiveLaws
        ]
    testLawsMany @(Map Int SmallInt)
        [ leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , rightReductiveLaws
        ]
    testLawsMany @(Map Int Natural)
        [ leftGCDMonoidLaws
        , leftReductiveLaws
        , monoidNullLaws
        , overlappingGCDMonoidLaws
        , positiveMonoidLaws
        , rightReductiveLaws
        ]
    testLawsMany @(Maybe ()) $ fmap disableCoverageCheck <$>
        [ commutativeLaws
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
    testLawsMany @(Maybe (Product SmallInt))
        [ commutativeLaws
        , leftReductiveLaws
        , monoidNullLaws
        , positiveMonoidLaws
        , reductiveLaws
        , rightReductiveLaws
        ]
    testLawsMany @(Maybe (Product Natural))
        [ commutativeLaws
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
    testLawsMany @(Maybe (Sum SmallInt))
        [ commutativeLaws
        , leftReductiveLaws
        , monoidNullLaws
        , positiveMonoidLaws
        , reductiveLaws
        , rightReductiveLaws
        ]
    testLawsMany @(Maybe (Sum Natural))
        [ commutativeLaws
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
        ]
-}

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
