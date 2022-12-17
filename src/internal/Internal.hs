-- |
-- Copyright: © 2022 Jonathan Knowles
-- License: Apache-2.0
--
module Internal
    ( cover
    , makeLaw0
    , makeLaw1
    , makeLaw2
    , makeLaw3
    , makeProperty
    , report
    , (==>)
    )
    where

import Data.Function
    ( (&) )
import Data.Proxy
    ( Proxy (..) )
import Internal.Semigroup.Eq
    ( allUnique, canVerifyAllNonNull )
import Internal.Semigroup.Tuple
    ( Tuple1, Tuple2, Tuple3, evalTuple1, evalTuple2, evalTuple3 )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , Testable
    , checkCoverage
    , counterexample
    , property
    )

import qualified Test.QuickCheck as QC

infixr 0 ==>
(==>) :: Bool -> Bool -> Bool
a ==> b = not a || b

cover :: Testable t => String -> Bool -> t -> Property
cover = flip (QC.cover 1)

makeLaw :: Testable t => String -> t -> (String, Property)
makeLaw title t = (title, checkCoverage $ property t)

makeLaw0
    :: forall a. (Eq a, Monoid a)
    => String
    -> (Proxy a -> Property)
    -> (String, Property)
makeLaw0 s = makeLaw s . makeProperty0

makeLaw1
    :: (Arbitrary a, Show a, Eq a, Monoid a, Testable t)
    => String
    -> (a -> t)
    -> (String, Property)
makeLaw1 s = makeLaw s . makeProperty1

makeLaw2
    :: (Arbitrary a, Show a, Eq a, Semigroup a, Testable t)
    => String
    -> (a -> a -> t)
    -> (String, Property)
makeLaw2 s = makeLaw s . makeProperty2

makeLaw3
    :: (Arbitrary a, Show a, Eq a, Semigroup a, Testable t)
    => String
    -> (a -> a -> a -> t)
    -> (String, Property)
makeLaw3 s = makeLaw s . makeProperty3

makeProperty :: Testable t => String -> t -> Property
makeProperty propertyDescription t =
    property t & counterexample counterexampleText
  where
    counterexampleText = unlines
        [ "Property not satisfied:"
        , propertyDescription
            & fmap replaceSpecialChars
        ]
      where
        replaceSpecialChars = \case
            'λ'   -> '\\'
            other -> other

makeProperty0
    :: forall a t. Testable t
    => (Proxy a -> t)
    -> Property
makeProperty0 p = property $ p $ Proxy @a

makeProperty1
    :: (Eq a, Semigroup a, Testable t)
    => (a -> t)
    -> (Tuple1 a -> Property)
makeProperty1 p (evalTuple1 -> a)
    = property $ p a

makeProperty2
    :: (Eq a, Semigroup a, Testable t)
    => (a -> a -> t)
    -> (Tuple2 a -> Property)
makeProperty2 p (evalTuple2 -> (a, b))
    = cover
        "allUnique [a, b]"
        (allUnique [a, b])
    $ cover
        "canVerifyAllNonNull [a, b]"
        (canVerifyAllNonNull [a, b])
    $ cover
        "allUnique [a, b] && canVerifyAllNonNull [a, b]"
        (allUnique [a, b] && canVerifyAllNonNull [a, b])
    $ property $ p a b

makeProperty3
    :: (Eq a, Semigroup a, Testable t)
    => (a -> a -> a -> t)
    -> (Tuple3 a -> Property)
makeProperty3 p (evalTuple3 -> (a, b, c))
    = cover
        "allUnique [a, b, c]"
        (allUnique [a, b, c])
    $ cover
        "canVerifyAllNonNull [a, b, c]"
        (canVerifyAllNonNull [a, b, c])
    $ cover
        "allUnique [a, b, c] && canVerifyAllNonNull [a, b, c]"
        (allUnique [a, b, c] && canVerifyAllNonNull [a, b, c])
    $ property $ p a b c

report :: (Show a, Testable prop) => String -> a -> prop -> Property
report name a = counterexample $
    name <> ":\n" <> show a <> "\n"
