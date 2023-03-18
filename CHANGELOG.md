# 0.2.0.0

- Improved generation of arbitrary `Semigroup` value combinations.
- Added derived laws for the `LeftGCDMonoid` type class.
- Added derived laws for the `RightGCDMonoid` type class.
- Removed hard-to-satisfy coverage check from `LCMMonoid` type class laws.
- Added support for building with GHC `9.6` series.

# 0.1.0.0

- Added laws for the `LCMMonoid` type class.
- Added laws for the `GCDMonoid` type class to match those of `LCMMonoid`.

# 0.0.0.1

- Revised lower dependency bound for the `vector` package.

# 0.0.0.0

- Provides support for testing instances of classes defined in the following
  modules:
    - `Data.Monoid.GCD`
    - `Data.Monoid.Monus`
    - `Data.Monoid.Null`
    - `Data.Semigroup.Cancellative`
