# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased
### Added
- `Weaken` instances for `SizeGreaterThan`, `SizeLessThan`.
- `weakenAndLeft`, `weakenAndRight`, `weakenOrLeft`, `weakenOrRight`
  type inference helper functions.

## [0.8] - 2022-10-09
### Changed
- on GHC >=9, make `refineTH` and `refineTH_` work in any monad
  `(Quote m, MonadFail m)`.
- bump `base`: "< 4.17" -> "< 4.18"
- bump `template-haskell`: "< 2.19" -> "< 2.20"
- bump `aeson`: "< 2.1" -> "< 2.2"

## [0.7] - 2022-07-01
### Changed
- make `Refined` predicate type `p` kind polymorphic (`p :: Type` -> `p :: k`)

## [0.6.3] - 2022-01-14
### Added
- `Hashable` instance for `Refined`
- `FromJSONKey` instance for `Refined`
- `ToJSONKey` instance for `Refined`
- `shrink` for `Refined`'s `Arbitrary` instance.
- `refineEither` function

### Changed
- improved efficiency of `strengthen`
- bump multiple dependency upper bounds

## [0.6.2] - 2021-01-31
### Changed
- `strengthen` no longer returns an `Either`, since the proof
  that it should always succeed is in its constraints.
- correct `success` documentation

## [0.6.1] - 2020-08-02
### Changed
- upper bound on QuickCheck: <2.14 -> <2.15

## [0.6] - 2020-07-21
### Changed
- `validate` now takes a `Proxy` as its first argument.
- All uses of prettyprinter are now just `Text`

### Removed
- Refined.These module
- Dependency on `prettyprinter`

### Fixed
- bug in `sized` internal helper that caused formatting issues
  in sized predicate errors

## [0.5.1] - 2020-07-14
### Changed
- `refineTH_` is now implemented in terms of `refineTH`
- Fix pretty-printing of `RefineException`s during compile-time

## [0.5] - 2020-07-11
### Added
- sized Predicate instances for `Text`
- sized Predicate instances for strict and lazy `ByteString`
- INLINABLE pragmas on `refine_` `reifyPredicate`
- `NFData` instance for `Refined`
- RefineSomeException constructor. Enables recovering
  specific validation exceptions.
  Thanks to @haroldcarr for adding this.
- RefineXorException constructor.
- `Empty` and `NotEmpty` predicates.
- `NaN` and `Infinite` predicates for floating-point numbers.
- @since pragmas to EVERYTHING.

### Changed
- lower bound on mtl to 2.2.2 due to use of liftEither.
  Thanks to @k0ral for reporting this
- Generalize sized predicates
- Allow newer template-haskell (< 0.16 ==> < 0.17)
- Allow newer aeson (< 1.5 ==> < 1.6)
  Thanks to @locallycompact for this change.

### Removed
- `Refined.Internal` module
  Thanks to @nikita-volkov for pushing me to do this.
- Orphan modules
  Thanks to @symbiont-sam-halliday for pointing out the
  silliness of these modules.
- `RefineT`. It was a needless abstraction that just made the
  library harder to learn and use, providing little benefit
  over `Maybe RefineException`.
  Thanks to @nikita-volkov for helping me see the light.

### Deprecated
- Refined.These module in favour of Data.These from these-skinny

## [0.4.4] - 2019-10-18
### Added
- `refine_`
- `refineTH_`
- test suite: unit tests for compiling.
### Changed
- Allow newer `template-haskell` and `QuickCheck`.

## [0.4.2.3] - 2019-09-17
### Added
- `reifyPredicate`
### Fixed
- `Arbitrary` instance for `Refined` should now always terminate.
   Thanks to @symbiont-joseph-kachmar for reporting this.

## [0.4.2.2] - 2019-07-19
### Added
- `exceptRefine`, `strengthen`, and `strengthenM`

## [0.4.2.1] - 2019-05-31
### Fixed
- Documentation fix for `DivisibleBy`
### Changed
- Re-export `DivisibleBy`, `Even`, and `Odd` from module `Refined`.
- Re-export all constructors from module `Refined`.

## [0.4.2] - 2019-05-30
### Removed
- Re-removed dependency of `these` package.
### Added
- `Even`, `Odd`, and `DivisibleBy` predicates.
- doctests for all predicates.
### Changed
- Make all predicates unary data constructors, instead of nullary,
  and export those newly added constructors.

## [0.4.1] - 2019-04-15
### Fixed
- Serious regression where `Not p` ~ `p`. Thanks to @k0ral who reported this.

## [0.4] - 2019-03-18
### Added
- 'NegativeFromTo', a Predicate that ensures a numeric value is within a range [a,b],
  where a < 0 and b >= a.
  Thanks to github.com/futtetennista for this change.
### Changed
- `RefinedNotException` now has a child (it should have had one in v2. This was an oversight.)
- `displayRefineException` no longer uses tabs, instead 2 spaces.
- make implementation of `displayRefineException` more clear via formatting.

## [0.3.0.0] - 2018-09-26
### Added
- Internal module, and Unsafe modules, making sure to take care w.r.t.
  the scope of coercions related to the use of the 'Refined' constructor.
- 'IdPred' predicate, predicate that never fails.
- Generic instances for all predicates.
- `reallyUnsafeRefine`, `reallyUnsafeUnderlyingRefined`, `reallyUnsafeAllUnderlyingRefined`,
  functions that allow one to use 'Coercion's to manually prove things about 'Refined'
  values.
### Changed
- Type role of Refined from 'phantom Representational' to 'nominal nominal'.
  With the old type role, one can use `coerce` to prove `Q x` given any `P x`.
  The second parameter should also be nominal because of interactions with something
  like `GreaterThan` and `Data.Ord.Down`.
  Thanks to David Feuer for pointing this out.
- Change docs to point users to 'Refined.Unsafe' module instead of recommending
  'Unsafe.Coerce.unsafeCoerce'.
- 'Ascending' and 'Descending' predicates now use 'Foldable' instead of 'IsList'.
- Lowered the lower bound on 'exceptions'; it was too strict for the support window.
### Removed
- Dependency of the 'these' package. It brings in some very
  heavy transitive dependencies, even though the datatype
  in `refined` is used to the most minimal extent.
  This is a breaking change because
  this change is exposed to the end user via 'RefineAndException'.
  It is exported from a module called 'Refined.These'. Users
  wishing to interact with such exceptions can either just
  use the datatype constituting a minimal API there, or depend
  on the 'these' package.

## [0.2.3.0] - 2018-06-01
### Added
- back in the 'Foldable' instance for 'Refined'. It is safe.

## [0.2.2.0] - 2018-05-31
### Removed
- Unsafe typeclass instances that could break the 'Refined' invariant.
  These should not have been added.

## [0.2.1.0] - 2018-05-31
### Removed
- Unsafe typeclass instances that could break the 'Refined' invariant.
  These should not have been added.

## [0.2.0.0] - 2018-05-30
### Changed
- Radical rewrite of the library, centred around 'RefineException'
  and the 'RefineT' monad transformer.
  'validate' now has the type signature
  validate :: (Predicate p x, Monad m) => p -> x -> RefineT m ()
### Added
- More predicates
