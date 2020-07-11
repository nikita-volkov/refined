# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [0.5] - 2020-XX-XX
### Added
- sized Predicate instances for `Text`
- INLINABLE pragmas on `refine_` `reifyPredicate` 
- `NFData` instance for `Refined`
- RefineSomeException constructor. Enables recovering
  specific validation exceptions.

### Changed
- lower bound on mtl to 2.2.2 due to use of liftEither. 
  Thanks to @k0ral for reporting this
- Generalize sized predicates
- Allow newer template-haskell (< 0.16 ==> < 0.17)

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

## [0.4.1.0] - 2019-04-15
### Fixed
- Serious regression where `Not p` ~ `p`. Thanks to @k0ral who reported this.

## [0.4.0.0] - 2019-03-18
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
