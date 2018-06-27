# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [0.3.0.0] - TBA
### Added
- Internal module, and Unsafe modules, making sure to take care w.r.t.
  the scope of coercions related to the use of the 'Refined' constructor.
- 'IdPred' predicate, predicate that never fails.
- Generic instances for all predicates.
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
