# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [0.2.4.0] - 2018-06-14
### Changed
- Type role of Refined from 'phantom Representational' to 'nominal nominal'.
  With the old type role, one can use `coerce` to prove `Q x` given any `P x`.
  The second parameter should also be nominal because of interactions with something
  like `GreaterThan` and `Data.Ord.Down`.
  Thanks to David Feuer for pointing this out.
- Change docs to point users to 'Refined.Unsafe' module instead of recommending
  'Unsafe.Coerce.unsafeCoerce'.
### Added
- Internal module, and Unsafe module.

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
