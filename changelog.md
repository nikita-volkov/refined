# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

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
