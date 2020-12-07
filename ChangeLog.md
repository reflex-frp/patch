# Revision history for patch

## Unreleased

* Remove the `split-these` flag.
  We do not need it as we only use the `These` datatype which is provided in all versions.

* Stop defining `Group`; `Group` from the `groups` package can be used instead.

  Most of the instances are provided by `groups`, except the `Group
  MonoidalMap` instance, which is not lawful.  `reflex` might provide it as an
  orphan for backwards compat, temporarily, but it should eventually be removed
  everywhere.

* `Applicative` is still defined, because the `Abelian` from `groups` has too
  stringent a constraint.

## 0.0.3.2

* Update version bounds

## 0.0.3.1

* Replace `fromJust` with something easier to debug.

## 0.0.3.0

* Create `PatchMapWithPatchingMove` variant which supports moves with a patch.

* Create `DecidablyEmpty` subclass of `Monoid`.

## 0.0.2.0

* Consistently provide:

   - `Wrapped` instances

   - `*WithIndex` instances

   - `un*` newtype unwrappers

  for `PatchMap`, `PatchIntMap`, and `PatchMapWithMove`.

## 0.0.1.0

* Support older GHCs with `split-these` flag.

* Additional instances for the `Group` class for basic types.

## 0.0.0.1

* Remove unneeded dependencies

## 0.0.0.0

* Extract patching functionality from Reflex.
