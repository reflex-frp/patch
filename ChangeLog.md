# Revision history for patch

## 0.1.0.0

* Rewrite `PatchMapWithMove` in terms of `PatchMapWithPatchingMove`.
  Care is taken to make this as little of a breaking change as possible.
  In particular, `PatchMapWithMove` is a newtype of `PatchMapWithPatchingMove` as is the `NodeInfo` of `PatchMapWithPatchingMove`'s `NodeInfo`.

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
