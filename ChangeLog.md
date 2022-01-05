# Revision history for patch

## 0.0.5.1 - 2021-12-28

* New dep of `base-orphans` for old GHC to get instances honestly instead of
  via `monoidal-containers`.

## 0.0.5.0 - 2021-12-17

* `Additive` now lives in `Data.Semigroup.Additive`, but is still reexported
  from `Data.Patch` for compatability.

* Rewrite `PatchMapWithMove` in terms of `PatchMapWithPatchingMove`.
  Care is taken to make this not a breaking change.
  In particular, `PatchMapWithMove` is a newtype of `PatchMapWithPatchingMove`, as is the `NodeInfo` and `From` of `PatchMapWithPatchingMove`'s versions of those.
  There are complete constructor and field patterns too, and everything is
  exported under the newtype as real constructors and fields would be.

## 0.0.4.0 - 2021-04-20

* Enable PolyKinds

## 0.0.3.2 - 2020-11-06

* Update version bounds

## 0.0.3.1 - 2020-02-05

* Replace `fromJust` with something easier to debug.

## 0.0.3.0 - 2020-02-05

* Create `PatchMapWithPatchingMove` variant which supports moves with a patch.

* Create `DecidablyEmpty` subclass of `Monoid`.

## 0.0.2.0 - 2020-01-17

* Consistently provide:

   - `Wrapped` instances

   - `*WithIndex` instances

   - `un*` newtype unwrappers

  for `PatchMap`, `PatchIntMap`, and `PatchMapWithMove`.

## 0.0.1.0 - 2020-01-09

* Support older GHCs with `split-these` flag.

* Additional instances for the `Group` class for basic types.

## 0.0.0.1 - 2020-01-08

* Remove unneeded dependencies

## 0.0.0.0 - 2020-01-08

* Extract patching functionality from Reflex.
