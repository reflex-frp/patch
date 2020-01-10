## Patch

Infrastructure for writing patches which act on other types.

A `Patch` type represents a kind of change made to a datastructure.

```haskell
class Patch p where
  type PatchTarget p :: *
  -- | Apply the patch p a to the value a.  If no change is needed, return
  -- 'Nothing'.
  apply :: p -> PatchTarget p -> Maybe (PatchTarget p)
```

### Patching Maps
For example, `Data.Patch.Map` defines the `PatchMap` type which can be used to patch `Map`s. A `PatchMap` represents updates to a `Map` that can insert, remove, or replace items in the `Map`. In this example, the `Map` is the `PatchTarget` and the `PatchMap` is the `Patch`. Keep in mind that there are many other possible `Patch`es that can be applied to a `Map` (i.e., `Map` can be the `PatchTarget` for many different `Patch` instances).

`PatchMap` is defined as:

```haskell
newtype PatchMap k v = PatchMap { unPatchMap :: Map k (Maybe v) }
```

The `Maybe` around the value is used to represent insertion/updates or deletion of the element at a given key.

Its `Patch` instance begins with:

```haskell
instance Ord k => Patch (PatchMap k v) where
  type PatchTarget (PatchMap k v) = Map k v
  ...
```

When a `PatchMap` is applied to its `PatchTarget`, the following changes can occur:

- If the key is present in the `Patch` and the `PatchTarget`...

  - And the `Patch` value at that key is `Nothing`: delete that key from the `PatchTarget`.

  - And the `Patch` value at that key is `Just x`: update the value at that key in the `PatchTarget` to `x`.

- If the key is present in the `Patch` and not present in the `PatchTarget`...

  - And the `Patch` value at that key is `Nothing`: do nothing because we're trying to delete a key that doesn't exist in the target in the first place.

  - And the `Patch` value at that key is `Just x`: insert the key and the value `x` into the `PatchTarget`

- If the key is *not* present in the `Patch` but present in the `PatchTarget`: do nothing.

There are, of course, more complicated ways of patching maps involving, for example, moving values from one key to another. You can find the code for that in `Data.Patch.PatchMapWithMove`. Note that the `PatchTarget` type associated with the `PatchMapWithMove` patch instance is still `Map k v`!
