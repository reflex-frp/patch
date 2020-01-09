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
