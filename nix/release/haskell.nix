{ haskellNix ? null }:

let ci = import ../deps/nix-haskell-ci (if haskellNix != null then { inherit haskellNix; } else {});
    project = import ../project/haskell.nix {};
in with ci.haskell-nix; buildMatrix { inherit project; targets = matrix.default; }
