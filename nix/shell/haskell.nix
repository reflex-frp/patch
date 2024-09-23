{ haskellNix ? null }:

let ci = import ../deps/nix-haskell-ci (if haskellNix != null then { inherit haskellNix; } else {});
    haskell = ci.nix-haskell;
    project = import ../project/haskell.nix {};
in haskell.project project
