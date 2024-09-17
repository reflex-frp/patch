{ haskellNix ? ../deps/haskell.nix }:

let haskell = import ../deps/nix-haskell { inherit haskellNix; };
    project = import ../project/haskell.nix {};
in haskell.project project;
