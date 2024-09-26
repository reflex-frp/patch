{ pkgs ? import <nixpkgs> {} }:

let reflex-platform-release = import ./nix/release/reflex-platform.nix {};
    haskell-nix-release = import ./nix/release/haskell.nix {};

in pkgs.runCommand "release" {} ''
  mkdir -p $out

  ln -s ${reflex-platform-release.metaCache} $out/reflex-platform
  ln -s ${haskell-nix-release} $out/haskell-nix
''
