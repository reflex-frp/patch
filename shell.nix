{ pkgs ? import <nixpkgs> {} }:

let reflex-platform-shell = import ./nix/shell/reflex-platform.nix;
    haskell-nix-shell = import ./nix/shell/haskell.nix {};

in {
  inherit reflex-platform-shell;
  inherit haskell-nix-shell;
}
