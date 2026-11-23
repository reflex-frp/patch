# Builds the wasm target with a GHC bindist from the ghc-wasm-meta pin
# instead of the drivers' own compilers.
#
# Example:
#
#   project.override (import ./wasm-meta.nix { series = "9.12"; })
#   => the project with its wasm target built by the 9.12.4.20260731 bindist
{ series }:

let # The exact bindist version of each series. The drivers name package
    # sets and library directories after the exact version, so the series
    # in the bindist's name is not enough. The pin carries the version in
    # ghcup metadata. That file is yaml, which nix cannot read.
    versions = {
      "9.12" = "9.12.4.20260731";
    };

in { nix-haskell-compilers, ... }: {

  imports = [
    (import "${nix-haskell-compilers}/ghc-wasm-meta" {
      flavour = series;
      version = versions.${series} or null;
    })
  ];

}
