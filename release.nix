{ reflex-platform-fun ? import ./dep/reflex-platform
}:

let
  native-reflex-platform = reflex-platform-fun {};
  inherit (native-reflex-platform.nixpkgs) lib;
  systems = ["x86_64-linux" "x86_64-darwin"];

  perPlatform = lib.genAttrs systems (system: let
    reflex-platform = reflex-platform-fun { inherit system; };
    compilers = [
      "ghc"
      "ghcjs"
    ] ++ lib.optionals (reflex-platform.androidSupport) [
      "ghcAndroidAarch64"
      "ghcAndroidAarch32"
    ] ++ lib.optionals (reflex-platform.iosSupport) [
      "ghcIosAarch64"
    ];
    compilerPkgs = lib.genAttrs compilers (ghc: let
      reflex-platform = reflex-platform-fun {
        inherit system;
        haskellOverlays = [
          # Use this package's source for reflex
          (self: super: {
            _dep = super._dep // {
              patch = builtins.filterSource (path: type: !(builtins.elem (baseNameOf path) [
                "release.nix"
                ".git"
                "dist"
                "dist-newstyle"
                "cabal.haskell-ci"
                "cabal.project"
                ".travis.yml"
              ])) ./.;
            };
          })
        ];
      };
    in reflex-platform.${ghc}.patch);
  in compilerPkgs // {
    cache = reflex-platform.pinBuildInputs "patch-${system}"
      (builtins.attrValues compilerPkgs);
  });

  metaCache = native-reflex-platform.pinBuildInputs "patch-everywhere"
    (map (a: a.cache) (builtins.attrValues perPlatform));

in perPlatform // { inherit metaCache; }
