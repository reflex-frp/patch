let
  rp = import ./dep/reflex-platform { __useNewerCompiler = true; };
  pkgs = rp.nixpkgs;
  system = builtins.currentSystem;
in
  pkgs.mkShell {
    name = "shell";
    buildInputs = [
      pkgs.cabal-install
      pkgs.ghcid
    ];
    inputsFrom = [
      (import ./release.nix {}).${system}.ghc.env
    ];
  }
