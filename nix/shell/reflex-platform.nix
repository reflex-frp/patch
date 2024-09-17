let
  rp = import ../deps/reflex-platform { __useNewerCompiler = true; };
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
      (import ../release/reflex-platform.nix {}).${system}.ghc.env
    ];
  }
