{}:

let ghcVersion = "ghcHEAD";
    nixpkgs = with (import <nixpkgs> {}); fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs";
      rev = "feb2849fdeb70028c70d73b848214b00d324a497";
      sha256 = "016ihxx048j09mfxxapaab5mx9kasjlvljqfgnz1vslmw8gcs2b6";
    };
    pkgs = import nixpkgs {
      overlays = [

        # "Fix" in place cabal2nix, otherwise interferes with overriding dependencies that cabal2nix depends on
        (final: prev: {
          cabal2nix-unwrapped = haskellLib.justStaticExecutables (prev.haskell.packages.ghc98.generateOptparseApplicativeCompletions [ "cabal2nix" ] prev.haskell.packages.ghc98.cabal2nix);
        })

        # Pick fresher dependencies from hackage
        (final: prev: {
          haskellPackages = prev.haskell.packages."${ghcVersion}".override { overrides = self: super: rec {
            aeson = self.callHackageDirect {
              pkg = "aeson";
              ver = "2.2.3.0";
              sha256 = "sha256-V4cld9jCPuzpMMAOJXeyWyVPwE05zcmZLpMtSc0HKqk=";
            } {};
            boring = self.callHackageDirect {
              pkg = "boring";
              ver = "0.2.2";
              sha256 = "sha256-df5Ovw44J/288HyF6mtxxp6vwdg+9AXsIng+q/boJ64=";
            } {};
            commutative-semigroups = self.callHackageDirect {
              pkg = "commutative-semigroups";
              ver = "0.2.0.1";
              sha256 = "sha256-awswOPDumlMe0AmCb6GgfM2qABp1Y/UxQahMLfj6OEg=";
            } {};
            constraints-extras = self.callHackageDirect {
              pkg = "constraints-extras";
              ver = "0.4.0.1";
              sha256 = "sha256-FV/tNnBR+C8yU9mr8XIEU1v0Wmk+hACOyPsZnHyVi7A=";
            } {};
            dependent-map = self.callCabal2nix "dependent-map" (pkgs.fetchFromGitHub {
              owner = "obsidiansystems";
              repo = "dependent-map";
              rev = "a646cce661eb1bd7a9529e6627719aa2834c5546";
              sha256 = "sha256-fGLgMfk0sfEot4GIVOokcl98LL53pFF5n7nmi+3UU0Q=";
            }) {};
            filepath = self.callHackageDirect {
              pkg = "filepath";
              ver = "1.5.3.0";
              sha256 = "sha256-d2GTFSSCE8UxMk7AllpbCk1POv8q/h+/NnWHpakz/10=";
            } {};
            hashable = self.callHackageDirect {
              pkg = "hashable";
              ver = "1.4.6.0";
              sha256 = "sha256-UK24kyPDWNwkmSJP04DATlXRrfmX+mWBUeGaO4ZYgTM=";
            } {};
            indexed-traversable = self.callHackageDirect {
              pkg = "indexed-traversable";
              ver = "0.1.4";
              sha256 = "sha256-YA5OTfYeet9ZqIY514GlB/sd32lQ7F3mMjqfU9P/PRg=";
            } {};
            indexed-traversable-instances = self.callHackageDirect {
              pkg = "indexed-traversable-instances";
              ver = "0.1.2";
              sha256 = "sha256-GT08HO5oIhnrajrDs8pFWYNzlT18VQ0TcNKD87Wadxc=";
            } {};
            integer-conversion = self.callHackageDirect {
              pkg = "integer-conversion";
              ver = "0.1.1";
              sha256 = "sha256-I/vk2uj0KZSwBoJQq07+SOZVGS+RfxK0wQ4g1oeBLEs=";
            } {};
            lens = haskellLib.doJailbreak (self.callHackageDirect {
              pkg = "lens";
              ver = "5.3.2";
              sha256 = "sha256-tRysYL8blrpkovrIvjA2TxVogZlfn34lR3CUooRKLMA=";
            } {});
            monoidal-containers = self.callHackageDirect {
              pkg = "monoidal-containers";
              ver = "0.6.5.0";
              sha256 = "sha256-PceVoF2CL8M0PA1wMEB7lDCLwfDwXVd1o41ccJnl2Og=";
            } {};
            primitive = self.callCabal2nix "primitive" (pkgs.fetchFromGitHub {
              owner = "haskell";
              repo = "primitive";
              rev = "16421cac9842a9b8cb48eff33380d5ff45a83539";
              sha256 = "sha256-2+YzbbSttschNalMeTWUGaoMzosqYi53ivTpkQw3CFM=";
            }) {};
            semialign = self.callHackageDirect {
              pkg = "semialign";
              ver = "1.3.1";
              sha256 = "sha256-9kidIpfu0JhU82nyT4H3KhsfudO0XdI/Fw0+gshSARY=";
            } {};
            scientific = self.callHackageDirect {
              pkg = "scientific";
              ver = "0.3.8.0";
              sha256 = "sha256-CQtBLy3tupw3YycpAjm0eHB8QOSO4COjoN1j5dlarJQ=";
            } {};
            text-iso8601 = self.callHackageDirect {
              pkg = "text-iso8601";
              ver = "0.1.1";
              sha256 = "sha256-dcjYeLQuAKQPoku8nVi0nKjrMC3x+L7frOqLWx7waFI=";
            } {};
            these = self.callHackageDirect {
              pkg = "these";
              ver = "1.2.1";
              sha256 = "sha256-84h3fyot14FILSQhfArP32qruJ4QakScm5s55iuFDEs=";
            } {};
            time-compat = self.callHackageDirect {
              pkg = "time-compat";
              ver = "1.9.7";
              sha256 = "sha256-XTMHdW7NT3FtKLhuKdQ3uXgHqnXDvM6z3neDwry9Jeo=";
            } {};
            th-abstraction = self.callHackageDirect {
              pkg = "th-abstraction";
              ver = "0.7.0.0";
              sha256 = "sha256-YNCvJ9C8PsOTT+B4NoRT6kA2bOFk4F6ygm8hzWMH+1I=";
            } {};
            uuid-types = self.callHackageDirect {
              pkg = "uuid-types";
              ver = "1.0.6";
              sha256 = "sha256-v4Vo4bnOyKy014+ZUdnSHvqpZTNjwYPB2qlE63GoDMU=";
            } {};
          };};
        })
      ];
    };
    haskellLib = pkgs.haskell.lib;

    pkg = pkgs.pkgsCross.ghcjs.haskellPackages.developPackage {
      root = ./.;
      cabal2nixOptions = "--jailbreak";
    };

in pkg
