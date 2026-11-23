{ config, lib, ... }:

{
  name = "patch";
  src = ./.;

  haskell-nix = {
    options.shell.withHaddock = false;
  };

  nixpkgs = {
    # Where this driver builds against a compiler newer than the bounds its
    # package set was written for, the bounds are the problem. Reading
    # cabal.project through cabal brings `exact-configuration` with it. Cabal
    # is then told every dependency and reads no bound. For a driver with no
    # solver, that amounts to `allow-newer: *:*`.
    options.use-plan =
      lib.versionAtLeast config.nixpkgs.compiler-version "9.14";
  };

  shell = {
    packages = ps: with ps; [ patch ];
    withHoogle = false;
  };

}
