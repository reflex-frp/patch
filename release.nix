# Every combination of driver and compiler patch is meant to work for. Each
# is built two ways: as the drivers build it, and as a person would inside
# the project's shell. `checks` is the style checks.
#
#   nix-build release.nix -A all
#   nix-build release.nix -A build
#   nix-build release.nix -A build.haskell-nix.ghc914
#   nix-build release.nix -A shell-build.nixpkgs.ghc912
#   nix-build release.nix -A checks
#   nix-instantiate --eval --strict --json release.nix -A attr-paths
#
# The nixpkgs driver has no 9.14 package set worth building against, so it
# builds only 9.12.
{ system ? builtins.currentSystem
, inputs ? import ./inputs.nix { inherit system; }
}:

let nix-haskell = import inputs.nix-haskell { inherit system inputs; };

    variant = modules: nix-haskell ([ (import ./project.nix) ] ++ modules);

    stock = variant [];

    lib = stock.pkgs.lib;

in with (import (inputs.nix-haskell + "/libs/prelude") { inherit lib; });

let
    # The one matrix of driver and compiler series. A cell is a project, the
    # driver that builds it, and the path it takes in every result set.
    cells = [

      { path = [ "haskell-nix" "ghc912" ]; project = seriesProject "9.12" []; driver = "haskell-nix"; }
      { path = [ "haskell-nix" "ghc914" ]; project = seriesProject "9.14" []; driver = "haskell-nix"; }
      { path = [ "nixpkgs" "ghc912" ]; project = seriesProject "9.12" []; driver = "nixpkgs"; }

    ];

    seriesProject = series: modules:
      variant ([ { compiler.name = ghcName series; } ] ++ modules);

    ghcName = series: "ghc${lib.replaceStrings [ "." ] [ "" ] series}";

    # The tree of one result set: each cell's derivations at the cell's
    # path, every level marked for `nix-build`.
    matrix = leaf:
      let subtrees = map (cell: lib.setAttrByPath cell.path (leaf cell)) cells;
      in recurse-for-derivations (lib.foldl lib.recursiveUpdate {} subtrees);

    # Each result set: what a cell builds.
    resultSets = {
      build = built;
      shell-build = shellBuild;
    };

    # patch's library, as one driver builds it.
    built = cell:
      lib.genAttrs libraries (driverLibrary.${cell.driver} cell.project.${cell.driver}.project);

    # haskell.nix gives a package a tree of components, the nixpkgs driver one
    # derivation.
    driverLibrary = {
      haskell-nix = proj: name: proj.hsPkgs.${name}.components.library;
      nixpkgs = proj: name: proj.packages.${name};
    };

    # The build a person would run, in the shell they would run it in. cabal
    # needs no package index: the shell's package database already carries
    # everything the project depends on. A configuration naming no
    # repository keeps cabal from fetching one it cannot reach.
    # One `buildCommand` rather than phases: the nixpkgs driver's shell states
    # its own, which stands in for the whole phase list, so a phase written
    # here would never run in that shell.
    shellBuild = cell:
      cell.project.${cell.driver}.project.shell.overrideAttrs (old: {
        name = "patch-${cell.driver}-shell-build";

        buildCommand = ''
          # What entering the shell runs.
          eval "$shellHook"

          cp -r ${cell.project.config.src-cleaned} source
          chmod -R u+w source
          cd source

          export HOME=$TMPDIR
          export CABAL_CONFIG=$TMPDIR/cabal.config
          echo 'jobs: $ncpus' > $CABAL_CONFIG
          echo 'active-repositories: :none' >> cabal.project.local
          echo 'tests: False' >> cabal.project.local
          echo 'benchmarks: False' >> cabal.project.local

          cabal build --offline lib:patch
          touch $out
        '';
      });

    # The name `nix-build -A` takes for a path through the tree.
    dotted = path: lib.concatStringsSep "." path;

    # The names of every entry of one result set, under the set's name.
    setEntries = name:
      map (cell: dotted ([ name ] ++ cell.path)) cells;

    released = lib.mapAttrs (_: matrix) resultSets // {
      checks = recurse-for-derivations checks;
    };

    checks = {
      fourmolu = style.fourmolu-check;
      hlint = style.hlint-check;
    };

    style = import ./style.nix { inherit system inputs; pkgs = stock.pkgs; };

    libraries = [ "patch" ];

in released // {

  all = stock.pkgs.linkFarm "patch-release"
    (link-farm-entries "" released);

  # Every derivation here, named the way `nix-build -A` takes it, for a CI
  # matrix that builds each on its own runner.
  #
  # Example:
  #
  #   attr-paths
  #   => [ "build.haskell-nix.ghc912"
  #        ...
  #        "shell-build.nixpkgs.ghc912"
  #        "checks.fourmolu"
  #        "checks.hlint" ]
  attr-paths = lib.concatMap setEntries (lib.attrNames resultSets)
    ++ map (name: "checks.${name}") (lib.attrNames checks);

}
