{ compiler ? "ghc910" }:

{
  project = {
    src = ../../.;
    compiler-nix-name = compiler;
  };
}
