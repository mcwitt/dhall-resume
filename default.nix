{ sources ? import ./nix/sources.nix
, haskellNix ? import sources.haskell-nix { }
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2003
, nixpkgsArgs ? haskellNix.nixpkgsArgs
, pkgs ? import nixpkgsSrc nixpkgsArgs
, compiler ? "ghc883"
}:

pkgs.haskell-nix.cabalProject {
  compiler-nix-name = compiler;
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "dhall-resume";
    src = ./.;
  };
}
