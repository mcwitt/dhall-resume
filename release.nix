{ compiler ? "ghc865", withHoogle ? false }:

let
  bootstrap = import <nixpkgs> { };

  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    inherit (nixpkgs) rev sha256;
  };

  pkgs = import src { };

  haskellPackages = pkgs.haskell.packages."${compiler}";

  hoogleAugmentedPackages = (if withHoogle then
    haskellPackages.override (old: {
      overrides = bootstrap.lib.composeExtensions (old.overrides or (_: _: { }))
        (self: super: {
          ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
          ghcWithPackages = self.ghc.withPackages;
        });
    })
  else
    haskellPackages);

in hoogleAugmentedPackages.callPackage ./dhall-resume.nix { }
