let hsPkgs = import ./default.nix { };
in hsPkgs.shellFor {
  packages = ps: [ ps.dhall-resume ];
  withHoogle = true;
  tools = {
    cabal = "3.2.0.0";
    cabal-bounds = "2.3.0";
    ghcide = "0.2.0";
    hlint = "3.1.6";
    ormolu = "0.1.2.0";
    weeder = "2.1.0";
  };
}
