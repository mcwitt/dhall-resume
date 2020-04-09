{ mkDerivation, base, dhall, HaTeX, stdenv, text }:
mkDerivation {
  pname = "dhall-cv";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base dhall HaTeX text ];
  executableHaskellDepends = [ base ];
  license = stdenv.lib.licenses.mit;
}
