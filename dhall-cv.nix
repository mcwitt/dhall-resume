{ mkDerivation, base, dhall, HaTeX, hspec, pandoc, raw-strings-qq
, stdenv, text
}:
mkDerivation {
  pname = "dhall-cv";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base dhall HaTeX pandoc text ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec raw-strings-qq text ];
  license = stdenv.lib.licenses.mit;
}
