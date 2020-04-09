{ mkDerivation, base, dhall, HaTeX, hspec, raw-strings-qq, stdenv
, text
}:
mkDerivation {
  pname = "dhall-cv";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base dhall HaTeX text ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec raw-strings-qq text ];
  license = stdenv.lib.licenses.mit;
}
