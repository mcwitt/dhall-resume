{ mkDerivation, base, dhall, HaTeX, hspec, optparse-applicative
, pandoc, raw-strings-qq, stdenv, text
}:
mkDerivation {
  pname = "dhall-resume";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base dhall HaTeX pandoc text ];
  executableHaskellDepends = [ base optparse-applicative text ];
  testHaskellDepends = [ base hspec raw-strings-qq text ];
  license = stdenv.lib.licenses.mit;
}
