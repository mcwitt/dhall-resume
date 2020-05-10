{ mkDerivation, base, data-default, dhall, HaTeX, hspec, lucid
, optparse-applicative, pandoc, raw-strings-qq, stdenv, text
}:
mkDerivation {
  pname = "dhall-resume";
  version = "0.1.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base data-default dhall HaTeX lucid pandoc text
  ];
  executableHaskellDepends = [ base optparse-applicative text ];
  testHaskellDepends = [ base hspec raw-strings-qq text ];
  license = stdenv.lib.licenses.mit;
}
