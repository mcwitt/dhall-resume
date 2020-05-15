{ mkDerivation, base, clay, data-default, dhall, HaTeX, hspec
, lucid, mtl, optparse-applicative, pandoc, raw-strings-qq, stdenv
, text
}:
mkDerivation {
  pname = "dhall-resume";
  version = "0.3.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base clay data-default dhall HaTeX lucid mtl pandoc text
  ];
  executableHaskellDepends = [ base optparse-applicative text ];
  testHaskellDepends = [ base hspec raw-strings-qq text ];
  license = stdenv.lib.licenses.mit;
}
