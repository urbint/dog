{ mkDerivation, alex, array, attoparsec, base, bytestring
, containers, criterion, deepseq, happy, hashable, hspec, mtl
, prettyprinter, QuickCheck, quickcheck-instances, stdenv
, string-conversions, template-haskell, text
}:
mkDerivation {
  pname = "dog";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array attoparsec base bytestring containers deepseq hashable mtl
    prettyprinter template-haskell text
  ];
  libraryToolDepends = [ alex happy ];
  executableHaskellDepends = [
    base bytestring string-conversions text
  ];
  testHaskellDepends = [
    base bytestring hspec QuickCheck quickcheck-instances
    string-conversions text
  ];
  benchmarkHaskellDepends = [ base bytestring criterion text ];
  homepage = "https://github.com/urbint/dog";
  description = "Lexer, Parser, QQ, Pretty-printer, interpreter for Datalog";
  license = stdenv.lib.licenses.bsd3;
}
