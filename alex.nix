{ mkDerivation, array, base, containers, directory, fetchgit
, process, QuickCheck, stdenv
}:
mkDerivation {
  pname = "alex";
  version = "3.1.4";
  src = fetchgit {
    url = "git://github.com/simonmar/alex";
    sha256 = "26e8de081aa35482a62043549888d6082ca7c1cfd196a081bf7880809d08614e";
    rev = "447bbb8f53db0201e7ba7fe525f2a3b37c2a5f94";
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    array base containers directory QuickCheck
  ];
  testHaskellDepends = [ base process ];
  homepage = "http://www.haskell.org/alex/";
  description = "Alex is a tool for generating lexical analysers in Haskell";
  license = stdenv.lib.licenses.bsd3;
}
