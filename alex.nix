{ mkDerivation, array, base, containers, directory, fetchgit, fetchpatch, happy
, perl, process, QuickCheck, alex, stdenv
}:

mkDerivation {
  pname = "alex";
  version = "3.1.4";
  src = fetchgit {
    url = "https://github.com/simonmar/alex";
    sha256 = "b55646863e4bafe1b04b756821ca5da6e88d398bd2801194d2c71c3a8b921800";
    rev = "335ac13c0c2d4ace03cee84a77262236e6db2108";
  };
  patches = [ (fetchpatch {
                url = "https://github.com/simonmar/alex/pull/65.patch";
                sha256 = "17aiq39riikd0wrl8zv01wmvnxhzbibh01y8q3540w0an8kw8cwk";
              })
            ];
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ array base containers directory QuickCheck ];
  testDepends = [ base process ];
  buildTools = [ happy perl alex ];
  homepage = "http://www.haskell.org/alex/";
  description = "Alex is a tool for generating lexical analysers in Haskell";
  license = stdenv.lib.licenses.bsd3;
}
