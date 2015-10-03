{ mkDerivation, alex, array, base, bytestring, containers
, data-default-generics, happy, lens, llvm-general
, llvm-general-pure, mtl, stdenv, text, transformers
, wl-pprint-text
}:
mkDerivation {
  pname = "dnohs";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    array base bytestring containers data-default-generics lens
    llvm-general llvm-general-pure mtl text transformers wl-pprint-text
  ];
  executableToolDepends = [ alex happy ];
  license = stdenv.lib.licenses.bsd3;
}
