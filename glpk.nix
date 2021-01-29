{ mkDerivation, array, base, containers, deepseq, gasp, glpk, mtl
, stdenv
}:
mkDerivation {
  pname = "glpk-hs";
  version = "0.7";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ array base containers deepseq gasp mtl ];
  librarySystemDepends = [ glpk ];
  executableHaskellDepends = [
    array base containers deepseq gasp mtl
  ];
  description = "Comprehensive GLPK linear programming bindings";
  license = stdenv.lib.licenses.bsd3;
}
