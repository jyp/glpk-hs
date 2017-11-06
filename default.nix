{ mkDerivation, array, base, containers, deepseq, gasp, glpk, mtl
, stdenv
}:
mkDerivation {
  pname = "glpk-hs";
  version = "0.5";
  src = ./.;
  libraryHaskellDepends = [ array base containers deepseq gasp mtl ];
  librarySystemDepends = [ glpk ];
  description = "Comprehensive GLPK linear programming bindings";
  license = stdenv.lib.licenses.bsd3;
}
