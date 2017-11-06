{ mkDerivation, base, binary, containers, stdenv }:
mkDerivation {
  pname = "gasp";
  version = "1.0.1.0";
  sha256 = "1y9kn03q6gvcf9zdi656121brvicm20y4a74g5qma54qb6ccr7bz";
  libraryHaskellDepends = [ base binary containers ];
  description = "A framework of algebraic classes";
  license = stdenv.lib.licenses.bsd3;
}
