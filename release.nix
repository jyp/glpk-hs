let
  config = {
    allowUnfree = true;
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          glpk-hs =
            haskellPackagesNew.callPackage ./default.nix { };
          gasp =
            haskellPackagesNew.callPackage ./gasp.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { glpk-hs = pkgs.haskellPackages.glpk-hs;
  }
