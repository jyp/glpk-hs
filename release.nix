let
  nixpkgs_source = fetchTarball "https://github.com/NixOS/nixpkgs-channels/archive/nixos-18.09.tar.gz"; 
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

  pkgs = import nixpkgs_source { inherit config; };

in
  { glpk-hs = pkgs.haskellPackages.glpk-hs;
  }
