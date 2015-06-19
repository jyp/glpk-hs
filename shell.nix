    let pkgs = (import <nixpkgs> {});
        haskellPackages = pkgs.recurseIntoAttrs(pkgs.haskellPackages.override {
            overrides = self: super:
            let callPackage = self.callPackage; in {
                  algebra = callPackage (import ../algebra/default.nix) {};
                  thisPackage = callPackage (import ./default.nix) {};
            };
           });
    in haskellPackages.thisPackage.env
