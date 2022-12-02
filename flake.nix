{
  description = "Haskell development environment";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable-small";

  outputs =
    { self
    , flake-utils
    , nixpkgs
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        haskellPackageNames = [
          "advent-of-code-twentytwo"
        ];
        ghcVersion = "ghc924";
        haskellMkPackage = hps: nm: hps.callCabal2nix nm (./. + "/${nm}") { };
        haskellOverlay = (
          selfn: supern: {
            haskellPackages = supern.haskell.packages.${ghcVersion}.override {
              overrides = selfh: superh:
                {
                  advent-of-code-twentytwo = selfh.callCabal2nix "advent-of-code-twentytwo" ./. rec { };
                };
            };
          }
        );
        overlays = [ haskellOverlay ];
        pkgs = import nixpkgs {
          inherit system;
          inherit overlays;
        };
        hpkgs = pkgs.haskellPackages;
        hlib = pkgs.haskell.lib;
        advent-of-code-twentytwoPkgs = nixpkgs.lib.genAttrs haskellPackageNames (n: hpkgs.${n});
        advent-of-code-twentytwoPkgsDev = builtins.mapAttrs (_: x: hlib.doBenchmark x) advent-of-code-twentytwoPkgs;
      in
      {
        packages = advent-of-code-twentytwoPkgs // { default = advent-of-code-twentytwoPkgs.advent-of-code-twentytwo; };

        devShells.default = hpkgs.shellFor {
          # shellHook =
          #   let
          #     scripts = ./scripts;
          #   in
          #   ''
          #     export PATH="${scripts}:$PATH"
          #   '';
          packages = _: (builtins.attrValues advent-of-code-twentytwoPkgsDev);
          nativeBuildInputs = with pkgs; [
            # See https://github.com/NixOS/nixpkgs/issues/59209.
            bashInteractive

            # Haskell toolchain.
            hpkgs.cabal-fmt
            hpkgs.cabal-install
            hpkgs.haskell-language-server
          ];
          buildInputs = with pkgs; [
          ];
          doBenchmark = true;
          # withHoogle = true;
        };
      }
    );
}
