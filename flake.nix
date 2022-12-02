{
  description = "Haskell development environment";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable-small";

  outputs =
    { self
    , flake-utils
    , nixpkgs
    }:
    let
      theseHpkgNames = [
        "advent-of-code-twentytwo"
      ];
      thisGhcVersion = "ghc924";
      hOverlay = selfn: supern: {
        haskell = supern.haskell // {
          packageOverrides = selfh: superh:
            supern.haskell.packageOverrides selfh superh //
              {
                advent-of-code-twentytwo = selfh.callCabal2nix "advent-of-code-twentytwo" ./. { };
              };
        };
      };
      perSystem = system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ hOverlay ];
          };
          hpkgs = pkgs.haskell.packages.${thisGhcVersion};
          hlib = pkgs.haskell.lib;
          theseHpkgs = nixpkgs.lib.genAttrs theseHpkgNames (n: hpkgs.${n});
          theseHpkgsDev = builtins.mapAttrs (_: x: hlib.doBenchmark x) theseHpkgs;
        in
        {
          packages = theseHpkgs // { default = theseHpkgs.advent-of-code-twentytwo; };

          devShells.default = hpkgs.shellFor {
            # shellHook =
            #   let
            #     scripts = ./scripts;
            #   in
            #   ''
            #     export PATH="${scripts}:$PATH"
            #   '';
            packages = _: (builtins.attrValues theseHpkgsDev);
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
        };
    in
    { overlays.default = hOverlay; } // flake-utils.lib.eachDefaultSystem perSystem;
}
