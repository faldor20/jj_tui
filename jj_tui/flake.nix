{
  description = "Example JavaScript development environment for Zero to Nix";

  # Flake inputs
  inputs = {

    nixpkgs.url = "github:nixos/nixpkgs"; # also valid: "nixpkgs"

  };
  # Flake outputs
  outputs = { self, nixpkgs, ... }@inputs:
    let
      # Systems supported
      allSystems =
        [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];

      # Helper to provide system-specific attributes
      forAllSystems = f:
        nixpkgs.lib.genAttrs allSystems (system:
          f {
            # OCaml packages available on nixpkgs
            pkgs = import nixpkgs { inherit system; };
            ocamlPackages =
              nixpkgs.legacyPackages.${system}.ocamlPackages; # Legacy packages that have not been converted to flakes
          });
    in {
      packages = forAllSystems ({ pkgs, ocamlPackages }: {
        default = ocamlPackages.buildDunePackage {
          pname = "hello";
          version = "0.1.0";
          duneVersion = "3";
          src = ./.;

          buildInputs = [

            # Ocaml package dependencies needed to build go here.
          ];

          strictDeps = true;

          preBuild = ''
          opam install . opam-monorepo
          opam monorepo pull
          '';
        };
      });

      # Development environment output
      devShells = forAllSystems ({ pkgs }: {
        default =

          pkgs.mkShell {
            packages = with pkgs; [
              gmp
              stdenv.cc.cc.lib
              dune_3
              clang
              ocaml
              opam
              fish
            ];
            # shellHook = let
            #   libPath =
            #     pkgs.lib.makeLibraryPath [ pkgs.pkgsStatic.stdenv.cc.cc.lib pkgs.pkgsStatic.gmp pkgs.pkgsStatic.musl ];
            # in ''
            #   export CC=${pkgs.pkgsStatic.musl.stdenv.cc}
            #   # yolo
            #   export CFLAGS="$CFLAGS -I${pkgs.pkgsStatic.stdenv.cc.cc.lib}/include -I${pkgs.pkgsStatic.gmp}/include"
            #   export LIBS="$LIBS -L${pkgs.pkgsStatic.stdenv.cc.cc.lib}/lib -L${pkgs.pkgsStatic.gmp}/lib"
            # '';
          };

      });

    };
}
