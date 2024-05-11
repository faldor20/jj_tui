{
  description = "Example JavaScript development environment for Zero to Nix";

  # Flake inputs
  inputs = {

    nixpkgs.url = "nixpkgs-unstable"; # also valid: "nixpkgs"

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
          f (let pkgs = import nixpkgs { inherit system; };
          in {
            pkgs = pkgs;
            # OCaml packages available on nixpkgs
            ocamlPackages =
              nixpkgs.legacyPackages.${system}.ocamlPackages; # Legacy packages that have not been converted to flakes
            ocamlPackagesStatic =
              nixpkgs.legacyPackages.${system}.pkgsMusl.ocamlPackages; # Legacy packages that have not been converted to flakes
          }));

    in {
      packages = forAllSystems ({ pkgs, ocamlPackages,ocamlPackagesStatic}:
        let
          eio-process = ocamlPackages.buildDunePackage {
            pname = "eio-process";
            version = "0.1.0";
            duneVersion = "3";
            src = pkgs.fetchFromGitHub {

              owner = "mbarbin";
              repo = "eio-process";
              rev = "482ba341884dc8711f93ec9cc6d7c941099e0faa";
              sha256 = "sha256-/Y2U+1y+nDMBrRfDAYif0WJp0vPWmvbSMt39wAB/rS8=";
            };
            nativeBuildInputs = with pkgs;
              [
                # gmp
                # stdenv.cc.cc.lib
                # dune_3
                # clang
                # ocaml
                # opam

              ];

            buildInputs = with ocamlPackages; [
              base
              eio
              parsexp
              ppx_compare
              ppx_enumerate
              ppx_hash
              ppx_here
              ppx_let
              ppx_sexp_conv
              ppx_sexp_value
            ];

            strictDeps = true;

            preBuild = ''
              #make home which is needed for opam
                # export HOME=$(pwd)/build-home
                # mkdir -p $HOME

                # opam init .

                # opam install . opam-monorepo
                # opam monorepo pull
            '';
          };
          jj_tui= pkgs: ocamlPackages:
ocamlPackages.buildDunePackage {
            pname = "jj_tui";
            version = "0.1.0";
            duneVersion = "3";
            src = ./.;

            buildInputs = [
              eio-process
              ocamlPackages.parsexp
              ocamlPackages.eio_main
              ocamlPackages.stdio
              ocamlPackages.nottui
              ocamlPackages.lwd
              ocamlPackages.base
              ocamlPackages.angstrom
              ocamlPackages.ppx_expect
              # ocamlPackages.parsexp

              # Ocaml package dependencies needed to build go here.
            ];
            env={
            DUNE_PROFILE="static";
              
            };

            strictDeps = true;

            preBuild = ''
              #make home which is needed for opam
                # export HOME=$(pwd)/build-home
                # mkdir -p $HOME

            export DUNE_PROFILE=static
                # opam init .

                # opam install . opam-monorepo
                # opam monorepo pull
            '';
          };

        in {
          default =jj_tui pkgs ocamlPackages;       # Development environment output
          pkgsMusl.default=jj_tui pkgs.pkgsMusl ocamlPackagesStatic;       # Development environment output
        });
      devShells = forAllSystems ({ pkgs, ... }: {
        default =

          pkgs.mkShell {
            packages = with pkgs; [
              gmp
              stdenv.cc.cc.lib
              dune_3
              ocaml
              opam
              fish
            ];
          };

      });

    };
}
