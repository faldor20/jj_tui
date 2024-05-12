{
  description = "Example JavaScript development environment for Zero to Nix";

  # Flake inputs
  inputs = {

    nixpkgs.url = "nixpkgs-unstable"; # also valid: "nixpkgs"

  };
  # Flake outputs
  outputs = { self, nixpkgs, flake-parts, ... }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems =
        [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
      perSystem = { config, self', inputs', pkgs, system, ... }:
        let
          # OCaml packages available on nixpkgs
          inherit (pkgs) ocamlPackages mkShell lib;
          inherit (ocamlPackages) buildDunePackage;
          ocamlPkgssStatic =
            pkgs.pkgsMusl.ocamlPackages; # Legacy packages that have not been converted to flakes
          eio-process = buildDunePackage {
            pname = "eio-process";
            version = "0.1.0";
            duneVersion = "3";
            src = pkgs.fetchFromGitHub {

              owner = "mbarbin";
              repo = "eio-process";
              rev = "482ba341884dc8711f93ec9cc6d7c941099e0faa";
              sha256 = "sha256-/Y2U+1y+nDMBrRfDAYif0WJp0vPWmvbSMt39wAB/rS8=";
            };

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
          };
          jj_tui_build_pkgs =

            [
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
          jj_tui = pkgs: ocamlPackages:
            ocamlPackages.buildDunePackage {
              pname = "jj_tui";
              version = "0.1.0";
              duneVersion = "3";
              src = ./.;

              buildInputs = jj_tui_build_pkgs;
              env = {
                DUNE_PROFILE = "static";

              };

              strictDeps = true;

              preBuild = ''
                export DUNE_PROFILE=static
              '';
            };

        in {
          packages = {
            default =
              jj_tui pkgs ocamlPackages; # Development environment output
            pkgsMusl.default = jj_tui pkgs.pkgsMusl
              pkgs.pkgsMusl.ocamlPackages; # Development environment output
          };
          devShells = {
            default = mkShell.override { stdenv = pkgs.gccStdenv; } {
              buildInputs = with ocamlPackages; [
                dune_3
                ocaml
                utop
                ocamlformat
              ];
              inputsFrom = [ self'.packages.default ];
              packages = builtins.attrValues {
                inherit (pkgs) gcc pkg-config;
                inherit (ocamlPackages) ocaml-lsp ocamlformat-rpc-lib;
              };
            };
          };

        };
    };
}
