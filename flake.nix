{
  description = "Example JavaScript development environment for Zero to Nix";

  # Flake inputs
  inputs = {

    nixpkgs.url = "nixpkgs-unstable"; # also valid: "nixpkgs"

    ocaml-overlay = {
      url = "github:nix-ocaml/nix-overlays";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  # Flake outputs
  outputs = { self, nixpkgs, flake-parts, ocaml-overlay, ... }@inputs:

    flake-parts.lib.mkFlake { inherit inputs; } {
      systems =
        [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
      perSystem = { config, self', inputs', pkgs, system, ... }:
        let
          jj_tui = pkgs: ocamlPackages: profile:

            let
              notty = ocamlPackages.notty.overrideAttrs
                (old: { src = ./forks/notty/.; });
              eio-process = ocamlPackages.buildDunePackage {
                pname = "eio-process";
                version = "0.1.0";
                duneVersion = "3";
                src = pkgs.fetchFromGitHub {

                  owner = "mbarbin";
                  repo = "eio-process";
                  rev = "482ba341884dc8711f93ec9cc6d7c941099e0faa";
                  sha256 =
                    "sha256-/Y2U+1y+nDMBrRfDAYif0WJp0vPWmvbSMt39wAB/rS8=";
                };

                buildInputs = with ocamlPackages; [
                  notty
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
                  ocamlPackages.base
                  ocamlPackages.angstrom
                  ocamlPackages.ppx_expect
                  ocamlPackages.uutf
                  # ocamlPackages.parsexp

                  # Ocaml package dependencies needed to build go here.
                ];
              jj_tui = let pname = "jj_tui";
              in ocamlPackages.buildDunePackage {
                pname = pname;
                version = "0.1.0";
                duneVersion = "3";
                src = ./.;

                buildInputs = jj_tui_build_pkgs;

                buildPhase = ''
                  runHook preBuild
                  dune build -p ${pname} --profile ${profile}  ''${enableParallelBuilding:+-j $NIX_BUILD_CORES}
                  runHook postBuild
                '';
                checkPhase = ''
                  runHook preCheck
                  dune runtest -p ${pname}--profile ${profile} ''${enableParallelBuilding:+-j $NIX_BUILD_CORES}
                  runHook postCheck
                '';
                installPhase = ''
                  runHook preInstall
                  dune install --profile ${profile} --prefix $out --libdir $OCAMLFIND_DESTDIR ${pname}
                  runHook postInstall
                '';

              };
            in {
              jj_tui = jj_tui;
              jj_tui_build_pkgs = jj_tui_build_pkgs;
            };
          # OCaml packages available on nixpkgs
          ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_1;
          inherit (pkgs) mkShell lib;

        in {
          _module.args.pkgs = import inputs.nixpkgs {
            inherit system;
            config.allowUnfree = true;
            overlays = [ ocaml-overlay.overlays.default ];
          };
          packages = {
            default = (jj_tui pkgs ocamlPackages
              "release").jj_tui; # Development environment output
            static = (jj_tui pkgs.pkgsCross.musl64
              pkgs.pkgsCross.musl64.ocaml-ng.ocamlPackages_5_1
              "static").jj_tui; # Development environment output
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
