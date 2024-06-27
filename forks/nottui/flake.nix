{
  description = "Nottui nix flake";

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
          # OCaml packages available on nixpkgs
          ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_1;
          packages = {
            lwd = ocamlPackages.lwd.overrideAttrs (old: {
              src = pkgs.fetchFromGitHub {
                owner = "faldor20";
                repo = "lwd";
                rev = "a150ef22ecb60d842e45a377fec233826a06b221";
                sha256 = "sha256-bee+tEmi8RMCT6W7N8qYC1UvDXl8RqAoWze1GBjfVPw=";
              };
            });

            nottui = ocamlPackages.buildDunePackage {
              pname = "nottui";
              version = "0.4.0";
              duneVersion = "3";
              src = ./.;
              buildInputs = with ocamlPackages; [
                packages.lwd
                ppx_inline_test
                ppx_assert
                notty
              ];
              strictDeps = true;
            };

            nottui-lwd = ocamlPackages.buildDunePackage {
              pname = "nottui-lwd";
              version = "0.4.0";
              duneVersion = "3";
              src = ./lib/nottui-lwd/.;
              buildInputs = with ocamlPackages; [ packages.nottui notty ];
              strictDeps = true;
            };

            nottui-pretty = ocamlPackages.buildDunePackage {
              pname = "nottui-pretty";
              version = "0.4.0";
              duneVersion = "3";
              src = ./lib/nottui-pretty/.;
              buildInputs = with ocamlPackages; [ packages.nottui ];
              strictDeps = true;
            };
          };

        in {
          _module.args.pkgs = import inputs.nixpkgs {
            inherit system;
            config.allowUnfree = true;
            overlays = [ ocaml-overlay.overlays.default ];
          };
          packages = packages;
          devShells = {
            default = pkgs.mkShell.override { stdenv = pkgs.gccStdenv; } {
              buildInputs = with ocamlPackages; [
                dune
                utop
                ocaml
                ocamlformat

                #for tangling
                re
                iter
                base
                angstrom
                ppx_let
              ];

              inputsFrom = [ self'.packages.nottui ];
              packages = builtins.attrValues {
                inherit (pkgs) gcc pkg-config;
                inherit (ocamlPackages) ocaml-lsp ocamlformat-rpc-lib;
              };
            };
          };

        };
    };
}
