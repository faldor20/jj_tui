# {
#   inputs = {
#     nixpkgs.url = "github:nix-ocaml/nix-overlays";
#     flakelight.url = "github:nix-community/flakelight";
#   };

#   outputs =
#     { flakelight, ... }@inputs:
#     flakelight ./. {
#       inherit inputs;

    
#       # default devshell
#       devShell= pkgs:
#       let 
#         ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_3;
#       in
#       {
#         packages = with pkgs; [
#           # ocaml53.dune_3
#           # ocaml53.utop
#           ocamlPackages.ocaml-lsp
#           # ocaml53.odoc
#           # ocaml53.ocamlformat-rpc-lib
#         ];
#         inputsFrom = [
#           # pkgs.lwd
#         ];
#       };

#       # # Define the main package
#       # package=
         
#       #   {
#       #     # ocamlPackages,
#       #     # lib,
#       #     defaultMeta,
#       #     pkgs,
#       #   }:
#       #   pkgs.ocamlPackages.buildDunePackage {
#       #     pname = "lwd";
#       #     version = "0.1.0";
#       #     src = ./.;

#       #     # nativeBuildInputs = with pkgs; with ocamlPackages; [
#       #     #       dune
#       #     #       utop
#       #     #       ocaml
#       #     #       ocamlformat
#       #     #       re
#       #     #       iter
#       #     #       base
#       #     #       angstrom
#       #     #       ppx_let
#       #     #       notty
#       #     #       ppx_inline_test
#       #     #       ppx_assert
#       #     #       seq
#       #     #       picos
#       #     #       picos_std
#       #     # ];
#       #     meta = defaultMeta // {
#       #       description = "Lightweight reactive documents";
#       #     };
        
#       # };
#     };
# }

{
  description = "Example JavaScript development environment for Zero to Nix";

  # Flake inputs
  inputs = {
    nixpkgs.url = "github:nix-ocaml/nix-overlays";
  };
  # Flake outputs
    outputs = { self, nixpkgs, flake-parts,  ... }@inputs:

    flake-parts.lib.mkFlake { inherit inputs; } {
      systems =
        [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
      perSystem = { config, self', inputs', pkgs, system, ... }:
        let
          # OCaml packages available on nixpkgs
          ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_2;
          inherit (pkgs) mkShell lib;
        in {
          _module.args.pkgs = import inputs.nixpkgs {
            inherit system;
            config.allowUnfree = true;
          };

          devShells = {
            default = mkShell.override { stdenv = pkgs.gccStdenv; } {
              buildInputs = with ocamlPackages; [
                dune
                utop
                ocaml
                ocamlformat
                re
                iter
                base
                angstrom
                ppx_let
                notty
                ppx_inline_test
                ppx_assert
                seq
                picos
                picos_std
              ];
              inputsFrom = [ 
             # self'.packages.default
            # ocamlPackages.bonsai
               ];
              packages = builtins.attrValues {
                inherit (pkgs) gcc pkg-config;
                inherit (ocamlPackages) ocaml-lsp ocamlformat-rpc-lib;
              };
            };
          };

        };
    };
}
