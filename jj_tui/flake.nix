{
  description = "Example JavaScript development environment for Zero to Nix";

  # Flake inputs
  inputs = {

    # nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs.url = "github:nixos/nixpkgs"; # also valid: "nixpkgs"

    # roc={
    #   url="github:roc-lang/roc";
    # inputs.nixpkgs.follows="nixpkgs";

    # };

  };
  # Flake outputs
  outputs = { self, nixpkgs, ... }@inputs:
    let
      # Systems supported
      allSystems = [
        "x86_64-linux" # 64-bit Intel/AMD Linux
      ];

      # Helper to provide system-specific attributes
      forAllSystems = f:
        nixpkgs.lib.genAttrs allSystems (system:
          f {
            pkgs = import nixpkgs { inherit system; };

          });
    in {
      # Development environment output
      devShells = forAllSystems ({ pkgs }: {
        default =

          pkgs.mkShell {
            packages = with pkgs; [ pkgs.pkg-config gmp stdenv.cc.cc.lib jujutsu ];
            shellHook = let
              libPath =
                pkgs.lib.makeLibraryPath [ pkgs.stdenv.cc.cc.lib pkgs.gmp ];
            in ''
              # yolo
              export CFLAGS="$CFLAGS -I${pkgs.stdenv.cc.cc.lib}/include -I${pkgs.gmp}/include"
              export LIBS="$LIBS -L${pkgs.stdenv.cc.cc.lib}/lib -L${pkgs.gmp}/lib"
            '';
          };

      });

    };
}
