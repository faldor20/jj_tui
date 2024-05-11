{
  description = "Example JavaScript development environment for Zero to Nix";

  # Flake inputs
  inputs = {

    # nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs.url = "nixpkgs-unstable"; # also valid: "nixpkgs"

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
            packages = with pkgs; [

              opam
              git
              gnumake
              m4
              bubblewrap
              bash
              coreutils
              ((pkgs.openssl.override { static = true; }))
              pkgs.pkg-config
              pkgsMusl.gmp
              stdenv.cc.cc.lib
              jujutsu
              glibc.static
              musl.dev
              musl
              pkgsMusl.gcc
              pkgs.dune_3
              pkgsMusl.ocaml
            ];
            shellHook = 

            let
             libPath = pkgs.lib.makeLibraryPath [
               pkgs.pkgsMusl.stdenv.cc.cc.lib
               pkgs.pkgsMusl.gmp
               pkgs.pkgsMusl.musl
             ];
            in ''
              export CC=${pkgs.pkgsMusl.musl.stdenv.cc}
            #   # yolo
            #   export CFLAGS="$CFLAGS -I${pkgs.pkgsMusl.stdenv.cc.cc.lib}/include -I${pkgs.pkgsMusl.gmp}/include"
            #   export LIBS="$LIBS -L${pkgs.pkgsMusl.stdenv.cc.cc.lib}/lib -L${pkgs.pkgsMusl.gmp}/lib"
            '';
          };

      });

    };
}
