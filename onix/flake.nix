{
  inputs = {
    nixpkgs.url = "nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    onix = {
      url = "github:odis-labs/onix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, onix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        onix' = (builtins.trace
          "${builtins.toJSON (builtins.attrNames onix.packages)}"
          onix.packages.${system}.latest);

        env = onix'.env {
          # The path where opam files are looked up.
          path = ./.;

          # Optional: dependency variables to be used during lock generation.
          vars = {
            "with-test" = true;
            "with-doc" = true;
            "with-dev-setup" = true;
          };

          # Optional: specify the compiler version for the build environment.
          deps = {
            "ocaml-system" = "5.1.1";
            "notty" = "./forks/notty/notty.opam";
            "eio-process" = "./forks/eio-process/eio-process.opam";
          };
        };
      in {
        packages = {  default = env.pkgs.jj_tui; };
        devShells = {
          default = pkgs.mkShell { nativeBuildInputs = [ pkgs.hello onix' ]; };
        };
      });
}
