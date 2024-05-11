{
  description = "Ocaml project using `ocaml-flake` and `flake-parts`";

  inputs = {
    nixpkgs.url = "nixpkgs-unstable";
    ocaml-flake.url = "github:9glenda/ocaml-flake";
  };

  outputs = inputs @ {
    flake-parts,
    ocaml-flake,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      imports = [
        ocaml-flake.flakeModule
      ];
      systems = ["x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin"];
      perSystem = _: {
        ocaml = {
          duneProjects = {
            default = {
              name = "jj_tui";
              src = ./.;
            };
            eio-process = {
              name = "eio-process";
              src = ./forks/.;
            };
            notty = {
              name = "notty";
              src = ./forks/.;
            };
          };
        };
      };
    };
}
