{
  inputs = {
    opam-repository = {
      url = "github:ocaml/opam-repository";
      flake = false;
    };
    nixpkgs-unstable.url = "nixpkgs-unstable";
    opam-nix = {
      url = "github:tweag/opam-nix";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
      inputs.opam-repository.follows = "opam-repository";

    };
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "nixpkgs-unstable";
  };
  outputs =
    { self, flake-utils, opam-nix, nixpkgs, opam-repository, ... }@inputs:
    let package = "jj_tui";

    in flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        # We put the entire build into a function so we can build with different dune profiles for static and non-static builds 
        build = profile:

          let
            on = opam-nix.lib.${system};
            devPackagesQuery = {
              # You can add "development" packages here. They will get added to the devShell automatically.
              # ocaml-lsp-server = "*";
              # ocamlformat = "*";
            };
            query = devPackagesQuery // {
              ## You can force versions of certain packages here, e.g:
              ## - force the ocaml compiler to be taken from opam-repository:
              # ocaml-base-compiler = "5.1.1";
              ## - or force the compiler to be taken from nixpkgs and be a certain version:
              ocaml-system = "5.1.1";

              # uutf="*";

              ## - or force ocamlfind to be a certain version:
              # ocamlfind = "1.9.2";
            };
            scope = on.buildDuneProject {
              pkgs = if profile == "static" then pkgs.pkgsStatic else pkgs;
            } package ./. query;

            overlay = final: prev: {
              # You can add overrides here
              ${package} = prev.${package}.overrideAttrs (_: {
                env = { DUNE_PROFILE = profile; };

                # Prevent the ocaml dependencies from leaking into dependent environments
                doNixSupport = false;
              });
            };
            scope' = scope.overrideScope' overlay;
            # The main package containing the executable
            main = scope'.${package};
            # Packages from devPackagesQuery
            devPackages = builtins.attrValues
              (pkgs.lib.getAttrs (builtins.attrNames devPackagesQuery) scope');
          in {
            legacyPackages = scope';

            packages.default = main;
            devPackages = devPackages;
          };
      in {

        packages.default = let res = build ""; in res.packages.default;
        # This is our statically built
        packages.static = let res = build "static"; in res.packages.default;

        devShells.default = let res = build "";
        in pkgs.mkShell {
          inputsFrom = [ res.packages.default ];
          buildInputs = res.devPackages ++ [


            # You can add packages from nixpkgs here
          ];
        };
      });
}
