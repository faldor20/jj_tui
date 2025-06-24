{
  description = "Notty Nix Flake";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:nixos/nixpkgs";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages."${system}";
        inherit (pkgs.ocamlPackages) buildDunePackage;
      in
      rec {
        packages = rec {
          default = notty;
          notty = buildDunePackage {
            pname = "notty";
            version = "n/a";
            src = ./.;
            duneVersion = "3";
            nativeBuildInputs = with pkgs.ocamlPackages; [ cppo ];
            propagatedBuildInputs = with pkgs.ocamlPackages; [ uutf ];
            doCheck = true;
          };
        };
        devShells.default = pkgs.mkShell {
          inputsFrom = pkgs.lib.attrValues packages;
          buildInputs = with pkgs.ocamlPackages; [ ocaml-lsp ];
        };
      });
}
