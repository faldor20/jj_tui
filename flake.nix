{
  description = "Example JavaScript development environment for Zero to Nix";

  # Flake inputs
  inputs = {

  nixpkgs.url = "github:nixos/nixpkgs";

    ocaml-overlay = {
      url = "github:nix-ocaml/nix-overlays";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  # Flake outputs
  outputs =
    {
      self,
      nixpkgs,
      flake-parts,
      ocaml-overlay,
      ...
    }@inputs:

    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];
      perSystem =
        {
          config,
          self',
          inputs',
          pkgs,
          system,
          ...
        }:
        let
          jj_tui =
            pkgs: ocamlPackages: profile:
            let
              notty-mine = ocamlPackages.notty.overrideAttrs (old: {
                src = ./forks/notty/.;
              });

              ppx_record_updater_src = pkgs.fetchFromGitHub {
                owner = "faldor20";
                repo = "ppx_record_updater";
                rev = "15a6ac0fa1a98e21e2b4b68b2eaee088186d5515";
                sha256 = "sha256-e1BD3+F+jutQFbjISKcCNJzkzxA+x8vuCphxT/CuZcA=";
              };

              ppx_record_updater = ocamlPackages.buildDunePackage {
                pname = "ppx_record_updater";
                version = "0.1.0";
                duneVersion = "3";
                src = ppx_record_updater_src;
                buildInputs = with ocamlPackages; [
                  ppxlib
                  ppx_deriving
                ];
                strictDeps = true;
              };

              picos_src = pkgs.fetchFromGitHub {
                owner = "ocaml-multicore";
                repo = "picos";
                rev = "1ba3357ec7ca56b9bbeb251904636e3453ff2c32";
                sha256 = "sha256-I0PFesFeZ+7H0uXDoC4LNltef5buuKeO2THyq0VBg/A=";
              };
              picos = ocamlPackages.buildDunePackage {
                pname = "picos";
                version = "0.6.0";
                duneVersion = "3";
                src = picos_src;
                buildInputs = with ocamlPackages; [
                  backoff
                  thread-local-storage
                ];
                propagatedBuildInputs = with ocamlPackages; [
                  backoff
                  thread-local-storage
                ];
                strictDeps = true;
              };
              picos_std = ocamlPackages.buildDunePackage {
                pname = "picos_std";
                version = "0.6.0";
                duneVersion = "3";
                src = picos_src;
                buildInputs = with ocamlPackages; [
                  picos
                  picos_aux
                  backoff
                  thread-local-storage
                  multicore-magic
                ];
                propagatedBuildInputs = with ocamlPackages; [
                  backoff
                  thread-local-storage
                  multicore-magic
                  picos_aux
                ];
                strictDeps = true;
              };
              picos_aux = ocamlPackages.buildDunePackage {
                pname = "picos_aux";
                version = "0.6.0";
                duneVersion = "3";
                src = picos_src;
                buildInputs = with ocamlPackages; [
                  backoff
                  multicore-magic
                ];

                strictDeps = true;
              };
              picos_mux = ocamlPackages.buildDunePackage {
                pname = "picos_mux";
                version = "0.6.0";
                duneVersion = "3";
                src = picos_src;
                buildInputs = with ocamlPackages; [
                  picos
                  picos_aux
                  picos_std
                  backoff
                  multicore-magic
                  thread-local-storage
                  psq
                ];
                propagatedBuildInputs = with ocamlPackages; [
                  psq
                ];
                strictDeps = true;
              };
              picos_io = ocamlPackages.buildDunePackage {
                pname = "picos_io";
                version = "0.6.0";
                duneVersion = "3";
                src = picos_src;
                buildInputs = with ocamlPackages; [
                  picos
                  picos_aux
                  picos_std
                  picos_mux

                  backoff
                  mtime
                  multicore-magic
                  psq
                  thread-local-storage
                ];
                propagatedBuildInputs = with ocamlPackages; [
                  psq
                  mtime
                ];

                strictDeps = true;
              };
              picos_mux_with_io = ocamlPackages.buildDunePackage {
                pname = "picos_mux";
                version = "0.6.0";
                duneVersion = "3";
                src = picos_src;
                buildInputs =
                  with ocamlPackages;
                  picos_io.buildInputs
                  ++ [
                    picos
                    picos_aux
                    picos_std
                    picos_io
                    backoff
                    multicore-magic
                    thread-local-storage
                  ];
                  propagatedBuildInputs = with ocamlPackages; [
                    backoff
                    multicore-magic
                    thread-local-storage
                  ];

                strictDeps = true;
              };

              signal = ocamlPackages.buildDunePackage {
                pname = "signal";
                version = "0.4.2";
                duneVersion = "3";
                src = pkgs.fetchFromGitHub {
                  owner = "rizo";
                  repo = "signal";
                  rev = "704fefe7cd7b59e234a51bb470c7a3254468b5a8";
                  sha256 = "sha256-AcphzD/4rrWnsVB5ebXzdthQ1Rrw3xXkv4n5ZHosoz0=";
                };
                buildInputs = with ocamlPackages; [

                ];
                strictDeps = true;
              };
              lwd = ocamlPackages.buildDunePackage {
                pname = "lwd";
                version = "0.1.0";
                duneVersion = "3";

                src = ./forks/lwd/.;

                # pkgs.fetchFromGitHub {

                #   owner = "faldor20";
                #   repo = "lwd";
                #   rev = "c19bc2fd55c2de977cdd283458ce06402b08febe";
                #   sha256 = "sha256-8QwDzRgffA4wnE9vWLpLfy9MdQ5Yc8wBF5jgRamGMfA=";
                # };

                buildInputs = with ocamlPackages; [ seq logs ];
                propagatedBuildInputs = with ocamlPackages; [ logs ];
            
                strictDeps = false;
              };
              lwd_picos = ocamlPackages.buildDunePackage {
                pname = "lwd_picos";
                version = "0.1.0";
                duneVersion = "3";

                src = ./forks/lwd/.;

                buildInputs = with ocamlPackages; [ seq lwd picos picos_std backoff multicore-magic thread-local-storage ];
                propagatedBuildInputs = with ocamlPackages; [
                  
                  lwd
                ];

                strictDeps = true;
              };
              nottui =
                let
                  pname = "nottui";
                in
                ocamlPackages.buildDunePackage {
                  pname = "nottui";
                  version = "dev";
                  duneVersion = "3";
                  src = ./forks/nottui/.;
                  buildInputs = with ocamlPackages; [
                    logs
                    signal
                    lwd
                    notty-mine
                    seq
                  ];
                  buildPhase = ''
                    runHook preBuild
                    rm -rf ./tutorial
                    dune build -p ${pname}  ''${enableParallelBuilding:+-j $NIX_BUILD_CORES}
                    runHook postBuild
                  '';
                  checkPhase = ''
                    runHook preCheck
                    dune runtest -p ${pname}''${enableParallelBuilding:+-j $NIX_BUILD_CORES}
                    runHook postCheck
                  '';
                  installPhase = ''
                    runHook preInstall
                    dune install --prefix $out --libdir $OCAMLFIND_DESTDIR ${pname}
                    runHook postInstall
                  '';

                  strictDeps = true;
                };
                 nottui_picos=
                let
                    pname = "nottui_picos";
                  in
                 ocamlPackages.buildDunePackage {
                    pname = "nottui_picos";
                  version = "dev";
                  duneVersion = "3";
                  src = ./forks/nottui/.;
                  buildInputs = with ocamlPackages; [
                    logs
                    signal
                    # lwd
                    lwd_picos
                    nottui
                  picos
                  picos_io
                  picos_std
                    notty-mine
                    seq
                  ];
                  buildPhase = ''
                    runHook preBuild
                    rm -rf ./tutorial
                    dune build -p ${pname}  ''${enableParallelBuilding:+-j $NIX_BUILD_CORES}
                    runHook postBuild
                  '';
                  checkPhase = ''
                    runHook preCheck
                    dune runtest -p ${pname}''${enableParallelBuilding:+-j $NIX_BUILD_CORES}
                    runHook postCheck
                  '';
                  installPhase = ''
                    runHook preInstall
                    dune install --prefix $out --libdir $OCAMLFIND_DESTDIR ${pname}
                    runHook postInstall
                  '';

                  strictDeps = true;
                };

              jj_tui_build_pkgs =

                picos.buildInputs
                ++ picos_std.buildInputs
                ++ picos_aux.buildInputs
                ++ [
                  lwd
                  lwd_picos
                  notty-mine
                  nottui
                  nottui_picos
                  picos
                  picos_std
                  picos_io
                  picos_mux_with_io

                  picos_aux

                  signal

                  ppx_record_updater
                  ocamlPackages.logs
                  ocamlPackages.logs-ppx

                  ocamlPackages.ppx_deriving

                  ocamlPackages.mtime
                  ocamlPackages.multicore-magic
                  ocamlPackages.psq
                  ocamlPackages.thread-local-storage

                  ocamlPackages.spawn
                  ocamlPackages.parsexp
                  ocamlPackages.stdio
                  ocamlPackages.base
                  ocamlPackages.angstrom
                  ocamlPackages.ppx_expect
                  ocamlPackages.uutf

                  ocamlPackages.re

                  ocamlPackages.yojson
                  ocamlPackages.ppx_deriving_yojson
                  ocamlPackages.yaml
                  ocamlPackages.ppx_deriving_yaml

                  # ocamlPackages.parsexp

                  # Ocaml package dependencies needed to build go here.
                ];
              jj_tui =
                let
                  pname = "jj_tui";
                in
                ocamlPackages.buildDunePackage {
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
                  strictDeps = true;

                };
            in
            {
              jj_tui = jj_tui;
              jj_tui_build_pkgs = jj_tui_build_pkgs;
            };
          # OCaml packages available on nixpkgs
          ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_3;
          inherit (pkgs) mkShell lib;

        in
        {
          _module.args.pkgs = import inputs.nixpkgs {
            inherit system;
            config.allowUnfree = true;
            overlays = [ ocaml-overlay.overlays.default ];
          };
          packages = {
            default = (jj_tui pkgs ocamlPackages "release").jj_tui; # Development environment output
            static =
              (jj_tui pkgs.pkgsCross.musl64 pkgs.pkgsCross.musl64.ocaml-ng.ocamlPackages_5_1 "static").jj_tui; # Development environment output
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
                inherit (ocamlPackages) ocaml-lsp ocamlformat-rpc-lib earlybird;
              };
            };
          };

        };
    };
}
