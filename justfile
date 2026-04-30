#by default using the nix shell we should not use dune pkg management
build:
  dune build --pkg disabled
