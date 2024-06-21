# Jujutsu TUI
[![nix](https://github.com/faldor20/jj_tui/actions/workflows/build-nix.yml/badge.svg)](https://github.com/faldor20/jj_tui/actions/workflows/build-nix.yml)
A TUI for the new version control system Jujutsu 

![screenshot](./screenshot.jpg)

Press `h` to show the help.
Press `Alt+Up`and`Alt+Down` to navigate windows

Please provide any suggestions. I'm new to jujutsu so I'm sure people have workflows I couldn't even dream of.  
## Installing
If you are on linux, just grab the latest release. It's statically linked and should work on any linux machine.
If you are on mac, also grab a prebuild, let me know if you have any issues as I can't test on a mac.


To open a shell with jj_tui on nix:`nix shell github:faldor20/jj_tui`

To install without nix:
1. Install opam with your package manager
2. clone the repo
3. Run: `opam install .`


## Dependencies
The jujutsu CLI.
I haven't tested on windows or Mac.
I believe it won't work outside Unix so Windows users will currently have to use wsl. 

# Dev
Can be built with nix `nix build` or open a nix shell with `nix develop`
For non-nix the project can be buit with dune or opam. 
