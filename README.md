# Jujutsu TUI
[![nix](https://github.com/faldor20/jj_tui/actions/workflows/build-nix.yml/badge.svg)](https://github.com/faldor20/jj_tui/actions/workflows/build-nix.yml)
A TUI for the new version control system Jujutsu 


![jj_tui-ezgif com-optimize](https://github.com/faldor20/jj_tui/assets/26968035/fb053320-484a-4d6f-9b66-e5b9d0d49e5d)


Press `?` to show the help. (commands are different between graph and files view).
Press `Alt+Up`and`Alt+Down` to navigate windows
List of graph commands:

![jj_tui commands](https://github.com/user-attachments/assets/1e446a3d-1736-4207-b311-29d8e4bdc333)


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
