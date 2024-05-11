# Jujutsu TUI
A WIP TUI for the new version control system Jujutsu 

Press `h` to show the help.

Please provide any suggestions. I'm new to jujutsu so I'm sure people have workflows I couldn't even dream of.  
# Dev
Can be built with nix `nix build .#jj_tui` or open a nix shell with `nix develop`
For non-nix the project can be buit with dune or opam. 
I've used opam-monorepo as an experiment, that can be built using `opam monorepo pull && dune build jj_tui`
