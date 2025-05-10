# Jujutsu TUI
[![nix](https://github.com/faldor20/jj_tui/actions/workflows/build-nix.yml/badge.svg)](https://github.com/faldor20/jj_tui/actions/workflows/build-nix.yml)
A TUI for the new version control system Jujutsu 


![jj_tui-ezgif com-optimize](https://github.com/faldor20/jj_tui/assets/26968035/fb053320-484a-4d6f-9b66-e5b9d0d49e5d)


Press `?` to show the help. (commands are different between graph and files view).
Press `Arrows` to navigate windows, `Enter` to focus status view
List of graph commands:

![jj_tui commands](https://github.com/user-attachments/assets/1e446a3d-1736-4207-b311-29d8e4bdc333)

Please provide any suggestions. I'm new to jujutsu so I'm sure people have workflows I couldn't even dream of.  
## Installing
`linux`: Grab the latest release. It's statically linked and should work on any linux machine.
`mac`: Grab a prebuild. Let me know if you have any issues as I can't test on a mac.

To open a shell with jj_tui on nix:`nix shell github:faldor20/jj_tui``

## Dependencies
The jujutsu CLI (minimum version 0.30.0)
I haven't tested on windows or Mac.
I believe it won't work outside Unix so Windows users will currently have to use wsl. 


# Config file:
You can make a `config.yaml` config file in the following directories to customize key inputs
`linux`: $XDG_CONFIG_HOME/jj_tui/
`macos`: ~/Library/preferences/jj_tui/
see `./jj_tui/lib/key_map.ml` for a spec for the keymap and `./jj_tui/lib/config.ml` for the config
The keymap config lets you fully customize all the commands and their sub mmenus.
``` yaml
key_map:
  graph:
    #Simple mapping from key to command_id
    p: prev
    #Command sub menu
    s:
      title: "Squash"
      sub:
        # sub menu command
        s: "squash_into_parent" 
# If the terminal is smaller than this width, the UI will change to a single pane view
single_pane_width_threshold: 110
```
For a full list of commands ids see [`jj_tui/bin/graph_commands.ml`](jj_tui/bin/graph_commands.ml) and [`jj_tui/bin/file_commands.ml`](jj_tui/bin/file_commands.ml)

# logs: 
`linux`: $XDG_STATE_HOME/jj_tui/
`macos`: ~/Library/logs/jj_tui/


# Dev
Can be built with nix `nix build` or open a nix shell with `nix develop`
For non-nix the project is currently not setup to build with dune or opam. Sorry, I'll get there one day.
