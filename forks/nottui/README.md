Nottui is a toolkit for making terminal user-interfaces.
It builds upon [Notty](https://github.com/pqwy/notty/), adding a layout DSL and support for event dispatch,
and [Lwd](https://github.com/let-def/lwd) which is used for interactivity.

**This repo is forked from [Lwd](https://github.com/let-def/lwd) I'd like to get it reintegrated with that at some point, I'm just waiting until let-def has a little more time on his plate**

# Getting started

The package is distributed through opam: 

```bash
$ opam install nottui
```

## Tutorial
There is a tutorial for nottui which will take you through the basics of making a small application, it covers: 
- how to do layout 
- reactive values
- handling keyboard input 
- and much more!

See [tutorial](tutorial/hackernews/tutorial.md)

## Docs
For more details on how to write Nottui applications and how Nottui works see the [docs](docs/).
I recommend reading at least [the fundimentals](docs/fundimentals.md).
## Examples

Here are a few examples of using Nottui, more can be found in the [examples](examples/) folder:

Let's start with Hello world.

#### Hello world

```ocaml
#require "nottui";;
open Nottui;;

Ui_loop.run (Lwd.pure (W.printf "Hello world"));;
```

Running the application is just a matter of calling `Ui_loop.run` with a ui
value.

**Note:** Press Ctrl-Q to return to the top-level (or shell).

#### Counting clicks

Now we will count the number of clicks on a button.

```ocaml
let vcount = Lwd.var 0;;

let button count = 
  W.button (Printf.sprintf "Clicked %d times!" count)
           (fun () -> Lwd.set vcount (count + 1));;
  
Ui_loop.run (Lwd.map button (Lwd.get vcount));;
```

We reserve state for holding the number of clicks, we render a button and
increment the state when clicked.

#### Displaying a tree

```ocaml
type tree = Tree of string * (unit -> tree list)

let rec tree_ui (Tree (label, child)) =
  let opened = Lwd.var false in
  let render is_opened =
    let btn_text = if is_opened then "[-] " else "[+] " in
    let btn_action () = Lwd.set opened (not is_opened) in
    let btn = W.button (btn_text ^ label) btn_action in
    let layout node forest =
      Ui.join_y node (Ui.join_x (Ui.space 2 0) forest) 
    in
    if is_opened 
    then Lwd.map (layout btn) (forest_ui (child ()))
    else Lwd.pure btn
  in
  Lwd.join (Lwd.map render (Lwd.get opened))
  
and forest_ui nodes = 
  Lwd_utils.pack Ui.pack_y 
    (List.map tree_ui nodes)
;;

let rec fake_fs () = [
  Tree ("bin", fake_fs);
  Tree ("home", fake_fs);
  Tree ("usr", fake_fs);
] in
Ui_loop.run (forest_ui (fake_fs ()));;
```
