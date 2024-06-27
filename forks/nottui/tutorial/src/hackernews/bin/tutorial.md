# Building a HackerNews-like Interface with OCaml

## Introduction

This tutorial will guide you through creating a HackerNews-like interface using Nottui. You'll learn how to render values, do styling, handle user input, create selection lists, and make popups. By the end of this tutorial, you'll have a functional, interactive UI that mimics some of HackerNews' core features.

## Table of Contents

1. Preliminary Setup
2. First Iteration: Post Rendering
3. Expanding the Application
   - Styling Improvements
   - Keyboard Input Handling
   - Selection Lists
   - Popups and Focus Management
   - Implementing Post Sorting

## 1. Preliminary Setup

Let's start by ensuring everything works properly:

1. Create a file named `hackernews.ml` with the following test content:

$#1

2. Run `dune exec hackernews.exe`. You should see a friendly greeting message.

## 2. First Iteration: Post Rendering

### 2.1 Creating the Post UI

We'll begin by rendering our posts. Here's the code for our `post_ui` function:

$#2

Let's break it down piece by piece:

1. Extract the website domain from the URL:

$#3

2. Create two horizontal rows stacked vertically:

```ocaml
Ui.vcat
  [ Ui.hcat
      [(* *)]
  ; Ui.hcat 
      [ (* *)]
  ]
```

3. Set text styling:
   The `~attr` parameter allows us to set styling for text. In this case, we set the style to bold.
   We can also use `A.fg` to set the foreground color or `A.bg` to set the background color.
   (See: [TODO: Add link to styling documentation] for more info)

```ocaml
W.string ~attr:A.(st bold) title;
```

4. Use `Lwd.pure`:
   `Lwd.pure` has the signature `'a -> 'a Lwd.t`. It's a way to take static UI (or any data) and make it play nice with functions that take reactive `Lwd.t` values. 
   You'll always need to use this to incorporate UI elements that don't depend on reactive data into the rest of your UI.

```ocaml
|> Lwd.pure
```

5. Add a focusable border:
   This puts a border around our post. Because we use a focusable box, it will highlight when focused, which can be changed using `Alt+Up` or `Alt+Down`.

```ocaml
|> W.Box.focusable
```

### 2.2 Creating the Main UI

Now we need some posts to render. We'll use a fake version of the HackerNews API for now:

$#4

Note that we used `W.vbox` rather than `Ui.vcat`. That's because each item is now a `Ui.t Lwd.t`, and `Ui.vcat` only accepts `Ui.t`.

Typically, you'll use `Ui.*` functions for creating small pieces of UI and `W.*` functions for larger transformations. For example, `Ui.string` creates a single string, while `W.Scroll.area` makes any UI scrollable.

## 3. Expanding the Application

Now that we have an MVP that shows our basic data rendering the way we want, let's expand it. 

In this chapter we will:\
1. Make our styling a little nicer  
2. learn about how to handle keybaord input
3. Make selection lists
4. Make popups and move focus around
5. Use all that to make a popup allowing the user to select how they want posts sorted

### 3.1 Styling Improvements

Let's update our `post_ui` function with some styling enhancements:

$#5

The main change here is making our items stretch to fill the entire screen. We do this by setting the **stretch width** (`sw`) to a non-zero value and also setting our **max width** (`mw`) to a value much wider than a screen could ever be. By default, `max_width` is the same as the object's width.

```ocaml
|> Ui.resize ~sw:1 ~mw:10000
```

We've also added spacing between elements for improved readability.

### 3.2 Implementing Sorting

First, let's set up some variables to store our state. For a simple UI, we'll keep these in the global scope, but for more modular designs, you might want to put these inside the function they are relevant to.

These variables will be `Lwd.var`s. An `Lwd.var` is essentially a `ref` that can be turned into an `Lwd.t` that reacts to the var being set.

- `show_prompt_var`: Defines if the prompt is shown, and if so, what content to show
- `sorting_mode_var`: Stores how we should sort the posts

$#6


### 3.3 Creating the Sorting Prompt

We will see how this function fits together. 

$#7

Let's add the overlay to our main UI and give it the var that controls the prompt. We'll also make the body stretchable to give our prompt some space:

$#8

Next, we'll process keyboard inputs. When 's' is pressed, we'll set the `show_prompt_var` to our prompt:

$#9

Notice how we used the `$=` operator to assign a value to the `Lwd.var`. This is just an alias to `Lwd.set` that looks a little nicer:

```ocaml
| `Finished sorting -> sorting_mode_var $= sorting
```

Here's a helper function to choose the sorting method:

$#10

### 3.4 Putting it all together

We've extended the posts generation to include a sorting step using our selected sorting function. We've also added a section at the bottom to show the key the user should press to open the sorting prompt:

$#11

Note that we pass all the other UI into the sorting prompt because we want it to pop up over everything:

$#12

Let's add a status indicator to show the current sorting mode:

$#15

This is our first use of `let$`! We're finally making a piece of UI that is reactive to changes. In this case, this UI will update whenever `sorting_mode_var` changes.

`let$` is syntactic sugar for `Lwd.map`. Just like `List.map`, it allows us to apply a transformation function to the contents of the `Lwd.t`. We also use `Lwd.get` to turn our `Lwd.var` into an `Lwd.t` as described earlier.

The equivalent code to `let$` is:

```ocaml
Lwd.get sorting_mode_var |> Lwd.map ~f:(fun sort_mode ->
  (*..rest...*)
```

Here we see `let$*`, which is similar to `let$` except that it is `Lwd.bind`. It's necessary when the result of the transformation is itself an `Lwd.t`. You're likely familiar with `Result.bind`, which behaves similarly.

$#14

In general, `let$*` should be avoided because it causes whatever is inside it to be fully recomputed when the `Lwd.t` it is binding on changes. However, in this case, that makes sense because our list will have to be fully re-sorted anyway.

In the next chapter, you'll see more use of both `let$` and `let$*`.

That's it! You've now created a HackerNews-like interface with OCaml, complete with post rendering, sorting functionality, and an interactive UI. Here's the full source code for reference:

$#13

## Wrap up

I hope this was helpful, and you've now got some idea how to put together a nottui app. If you have any feedback please make an issue on the repo, or message me @faldor20 on the ocaml discord. 

