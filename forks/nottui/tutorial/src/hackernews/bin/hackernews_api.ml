type post = {
  id : int;
  title : string;
  url : string;
  score : int;
  comments : int;
  kids: int list;
}

(** generates a list of child ids, skewed twards low numbers *)
let generate_kids_list()=

List.init (Float.pow ((Random.float 10.0)/. 3.0) 6.0|>Int.of_float) (fun _ -> Random.int 10000000 + 2000000)

(** returns a list of posts from hackernews*)
let fake_posts () =
  let titles = [
    "OCaml 5.0 Released: What’s New?";
    "Why Functional Programming Matters";
    "Building Scalable Systems with OCaml";
    "Understanding Type Systems";
    "Introduction to Category Theory";
    "The Future of Multi-Core OCaml";
    "How to Contribute to Open Source Projects";
    "OCaml vs Haskell: A Comparison";
    "Getting Started with MirageOS";
    "Real-World Applications of OCaml"
  ] in
  let urls = [
    "https://ocaml.com/ocaml-5-released";
    "https://functional.com/functional-programming-matters";
    "https://scaleable.com/building-scalable-systems";
    "https://understanding.com/understanding-type-systems";
    "https://theory.com/introduction-to-category-theory";
    "https://multicore.com/future-of-multicore-ocaml";
    "https://contrib.com/contributing-to-open-source";
    "https://haskell.com/ocaml-vs-haskell";
    "https://migrations.com/getting-started-mirageos";
    "https://realworldocaml.com/real-world-applications-ocaml"
  ] in
  let rec make_posts ids titles urls scores comments acc =
    match (ids, titles, urls, scores, comments) with
    | ([], [], [], [], []) -> List.rev acc
    | (id::ids_tail, title::titles_tail, url::urls_tail, score::scores_tail, comment::comments_tail) ->
        let post = {id; title; url; score; comments = comment; kids=generate_kids_list()} in
        make_posts ids_tail titles_tail urls_tail scores_tail comments_tail (post :: acc)
    | _ -> acc
  in
  let ids = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] in
  let scores = [120; 85; 99; 75; 110; 95; 130; 90; 70; 80] in
  let comments = [15; 25; 30; 20; 35; 40; 50; 45; 10; 5] in
  make_posts ids titles urls scores comments []

  type comment = {
  by : string;
  id : int;
  kids : int list;
  parent : int;
  text : string;
  time : int;
  comment_type : string;
}

let generate_fake_comment parent_id comment_id =
  let authors = ["ocaml_fan"; "functional_guru"; "type_safety_advocate"; "pattern_matcher"; "monad_master"] in
  let texts = [
    "Great article! I've been using OCaml for years and it never ceases to amaze me.";
    "I disagree with some points, but overall a good read.";
    "Has anyone tried implementing this in a production environment?";
    "This reminds me of a similar approach we took in our project. It worked wonders!";
    "I'd love to see a follow-up article exploring this topic further.";
    "The author makes some interesting points, but I think they're overlooking some key issues.";
    "This is a game-changer for functional programming. Can't wait to try it out!";
    "I'm skeptical about the performance claims. Has anyone done benchmarks?";
    "As always, it depends on the specific use case. YMMV.";
    "I've been waiting for something like this for a long time. Thanks for sharing!"
  ] in
  {
    by = List.nth authors (Random.int (List.length authors));
    id = comment_id;
    kids = generate_kids_list();
    parent = parent_id;
    text = List.nth texts (Random.int (List.length texts));
    time = int_of_float (Unix.time ()) - Random.int 86400;  (* Random time within last 24 hours *)
    comment_type = "comment";
  }

(*generates a comment with some number of children mostly those comments will have no children, but some will*)
let generate_fake_comments count parent_id  =
  List.init count (fun _ -> generate_fake_comment parent_id (Random.int 10000000 + 2000000))
