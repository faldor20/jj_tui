(**
   `render_jj_graph.ml`

   This module is a small, self-contained experiment for rendering jj-style commit graphs
   in a terminal. The renderer is **lane-based**:

   The tests in `render_jj_graph_tests.ml` are "golden" tests: they assert the exact glyph
   output. When changing the algorithm, prefer updating the algorithm to match the golden
   outputs, not the other way around.
*)

(** Glyph constants used by the renderer. *)
module P = struct
  let v = Notty.make_uchar "│"
  let vr = Notty.make_uchar "├"
  let vl = Notty.make_uchar "┤"
  let t = Notty.make_uchar "┬"
  let cross = Notty.make_uchar "┼"
  let h = Notty.make_uchar "─"
  let b = Notty.make_uchar "┴"

  (* elbow down right *)
  let edr = Notty.make_uchar "╮"
  let eur = Notty.make_uchar "╯"
  let edl = Notty.make_uchar "╭"
  let eul = Notty.make_uchar "╰"
  let sp = Notty.make_uchar " "
  let ancestor = Notty.make_uchar "╷"
  let term = Notty.make_uchar "~"

  module Node = struct
    let normal = Notty.make_uchar "○"
    let working_copy = Notty.make_uchar "@"
    let wip = Notty.make_uchar "◌"
    let immutable = Notty.make_uchar "◆"
    let conflict = Notty.make_uchar "×"
  end
end

(** Node type for the graph. *)
type node = {
    parents : node list
  ; creation_time : int64
  ; working_copy : bool
  ; immutable : bool
  ; wip : bool
  ; change_id : string
  ; commit_id : string
  ; description : string
  ; bookmarks : string list
  ; workspaces : string list
        (** Names of workspaces whose working-copy commit is this node, e.g.
            ["default"; "ws1"]. Rendered as [name@] after the timestamp. *)
  ; author_email : string
  ; author_timestamp : string
  ; empty : bool
  ; hidden : bool
  ; divergent : bool
  ; conflict : bool
  ; is_preview : bool
  ; change_id_prefix : string
  ; change_id_rest : string
  ; commit_id_prefix : string
  ; commit_id_rest : string
}

(** Special marker for elided nodes. Visible elision rows and anonymous placeholder
    parents both derive their ids from this prefix, which lets the renderer keep
    them distinct while still recognizing them as elision markers. *)
let elided_marker = "~ELIDED~"

let is_elided_id id = String.starts_with ~prefix:elided_marker id

(** Create a special node representing an elided section *)
let make_elided_node ?(id = elided_marker) ?(parents = []) () : node =
  {
    parents
  ; creation_time = Int64.zero
  ; working_copy = false
  ; immutable = false
  ; wip = false
  ; change_id = id
  ; commit_id = id
  ; description = "(elided revisions)"
  ; bookmarks = []
  ; workspaces = []
  ; author_email = ""
  ; author_timestamp = ""
  ; empty = false
  ; hidden = true
  ; divergent = false
  ; conflict = false
  ; is_preview = false
  ; change_id_prefix = ""
  ; change_id_rest = ""
  ; commit_id_prefix = ""
  ; commit_id_rest = ""
  }
;;

(** Check if a node represents an elided section *)
let is_elided (n : node) : bool = is_elided_id n.commit_id

(* ============================================================================
   Preview Node Support
   ============================================================================ *)

type preview_mode =
  [ `Insert_before
  | `Insert_after
  | `Add_after
  ]

type preview_source_mode =
  [ `Revisions
  | `Source
  | `Branch
  ]

module StringSet = Set.Make (String)

let node_matches_rev (n : node) rev = n.change_id = rev || n.commit_id = rev

let dedupe_preserve_order items =
  let seen = Hashtbl.create (List.length items) in
  List.filter
    (fun item ->
       if Hashtbl.mem seen item
       then false
       else (
         Hashtbl.add seen item ();
         true))
    items
;;

let resolve_revs (nodes : node list) (revs : string list) : string list =
  if revs = []
  then []
  else (
    nodes
    |> List.filter (fun n -> List.exists (node_matches_rev n) revs)
    |> List.map (fun n -> n.commit_id)
    |> dedupe_preserve_order)
;;

type resolved_revs = {
    source_ids : string list
  ; target_ids : string list
  ; source_set : StringSet.t
}

let resolve_sources_targets
      ~(nodes : node list)
      ~(sources : string list)
      ~(targets : string list) : resolved_revs
  =
  let source_ids = resolve_revs nodes sources in
  let target_ids = resolve_revs nodes targets in
  let source_set = StringSet.of_list source_ids in
  { source_ids; target_ids; source_set }
;;

(** [default_preview_targets ~sources nodes]
    Computes default rebase target revisions by collecting the parents of each
    root in the source set (root = source commit whose parents are all outside
    the source set).  This preserves the existing tree shape when entering
    preview mode instead of using the currently-hovered commit as the target. *)
let default_preview_targets ~(sources : string list) (nodes : node list) : string list =
  if sources = []
  then []
  else (
    let source_ids = resolve_revs nodes sources in
    let source_set = StringSet.of_list source_ids in
    let seen = Hashtbl.create 8 in
    (* Walk roots in graph order and keep parent order stable so the preview
       starts from the commit's existing parent layout. *)
    nodes
    |> List.filter (fun n -> StringSet.mem n.commit_id source_set)
    |> List.filter (fun n ->
      not (List.exists (fun p -> StringSet.mem p.commit_id source_set) n.parents))
    |> List.concat_map (fun root ->
      root.parents
      |> List.map (fun p -> p.commit_id)
      |> List.filter (fun id -> not (StringSet.mem id source_set)))
    |> List.filter (fun id ->
      if Hashtbl.mem seen id
      then false
      else (
        Hashtbl.add seen id ();
        true)))
;;

let validate_preview_cycles
      ~(mode : preview_mode)
      ~(ancestors_of : string -> StringSet.t)
      ~(source_ids : string list)
      ~(target_ids : string list) : string option
  =
  let invalid_target target_id =
    List.exists
      (fun source_id ->
         if source_id = target_id
         then true
         else (
           let source_ancestors = ancestors_of source_id in
           let target_ancestors = ancestors_of target_id in
           match mode with
           | `Insert_before ->
             StringSet.mem target_id source_ancestors
           | `Insert_after | `Add_after ->
             StringSet.mem source_id target_ancestors))
      source_ids
  in
  if List.exists invalid_target target_ids
  then Some "Preview blocked: cycle detected"
  else None
;;

let preview_id_for ?source_id ?target_id ~label () =
  match source_id, target_id with
  | Some source_id, _ ->
    Printf.sprintf "preview:%s" source_id
  | None, Some target_id ->
    Printf.sprintf "preview:%s:%s" label target_id
  | None, None ->
    Printf.sprintf "preview:%s" label
;;

let make_preview_clone ~source_node ~preview_id ~description =
  {
    source_node with
    commit_id = preview_id
  ; change_id = preview_id
  ; description
  ; is_preview = true
  }
;;

let build_parent_map (nodes : node list) =
  let map = Hashtbl.create (List.length nodes) in
  List.iter
    (fun n -> Hashtbl.replace map n.commit_id (List.map (fun p -> p.commit_id) n.parents))
    nodes;
  map
;;

let build_children_map parent_map =
  let children = Hashtbl.create (Hashtbl.length parent_map) in
  Hashtbl.iter
    (fun child_id parent_ids ->
       List.iter
         (fun parent_id ->
            let existing =
              Option.value (Hashtbl.find_opt children parent_id) ~default:[]
            in
            Hashtbl.replace children parent_id (child_id :: existing))
         parent_ids)
    parent_map;
  children
;;

let descendants_of ~children_map ~sources =
  let visited = Hashtbl.create (List.length sources * 2) in
  let queue = Queue.create () in
  List.iter
    (fun id ->
       if not (Hashtbl.mem visited id)
       then (
         Hashtbl.add visited id ();
         Queue.add id queue))
    sources;
  while not (Queue.is_empty queue) do
    let current = Queue.take queue in
    let children = Option.value (Hashtbl.find_opt children_map current) ~default:[] in
    List.iter
      (fun child ->
         if not (Hashtbl.mem visited child)
         then (
           Hashtbl.add visited child ();
           Queue.add child queue))
      children
  done;
  visited |> Hashtbl.to_seq_keys |> List.of_seq
;;

let build_ancestors parent_map =
  let cache = Hashtbl.create (Hashtbl.length parent_map) in
  let rec ancestors id =
    match Hashtbl.find_opt cache id with
    | Some result ->
      result
    | None ->
      let parents = Option.value (Hashtbl.find_opt parent_map id) ~default:[] in
      let result =
        List.fold_left
          (fun acc parent_id ->
             let acc = StringSet.add parent_id acc in
             StringSet.union acc (ancestors parent_id))
          StringSet.empty
          parents
      in
      Hashtbl.replace cache id result;
      result
  in
  ancestors
;;

type graph_model = {
    ordered_ids : string list
  ; node_by_id : (string, node) Hashtbl.t
  ; parent_map : (string, string list) Hashtbl.t
  ; children_map : (string, string list) Hashtbl.t
  ; order_index : (string, int) Hashtbl.t
}

type transformed_graph = {
    node_ids : string list
  ; base_nodes : (string, node) Hashtbl.t
  ; parent_map : (string, string list) Hashtbl.t
  ; order_index : (string, int) Hashtbl.t
}

type preview_shape = {
    root_preview_ids : string list
  ; head_preview_ids : string list
}

(** Build the canonical commit DAG once so preview transformation and row layout
    both operate on the same topology. *)
let build_graph_model (nodes : node list) : graph_model =
  let ordered_ids = List.map (fun n -> n.commit_id) nodes in
  let node_by_id = Hashtbl.create (List.length nodes) in
  let order_index = Hashtbl.create (List.length nodes) in
  List.iteri
    (fun index node ->
       Hashtbl.replace node_by_id node.commit_id node;
       Hashtbl.replace order_index node.commit_id index)
    nodes;
  let parent_map = build_parent_map nodes in
  let children_map = build_children_map parent_map in
  { ordered_ids; node_by_id; parent_map; children_map; order_index }
;;

(** When a preview removes a selected source subtree from the visible graph, any
    surviving descendants must reconnect to the first parents outside that source
    set. This keeps the non-preview graph connected without relying on the old
    single-source special case. *)
let build_external_parent_frontier
      ~(graph : graph_model)
      ~(source_set : StringSet.t)
      ~(source_ids : string list) =
  let cache = Hashtbl.create (List.length source_ids) in
  let rec external_parents source_id =
    match Hashtbl.find_opt cache source_id with
    | Some parent_ids ->
      parent_ids
    | None ->
      let parent_ids =
        Option.value (Hashtbl.find_opt graph.parent_map source_id) ~default:[]
        |> List.concat_map (fun parent_id ->
          if StringSet.mem parent_id source_set then external_parents parent_id else [ parent_id ])
        |> dedupe_preserve_order
      in
      Hashtbl.replace cache source_id parent_ids;
      parent_ids
  in
  List.iter (fun source_id -> ignore (external_parents source_id)) source_ids;
  external_parents
;;

(** Remove selected sources from the visible graph while preserving the remaining
    topology. This produces the preview-agnostic graph that later edits build on. *)
let remove_sources_from_graph
      ~(graph : graph_model)
      ~(source_ids : string list)
      ~(source_set : StringSet.t) : transformed_graph
  =
  let node_ids = graph.ordered_ids |> List.filter (fun id -> not (StringSet.mem id source_set)) in
  let base_nodes = Hashtbl.create (List.length node_ids) in
  let parent_map = Hashtbl.create (List.length node_ids) in
  let order_index = Hashtbl.create (List.length node_ids + List.length source_ids) in
  List.iter
    (fun id ->
       let node = Hashtbl.find graph.node_by_id id in
       Hashtbl.replace base_nodes id node;
       Hashtbl.replace order_index id (Hashtbl.find graph.order_index id);
       let parent_ids =
         Option.value (Hashtbl.find_opt graph.parent_map id) ~default:[]
         |> List.filter (fun parent_id -> not (StringSet.mem parent_id source_set))
       in
       Hashtbl.replace parent_map id parent_ids)
    node_ids;
  let external_parents =
    build_external_parent_frontier ~graph ~source_set ~source_ids
  in
  List.iter
    (fun source_id ->
       let replacement_parents = external_parents source_id in
       let children = Option.value (Hashtbl.find_opt graph.children_map source_id) ~default:[] in
       List.iter
          (fun child_id ->
             if Hashtbl.mem parent_map child_id
             then (
               let child_parents =
                 Option.value (Hashtbl.find_opt parent_map child_id) ~default:[]
               in
               let updated_parents =
                 child_parents
                 |> List.concat_map (fun parent_id ->
                   if parent_id = source_id then replacement_parents else [ parent_id ])
                 |> List.filter (fun parent_id -> not (StringSet.mem parent_id source_set))
                 |> dedupe_preserve_order
               in
               Hashtbl.replace parent_map child_id updated_parents))
          children)
    source_ids;
  { node_ids; base_nodes; parent_map; order_index }
;;

(** Preview nodes mirror the moved source subgraph. Their internal parent edges
    preserve the original source-set structure, while the outer edit later decides
    how that cloned subgraph attaches to the rest of the graph. *)
let add_preview_subgraph
      ~(graph : graph_model)
      ~(transformed : transformed_graph)
      ~(source_ids : string list)
      ~(source_set : StringSet.t) : transformed_graph * preview_shape
  =
  let preview_map = Hashtbl.create (List.length source_ids) in
  List.iter
    (fun source_id ->
       let source_node = Hashtbl.find graph.node_by_id source_id in
       let preview_id = preview_id_for ~label:"preview" ~source_id () in
       let preview_node =
         make_preview_clone
           ~source_node
           ~preview_id
           ~description:("preview: " ^ source_node.description)
       in
       Hashtbl.replace transformed.base_nodes preview_id preview_node;
       Hashtbl.replace transformed.parent_map preview_id [];
       Hashtbl.replace transformed.order_index preview_id (Hashtbl.find graph.order_index source_id);
       Hashtbl.replace preview_map source_id preview_id)
    source_ids;
  List.iter
    (fun source_id ->
       let preview_id = Hashtbl.find preview_map source_id in
       let preview_parent_ids =
         Option.value (Hashtbl.find_opt graph.parent_map source_id) ~default:[]
         |> List.filter (fun parent_id -> StringSet.mem parent_id source_set)
         |> List.filter_map (fun parent_id -> Hashtbl.find_opt preview_map parent_id)
       in
       Hashtbl.replace transformed.parent_map preview_id preview_parent_ids)
    source_ids;
  let root_ids =
    source_ids
    |> List.filter (fun source_id ->
      Option.value (Hashtbl.find_opt graph.parent_map source_id) ~default:[]
      |> List.exists (fun parent_id -> StringSet.mem parent_id source_set)
      |> not)
  in
  let head_ids =
    source_ids
    |> List.filter (fun source_id ->
      Option.value (Hashtbl.find_opt graph.children_map source_id) ~default:[]
      |> List.exists (fun child_id -> StringSet.mem child_id source_set)
      |> not)
  in
  let preview_ids = List.map (fun source_id -> Hashtbl.find preview_map source_id) source_ids in
  let root_preview_ids =
    List.map (fun source_id -> Hashtbl.find preview_map source_id) root_ids
  in
  let head_preview_ids =
    List.map (fun source_id -> Hashtbl.find preview_map source_id) head_ids
  in
  ({ transformed with node_ids = transformed.node_ids @ preview_ids },
   { root_preview_ids; head_preview_ids })
;;

(** Apply the outer rebase edit to the transformed graph. The row renderer never
    reasons about preview semantics; it only consumes this already-edited DAG. *)
let apply_preview_edit
      ~(mode : preview_mode)
      ~(target_ids : string list)
      ~(preview_shape : preview_shape)
      ~(transformed : transformed_graph) : transformed_graph
  =
  let target_parent_union =
    target_ids
    |> List.concat_map (fun target_id ->
      Option.value (Hashtbl.find_opt transformed.parent_map target_id) ~default:[])
    |> dedupe_preserve_order
  in
  let children_before_attachment = build_children_map transformed.parent_map in
  (match mode with
   | `Insert_before ->
     List.iter
       (fun preview_id ->
          Hashtbl.replace transformed.parent_map preview_id target_parent_union)
       preview_shape.root_preview_ids;
     List.iter
       (fun target_id ->
          if Hashtbl.mem transformed.parent_map target_id
          then Hashtbl.replace transformed.parent_map target_id preview_shape.head_preview_ids)
       target_ids
   | `Insert_after ->
     List.iter
       (fun preview_id -> Hashtbl.replace transformed.parent_map preview_id target_ids)
       preview_shape.root_preview_ids;
     List.iter
       (fun target_id ->
          let children =
            Option.value (Hashtbl.find_opt children_before_attachment target_id) ~default:[]
          in
          List.iter
            (fun child_id ->
               if Hashtbl.mem transformed.parent_map child_id
               then (
                 let child_parents =
                   Option.value (Hashtbl.find_opt transformed.parent_map child_id) ~default:[]
                 in
                 let updated_parents =
                   List.filter (fun parent_id -> parent_id <> target_id) child_parents
                   @ preview_shape.head_preview_ids
                   |> dedupe_preserve_order
                 in
                 Hashtbl.replace transformed.parent_map child_id updated_parents))
            children)
       target_ids
   | `Add_after ->
     List.iter
       (fun preview_id -> Hashtbl.replace transformed.parent_map preview_id target_ids)
       preview_shape.root_preview_ids);
  transformed
;;

(** Derive a child-first topological order directly from the transformed graph.
    Original graph order is only used as a stable tie-break when several nodes are
    simultaneously valid next rows. *)
let stable_topological_order (graph : transformed_graph) : string list =
  let children_map = build_children_map graph.parent_map in
  let node_set = Hashtbl.create (List.length graph.node_ids) in
  let remaining_children = Hashtbl.create (List.length graph.node_ids) in
  List.iter (fun id -> Hashtbl.replace node_set id ()) graph.node_ids;
  List.iter
    (fun id ->
       let child_count =
         Option.value (Hashtbl.find_opt children_map id) ~default:[]
         |> List.fold_left
              (fun acc child_id -> if Hashtbl.mem node_set child_id then acc + 1 else acc)
              0
       in
       Hashtbl.replace remaining_children id child_count)
    graph.node_ids;
  let compare_ids left right =
    let left_index = Option.value (Hashtbl.find_opt graph.order_index left) ~default:max_int in
    let right_index = Option.value (Hashtbl.find_opt graph.order_index right) ~default:max_int in
    match Int.compare left_index right_index with
    | 0 ->
      String.compare left right
    | order ->
      order
  in
  let ready =
    ref
      (graph.node_ids
       |> List.filter (fun id -> Hashtbl.find remaining_children id = 0)
       |> List.sort compare_ids)
  in
  let emitted = Hashtbl.create (List.length graph.node_ids) in
  let ordered_rev = ref [] in
  while !ready <> [] do
    let next_id = List.hd !ready in
    ready := List.tl !ready;
    if not (Hashtbl.mem emitted next_id)
    then (
      Hashtbl.replace emitted next_id ();
      ordered_rev := next_id :: !ordered_rev;
      Option.value (Hashtbl.find_opt graph.parent_map next_id) ~default:[]
      |> List.iter (fun parent_id ->
        if Hashtbl.mem remaining_children parent_id
        then (
          let next_count = Hashtbl.find remaining_children parent_id - 1 in
          Hashtbl.replace remaining_children parent_id next_count;
          if next_count = 0
          then ready := List.sort compare_ids (parent_id :: !ready))))
  done;
  let ordered_ids = List.rev !ordered_rev in
  if List.length ordered_ids = List.length graph.node_ids
  then ordered_ids
  else (
    let missing_ids =
      graph.node_ids
      |> List.filter (fun id -> not (Hashtbl.mem emitted id))
      |> List.sort compare_ids
    in
    ordered_ids @ missing_ids)
;;

let materialize_transformed_graph (graph : transformed_graph) : node list =
  (* Keep anonymous elided parents available as node objects so child rows can still
     render termination markers, while visible elided rows are emitted separately
     through [graph.node_ids]. *)
  Hashtbl.iter
    (fun _ parent_ids ->
       List.iter
         (fun parent_id ->
            if is_elided_id parent_id && not (Hashtbl.mem graph.base_nodes parent_id)
            then Hashtbl.replace graph.base_nodes parent_id (make_elided_node ~id:parent_id ()))
         parent_ids)
    graph.parent_map;
  let ordered_ids = stable_topological_order graph in
  let final_nodes = Hashtbl.create (List.length ordered_ids) in
  let rec build_node id =
    match Hashtbl.find_opt final_nodes id with
    | Some node ->
      node
    | None ->
      let base_node = Hashtbl.find graph.base_nodes id in
      let parents =
        Option.value (Hashtbl.find_opt graph.parent_map id) ~default:[]
        |> List.filter (fun parent_id -> Hashtbl.mem graph.base_nodes parent_id)
        |> List.map build_node
      in
      let node = { base_node with parents } in
      Hashtbl.replace final_nodes id node;
      node
  in
  List.map build_node ordered_ids
;;

(** [expand_preview_sources ~mode ~sources ~targets nodes]
    Expands a list of "sources" of a rebase preview, given the preview source mode,
    the starting sources and targets, and the list of graph [nodes].

    The purpose of this function is to determine, based on user actions, which commits
    should be highlighted or affected by a rebase preview operation in the commit graph UI.

    - For [`Revisions] mode, the expansion consists of just [sources] themselves.
    - For [`Source] mode, it includes all descendants of [sources] (i.e., each source and all its children recursively).
    - For [`Branch] mode, it computes the entire branch: the "base" is the set of ancestors of [sources] but not ancestors of [targets];
      then it includes all descendants of this base set. This produces the same set of commits as would be affected by a `jj rebase -b ...`.

    The function returns all commit ids in [nodes] which are in the computed set according to the chosen mode.
*)
let expand_preview_sources
      ~(mode : preview_source_mode)
      ~(sources : string list)
      ~(targets : string list)
      (nodes : node list) : string list
  =
  if sources = []
  then []
  else (
    let parent_map = build_parent_map nodes in
    let children_map = build_children_map parent_map in
    let ancestors_of = build_ancestors parent_map in
    let sources = resolve_revs nodes sources in
    let targets = resolve_revs nodes targets in
    let descendants = descendants_of ~children_map ~sources in
    let expanded =
      match mode with
      | `Revisions ->
        sources
      | `Source ->
        descendants
      | `Branch ->
        let ancestors_of_targets =
          targets
          |> List.fold_left
               (fun acc target_id ->
                  let ancestors = ancestors_of target_id |> StringSet.elements in
                  StringSet.union acc (StringSet.of_list (target_id :: ancestors)))
               StringSet.empty
        in
        let ancestors_of_sources =
          sources
          |> List.fold_left
               (fun acc source_id ->
                  let ancestors = ancestors_of source_id |> StringSet.elements in
                  StringSet.union acc (StringSet.of_list (source_id :: ancestors)))
               StringSet.empty
        in
        let base_set =
          StringSet.diff ancestors_of_sources ancestors_of_targets |> StringSet.elements
        in
        let branch_descendants = descendants_of ~children_map ~sources:base_set in
        StringSet.(union (of_list base_set) (of_list branch_descendants) |> elements)
    in
    let expanded_set = StringSet.of_list expanded in
    nodes
    |> List.filter (fun n -> StringSet.mem n.commit_id expanded_set)
    |> List.map (fun n -> n.commit_id))
;;

let apply_rebase_preview
      ~(mode : preview_mode)
      ~(sources : string list)
      ~(targets : string list)
      (nodes : node list) : node list * string option
  =
  if sources = [] || targets = []
  then nodes, None
  else (
    let graph = build_graph_model nodes in
    let { source_ids; target_ids; source_set } =
      resolve_sources_targets ~nodes ~sources ~targets
    in
    if source_ids = [] || target_ids = []
    then nodes, None
    else (
      let source_order =
        graph.ordered_ids |> List.filter (fun id -> StringSet.mem id source_set)
      in
      let transformed =
        remove_sources_from_graph ~graph ~source_ids:source_order ~source_set
      in
      let ancestors_of = build_ancestors transformed.parent_map in
      match validate_preview_cycles ~mode ~ancestors_of ~source_ids ~target_ids with
      | Some msg ->
        nodes, Some msg
      | None ->
        let transformed, preview_shape =
          add_preview_subgraph
            ~graph
            ~transformed
            ~source_ids:source_order
            ~source_set
        in
        let transformed =
          apply_preview_edit ~mode ~target_ids ~preview_shape ~transformed
        in
        materialize_transformed_graph transformed, None))
;;

(** Row type classification for structured output *)
type row_type =
  | NodeRow (** The main row with the node glyph *)
  | LinkRow (** Merge/fork connector lines *)
  | PadRow (** Padding/continuation lines *)
  | TermRow (** Termination lines with ~ *)

(** Structured output for UI integration *)
type graph_row_output = {
    graph_chars : string (** The graph prefix like "○ " or "├─╮" *)
  ; graph_image : Notty.image (** Notty image for graph prefix, with styling *)
  ; node : node (** The node this row represents *)
  ; row_type : row_type (** What kind of row this is *)
}

(** Column state - tracks what occupies each graph column *)
type column =
  | Empty
  | Blocked
  | Reserved of node
  | Ancestor of node
  | Parent of node

(** Ancestor type for parent specifications *)
type ancestor_type =
  | A_Ancestor of node
  | A_Parent of node
  | A_Anonymous

(** State for the renderer *)
type state = {
    depth : int
  ; columns : column array
  ; pending_joins : (int * int) list
}

(** Node line entry - what to render in node row for each column *)
type node_line_entry =
  | NL_Blank
  | NL_Ancestor
  | NL_Parent
  | NL_Node

(** Pad line entry - what to render in padding rows *)
type pad_line_entry =
  | PL_Blank
  | PL_Ancestor
  | PL_Parent

(** LinkLine module - bitflags for link row rendering *)
module LinkLine = struct
  type t = int

  let empty = 0
  let horiz_parent = 0x0001
  let horiz_ancestor = 0x0002
  let vert_parent = 0x0004
  let vert_ancestor = 0x0008
  let left_fork_parent = 0x0010
  let left_fork_ancestor = 0x0020
  let right_fork_parent = 0x0040
  let right_fork_ancestor = 0x0080
  let left_merge_parent = 0x0100
  let left_merge_ancestor = 0x0200
  let right_merge_parent = 0x0400
  let right_merge_ancestor = 0x0800
  let child = 0x1000

  (* Compound flags *)
  let horizontal = horiz_parent lor horiz_ancestor
  let vertical = vert_parent lor vert_ancestor
  let left_fork = left_fork_parent lor left_fork_ancestor
  let right_fork = right_fork_parent lor right_fork_ancestor
  let left_merge = left_merge_parent lor left_merge_ancestor
  let right_merge = right_merge_parent lor right_merge_ancestor
  let any_merge = left_merge lor right_merge
  let any_fork = left_fork lor right_fork
  let ( lor ) = ( lor )
  let intersects a b = a land b <> 0
  let contains a b = a land b = b
end

(** Graph row - intermediate representation for one node *)
type graph_row = {
    row_node : node
  ; glyph : Uchar.t
  ; message : string
  ; merge : bool
  ; node_line : node_line_entry array
  ; link_line : LinkLine.t array option
  ; term_line : bool array option
  ; pad_lines : pad_line_entry array
}

(* ============================================================================
   Column utilities (Rust ColumnsExt equivalent)
   ============================================================================ *)

let column_matches col n =
  match col with Empty | Blocked -> false | Reserved o | Ancestor o | Parent o -> o == n
;;

let column_variant = function
  | Empty ->
    0
  | Blocked ->
    1
  | Reserved _ ->
    2
  | Ancestor _ ->
    3
  | Parent _ ->
    4
;;

let column_merge a b = if column_variant b > column_variant a then b else a

let columns_find cols n =
  let rec loop i =
    if i >= Array.length cols
    then None
    else if column_matches cols.(i) n
    then Some i
    else loop (i + 1)
  in
  loop 0
;;

let columns_first_empty cols =
  let rec loop i =
    if i >= Array.length cols
    then None
    else (match cols.(i) with Empty -> Some i | _ -> loop (i + 1))
  in
  loop 0
;;

let columns_find_empty cols ~prefer =
  if prefer < Array.length cols
  then (match cols.(prefer) with Empty -> Some prefer | _ -> columns_first_empty cols)
  else columns_first_empty cols
;;

let column_to_node_line = function
  | Ancestor _ ->
    NL_Ancestor
  | Parent _ ->
    NL_Parent
  | _ ->
    NL_Blank
;;

let column_to_link_line = function
  | Ancestor _ ->
    LinkLine.vert_ancestor
  | Parent _ ->
    LinkLine.vert_parent
  | _ ->
    LinkLine.empty
;;

let column_to_pad_line = function
  | Ancestor _ ->
    PL_Ancestor
  | Parent _ ->
    PL_Parent
  | _ ->
    PL_Blank
;;

let ancestor_to_column = function
  | A_Ancestor n ->
    Ancestor n
  | A_Parent n ->
    Parent n
  | A_Anonymous ->
    Blocked
;;

let ancestor_id = function
  | A_Ancestor n ->
    Some n
  | A_Parent n ->
    Some n
  | A_Anonymous ->
    None
;;

let ancestor_is_direct = function
  | A_Ancestor _ ->
    false
  | A_Parent _ ->
    true
  | A_Anonymous ->
    true
;;

let ancestor_to_link_line anc ~direct ~indirect =
  if ancestor_is_direct anc then direct else indirect
;;

(* Reset columns: Blocked -> Empty, then trim trailing Empty *)
let columns_reset cols =
  let len = Array.length cols in
  for i = 0 to len - 1 do
    match cols.(i) with Blocked -> cols.(i) <- Empty | _ -> ()
  done;
  (* Find last non-empty *)
  let rec find_last i =
    if i < 0 then 0 else (match cols.(i) with Empty -> find_last (i - 1) | _ -> i + 1)
  in
  let new_len = find_last (len - 1) in
  if new_len < len then Array.sub cols 0 new_len else cols
;;

(* ============================================================================
   AncestorColumnBounds - for computing horizontal line ranges
   ============================================================================ *)

type ancestor_bounds = {
    target : int
  ; min_ancestor : int
  ; min_parent : int
  ; max_parent : int
  ; max_ancestor : int
}

let compute_bounds parent_columns target =
  if List.length parent_columns = 0
  then None
  else (
    let indices = List.map fst parent_columns in
    let min_ancestor = List.fold_left min target indices in
    let max_ancestor = List.fold_left max target indices in
    let direct_indices =
      parent_columns
      |> List.filter (fun (_, anc) -> ancestor_is_direct anc)
      |> List.map fst
    in
    let min_parent =
      if List.length direct_indices = 0
      then target
      else min target (List.fold_left min max_int direct_indices)
    in
    let max_parent =
      if List.length direct_indices = 0
      then target
      else max target (List.fold_left max min_int direct_indices)
    in
    Some { target; min_ancestor; min_parent; max_parent; max_ancestor })
;;

let bounds_horizontal_line bounds index =
  if index = bounds.target
  then LinkLine.empty
  else if index > bounds.min_parent && index < bounds.max_parent
  then LinkLine.horiz_parent
  else if index > bounds.min_ancestor && index < bounds.max_ancestor
  then LinkLine.horiz_ancestor
  else LinkLine.empty
;;

(* ============================================================================
   GraphRowRenderer.next_row - core algorithm
   ============================================================================ *)

let next_row ~(columns : column array ref) ~(visible_node_ids : (string, unit) Hashtbl.t) (n : node)
  : graph_row =
  let visible_elided = is_elided n && Hashtbl.mem visible_node_ids n.commit_id in
  let parents =
    n.parents
    |> List.map (fun p ->
      if is_elided p && not (Hashtbl.mem visible_node_ids p.commit_id)
      then A_Anonymous
      else A_Parent p)
    (* Visible elided nodes behave like normal graph nodes so they can sit between
       a commit and its parent. Only elided placeholders that are absent from the
       rendered node order collapse into anonymous termination lines. *)
  in
  (* Find a column for this node *)
  let column =
    match columns_find !columns n with
    | Some i ->
      i
    | None ->
      (match columns_first_empty !columns with
       | Some i ->
         i
       | None ->
         let len = Array.length !columns in
         columns := Array.append !columns [| Empty |];
         len)
  in
  (* Clear the node's column *)
  !columns.(column) <- Empty;
  (* This row is for a merge if there are multiple parents *)
  let merge = List.length parents > 1 in
  (* Build initial row arrays from current columns *)
  let node_line = Array.map column_to_node_line !columns in
  node_line.(column) <- NL_Node;
  let link_line = Array.map column_to_link_line !columns in
  let term_line = Array.map (fun _ -> false) !columns in
  let pad_lines = Array.map column_to_pad_line !columns in
  let need_link_line = ref false in
  let need_term_line = ref false in
  let parent_columns = ref [] in
  List.iter
    (fun p ->
       match ancestor_id p with
       | Some parent_node ->
         (match columns_find !columns parent_node with
          | Some index ->
            !columns.(index) <- column_merge !columns.(index) (ancestor_to_column p);
            parent_columns := (index, p) :: !parent_columns
          | None ->
            (match columns_find_empty !columns ~prefer:column with
             | Some index ->
               !columns.(index) <- column_merge !columns.(index) (ancestor_to_column p);
               parent_columns := (index, p) :: !parent_columns
             | None ->
               let new_idx = Array.length !columns in
               columns := Array.append !columns [| ancestor_to_column p |];
               parent_columns := (new_idx, p) :: !parent_columns))
       | None ->
         (match columns_find_empty !columns ~prefer:column with
          | Some index ->
            !columns.(index) <- column_merge !columns.(index) (ancestor_to_column p);
            parent_columns := (index, p) :: !parent_columns
          | None ->
            let new_idx = Array.length !columns in
            columns := Array.append !columns [| ancestor_to_column p |];
            parent_columns := (new_idx, p) :: !parent_columns))
    parents;
  (* Ensure arrays are long enough for any new columns *)
  let cols_len = Array.length !columns in
  let extend arr default =
    if Array.length arr < cols_len
    then (
      let new_arr = Array.make cols_len default in
      Array.blit arr 0 new_arr 0 (Array.length arr);
      new_arr)
    else arr
  in
  let node_line = extend node_line NL_Blank in
  let link_line = extend link_line LinkLine.empty in
  let term_line = extend term_line false in
  let pad_lines = extend pad_lines PL_Blank in
  (* Mark anonymous parents as terminating *)
  List.iter
    (fun (i, p) ->
       match ancestor_id p with
       | None ->
         term_line.(i) <- true;
         need_term_line := true
       | Some _ ->
         ())
    !parent_columns;
  (* Reverse parent_columns to get proper order *)
  parent_columns := List.rev !parent_columns;
  (* Single parent swap optimization *)
  let link_line =
    if List.length parents = 1
    then (
      match !parent_columns with
      | [ (parent_column, _) ] when parent_column > column ->
        (* Swap columns *)
        let tmp = !columns.(column) in
        !columns.(column) <- !columns.(parent_column);
        !columns.(parent_column) <- tmp;
        (* Update parent_columns *)
        let p = snd (List.hd !parent_columns) in
        parent_columns := [ column, p ];
        (* Generate link line from this column to old parent column *)
        let was_direct =
          LinkLine.intersects link_line.(parent_column) LinkLine.vert_parent
        in
        link_line.(column)
        <- LinkLine.(
             link_line.(column)
             lor if was_direct then right_fork_parent else right_fork_ancestor);
        for i = column + 1 to parent_column - 1 do
          link_line.(i)
          <- LinkLine.(
               link_line.(i) lor if was_direct then horiz_parent else horiz_ancestor)
        done;
        link_line.(parent_column)
        <- (if was_direct
            then LinkLine.left_merge_parent
            else LinkLine.left_merge_ancestor);
        need_link_line := true;
        (* Pad line for old parent column is now blank *)
        pad_lines.(parent_column) <- PL_Blank;
        link_line
      | _ ->
        link_line)
    else link_line
  in
  (* Connect node column to all parent columns *)
  (match compute_bounds !parent_columns column with
   | Some bounds ->
     (* Horizontal line between outermost ancestors *)
     for i = bounds.min_ancestor + 1 to bounds.max_ancestor - 1 do
       if i <> bounds.target
       then (
         link_line.(i) <- LinkLine.(link_line.(i) lor bounds_horizontal_line bounds i);
         need_link_line := true)
     done;
     (* Merge markers on node column *)
     if bounds.max_parent > column
     then (
       link_line.(column) <- LinkLine.(link_line.(column) lor right_merge_parent);
       need_link_line := true)
     else if bounds.max_ancestor > column
     then (
       link_line.(column) <- LinkLine.(link_line.(column) lor right_merge_ancestor);
       need_link_line := true);
     if bounds.min_parent < column
     then (
       link_line.(column) <- LinkLine.(link_line.(column) lor left_merge_parent);
       need_link_line := true)
     else if bounds.min_ancestor < column
     then (
       link_line.(column) <- LinkLine.(link_line.(column) lor left_merge_ancestor);
       need_link_line := true);
     (* Fork markers on each parent column *)
     List.iter
       (fun (i, p) ->
          pad_lines.(i) <- column_to_pad_line !columns.(i);
          if i < column
          then
            link_line.(i)
            <- LinkLine.(
                 link_line.(i)
                 lor ancestor_to_link_line
                       p
                       ~direct:right_fork_parent
                       ~indirect:right_fork_ancestor)
          else if i = column
          then
            link_line.(i)
            <- LinkLine.(
                 link_line.(i)
                 lor child
                 lor ancestor_to_link_line p ~direct:vert_parent ~indirect:vert_ancestor)
          else
            link_line.(i)
            <- LinkLine.(
                 link_line.(i)
                 lor ancestor_to_link_line
                       p
                       ~direct:left_fork_parent
                       ~indirect:left_fork_ancestor))
       !parent_columns
   | None ->
     ());
  (* Reset columns *)
  columns := columns_reset !columns;
  (* Compute glyph for this node *)
  let glyph =
    if is_elided n
    then P.term
    else if n.working_copy
    then P.Node.working_copy
    else if n.conflict
    then P.Node.conflict
    else if n.immutable
    then P.Node.immutable
    else if n.wip
    then P.Node.wip
    else P.Node.normal
  in
  {
    row_node = n
  ; glyph
  ; message = ""
  ; merge
  ; node_line
  ; link_line = (if !need_link_line then Some link_line else None)
  ; term_line =
      (if visible_elided
       then None
       else if !need_term_line
       then Some term_line
       else None)
  ; pad_lines
  }
;;

(* ============================================================================
   BoxDrawing - glyph selection and string rendering
   ============================================================================ *)

module Glyph = struct
  let space = 0
  let horizontal = 1
  let parent = 2
  let ancestor = 3
  let merge_left = 4
  let merge_right = 5
  let merge_both = 6
  let fork_left = 7
  let fork_right = 8
  let fork_both = 9
  let join_left = 10
  let join_right = 11
  let join_both = 12
  let termination = 13
end

(** 2-character glyph strings matching Rust CURVED_GLYPHS.
    Second character is "─" if horizontal line continues right, " " otherwise. *)
let glyphs =
  [|
     "  " (* space *)
   ; "──" (* horizontal *)
   ; "│ " (* parent *)
   ; "╷ " (* ancestor *)
   ; "╯ " (* merge_left *)
   ; "╰─" (* merge_right *)
   ; "┴─" (* merge_both *)
   ; "╮ " (* fork_left *)
   ; "╭─" (* fork_right *)
   ; "┬─" (* fork_both *)
   ; "┤ " (* join_left *)
   ; "├─" (* join_right *)
   ; "┼─" (* join_both *)
   ; "~ " (* termination *)
  |]
;;

let pad_line_to_glyph = function
  | PL_Parent ->
    Glyph.parent
  | PL_Ancestor ->
    Glyph.ancestor
  | PL_Blank ->
    Glyph.space
;;

let select_link_glyph cur ~merge =
  let open LinkLine in
  if intersects cur horizontal
  then
    if intersects cur child
    then Glyph.join_both
    else if intersects cur any_fork && intersects cur any_merge
    then Glyph.join_both
    else if intersects cur any_fork && intersects cur vert_parent && not merge
    then Glyph.join_both
    else if intersects cur any_fork
    then Glyph.fork_both
    else if intersects cur any_merge
    then Glyph.merge_both
    else Glyph.horizontal
  else if intersects cur vert_parent && not merge
  then (
    let left = intersects cur (left_merge lor left_fork) in
    let right = intersects cur (right_merge lor right_fork) in
    match left, right with
    | true, true ->
      Glyph.join_both
    | true, false ->
      Glyph.join_left
    | false, true ->
      Glyph.join_right
    | false, false ->
      Glyph.parent)
  else if
    intersects cur (vert_parent lor vert_ancestor)
    && not (intersects cur (left_fork lor right_fork))
  then (
    let left = intersects cur left_merge in
    let right = intersects cur right_merge in
    match left, right with
    | true, true ->
      Glyph.join_both
    | true, false ->
      Glyph.join_left
    | false, true ->
      Glyph.join_right
    | false, false ->
      if intersects cur vert_ancestor then Glyph.ancestor else Glyph.parent)
  else if intersects cur left_fork && intersects cur (left_merge lor child)
  then Glyph.join_left
  else if intersects cur right_fork && intersects cur (right_merge lor child)
  then Glyph.join_right
  else if intersects cur left_merge && intersects cur right_merge
  then Glyph.merge_both
  else if intersects cur left_fork && intersects cur right_fork
  then Glyph.fork_both
  else if intersects cur left_fork
  then Glyph.fork_left
  else if intersects cur left_merge
  then Glyph.merge_left
  else if intersects cur right_fork
  then Glyph.fork_right
  else if intersects cur right_merge
  then Glyph.merge_right
  else Glyph.space
;;

let render_row_to_string (row : graph_row) ~extra_pad_line_ref : string =
  let buf = Buffer.create 64 in
  (match !extra_pad_line_ref with
   | Some s ->
     Buffer.add_string buf (String.trim s);
     Buffer.add_char buf '\n';
     extra_pad_line_ref := None
   | None ->
     ());
  Array.iter
    (fun entry ->
       match entry with
       | NL_Node ->
         Buffer.add_utf_8_uchar buf row.glyph;
         Buffer.add_char buf ' '
       | NL_Parent ->
         Buffer.add_string buf glyphs.(Glyph.parent)
       | NL_Ancestor ->
         Buffer.add_string buf glyphs.(Glyph.ancestor)
       | NL_Blank ->
         Buffer.add_string buf glyphs.(Glyph.space))
    row.node_line;
  let node_str = Buffer.contents buf |> String.trim in
  Buffer.reset buf;
  Buffer.add_string buf node_str;
  Buffer.add_char buf '\n';
  (match row.link_line with
   | Some link_row ->
     let link_buf = Buffer.create 64 in
     Array.iter
       (fun cur ->
          let glyph_idx = select_link_glyph cur ~merge:row.merge in
          Buffer.add_string link_buf glyphs.(glyph_idx))
       link_row;
     let link_str = Buffer.contents link_buf |> String.trim in
     Buffer.add_string buf link_str;
     Buffer.add_char buf '\n'
   | None ->
     ());
  let need_extra_pad = ref false in
  (match row.term_line with
   | Some term_row ->
     let term_buf1 = Buffer.create 64 in
     Array.iteri
       (fun i term ->
          if term
          then Buffer.add_string term_buf1 glyphs.(Glyph.parent)
          else (
            let pad_glyph = pad_line_to_glyph row.pad_lines.(i) in
            Buffer.add_string term_buf1 glyphs.(pad_glyph)))
       term_row;
     Buffer.add_string buf (Buffer.contents term_buf1 |> String.trim);
     Buffer.add_char buf '\n';
     let term_buf2 = Buffer.create 64 in
     Array.iteri
       (fun i term ->
          if term
          then Buffer.add_string term_buf2 glyphs.(Glyph.termination)
          else (
            let pad_glyph = pad_line_to_glyph row.pad_lines.(i) in
            Buffer.add_string term_buf2 glyphs.(pad_glyph)))
       term_row;
     Buffer.add_string buf (Buffer.contents term_buf2 |> String.trim);
     Buffer.add_char buf '\n';
     need_extra_pad := true
   | None ->
     ());
  let pad_buf = Buffer.create 64 in
  Array.iter
    (fun entry ->
       let glyph_idx = pad_line_to_glyph entry in
       Buffer.add_string pad_buf glyphs.(glyph_idx))
    row.pad_lines;
  let base_pad_line = Buffer.contents pad_buf in
  (* Visible elided nodes are a one-line summary row. Keeping the old pad line
     below them only adds an empty-looking spacer in the UI. *)
  if !need_extra_pad
  then extra_pad_line_ref := Some base_pad_line;
  Buffer.contents buf
;;

(* ============================================================================
   Public API - render_nodes_to_string
   ============================================================================ *)

let render_nodes_to_string ?(info_rows = fun _ -> 0) (_state : state) (nodes : node list)
  : string
  =
  let columns = ref [||] in
  let visible_node_ids = Hashtbl.create (List.length nodes) in
  List.iter (fun node -> Hashtbl.replace visible_node_ids node.commit_id ()) nodes;
  let extra_pad_line_ref = ref None in
  let buf = Buffer.create 256 in
  List.iter
    (fun n ->
       let row = next_row ~columns ~visible_node_ids n in
       let row_str = render_row_to_string row ~extra_pad_line_ref in
       Buffer.add_string buf row_str;
       let extra_rows = info_rows n in
       for _ = 1 to extra_rows do
         let pad_buf = Buffer.create 64 in
         Array.iter
           (fun col ->
              let glyph_idx = pad_line_to_glyph (column_to_pad_line col) in
              Buffer.add_string pad_buf glyphs.(glyph_idx))
           !columns;
         Buffer.add_string buf (Buffer.contents pad_buf |> String.trim);
         Buffer.add_char buf '\n'
       done)
    nodes;
  (* Final extra pad line if pending *)
  (match !extra_pad_line_ref with
   | Some s ->
     Buffer.add_string buf (String.trim s);
     Buffer.add_char buf '\n'
   | None ->
     ());
  Buffer.contents buf
;;

(* ============================================================================
   Public API - render_nodes_structured
   ============================================================================ *)

let classify_row_type (line : string) : row_type =
  let contains_str s substr =
    try
      let _ = Str.search_forward (Str.regexp_string substr) s 0 in
      true
    with
    | Not_found ->
      false
  in
  let has_node_glyph =
    contains_str line "○"
    || contains_str line "@"
    || contains_str line "◌"
    || contains_str line "◆"
    || contains_str line "×"
  in
  let has_term = contains_str line "~" in
  let has_merge_fork =
    contains_str line "├"
    || contains_str line "╮"
    || contains_str line "╯"
    || contains_str line "╰"
    || contains_str line "┬"
    || contains_str line "┴"
    || contains_str line "┼"
  in
  if has_node_glyph
  then NodeRow
  else if has_term
  then TermRow
  else if has_merge_fork
  then LinkRow
  else PadRow
;;

(** Trim trailing whitespace from a graph image to match its string form. *)
let trim_graph_image ~graph_chars (img : Notty.image) : Notty.image =
  let open Notty in
  let trimmed_width = I.width (I.string A.empty graph_chars) in
  let width = I.width img in
  if width > trimmed_width then I.hcrop 0 (width - trimmed_width) img else img
;;

let node_row_type (node : node) graph_chars =
  if is_elided node then NodeRow else classify_row_type graph_chars
;;

(** Render nodes to structured output for UI integration *)
let render_nodes_structured
      ?(info_lines = fun _ -> 0)
      ?(node_attr = fun _ -> Notty.A.empty)
      (_state : state)
      (nodes : node list) : graph_row_output list
  =
  let columns = ref [||] in
  let visible_node_ids = Hashtbl.create (List.length nodes) in
  List.iter (fun node -> Hashtbl.replace visible_node_ids node.commit_id ()) nodes;
  let extra_pad_line_ref = ref None in
  let result = ref [] in
  List.iter
    (fun n ->
       let row = next_row ~columns ~visible_node_ids n in
        (match !extra_pad_line_ref with
         | Some (s, img) ->
           let trimmed = String.trim s in
          let trimmed_img = trim_graph_image ~graph_chars:trimmed img in
          result
           := {
                graph_chars = trimmed
              ; graph_image = trimmed_img
              ; node = n
              ; row_type = classify_row_type trimmed
              }
              :: !result;
           extra_pad_line_ref := None
        | None ->
          ());
       let node_buf = Buffer.create 64 in
       let node_images = ref [] in
       Array.iter
         (fun entry ->
            match entry with
            | NL_Node ->
              Buffer.add_utf_8_uchar node_buf row.glyph;
              Buffer.add_char node_buf ' ';
              let glyph_img = Notty.I.uchar (node_attr row.row_node) row.glyph 1 1 in
              let space_img = Notty.I.string Notty.A.empty " " in
              node_images := Notty.I.hcat [ glyph_img; space_img ] :: !node_images
            | NL_Parent ->
              Buffer.add_string node_buf glyphs.(Glyph.parent);
              node_images
              := Notty.I.string Notty.A.empty glyphs.(Glyph.parent) :: !node_images
            | NL_Ancestor ->
              Buffer.add_string node_buf glyphs.(Glyph.ancestor);
              node_images
              := Notty.I.string Notty.A.empty glyphs.(Glyph.ancestor) :: !node_images
            | NL_Blank ->
              Buffer.add_string node_buf glyphs.(Glyph.space);
              node_images
              := Notty.I.string Notty.A.empty glyphs.(Glyph.space) :: !node_images)
         row.node_line;
       let node_str = Buffer.contents node_buf |> String.trim in
       let node_img = !node_images |> List.rev |> Notty.I.hcat in
       let node_img = trim_graph_image ~graph_chars:node_str node_img in
        result
        := {
             graph_chars = node_str
           ; graph_image = node_img
           ; node = n
           ; row_type = node_row_type n node_str
           }
           :: !result;
       (match row.link_line with
        | Some link_row ->
          let link_buf = Buffer.create 64 in
          let link_images = ref [] in
          Array.iter
            (fun cur ->
               let glyph_idx = select_link_glyph cur ~merge:row.merge in
               Buffer.add_string link_buf glyphs.(glyph_idx);
               link_images
               := Notty.I.string Notty.A.empty glyphs.(glyph_idx) :: !link_images)
            link_row;
          let link_str = Buffer.contents link_buf |> String.trim in
          let link_img = !link_images |> List.rev |> Notty.I.hcat in
          let link_img = trim_graph_image ~graph_chars:link_str link_img in
          result
          := {
               graph_chars = link_str
             ; graph_image = link_img
             ; node = n
             ; row_type = classify_row_type link_str
             }
             :: !result
        | None ->
          ());
       let need_extra_pad = ref false in
       (match row.term_line with
        | Some term_row ->
          let term_buf1 = Buffer.create 64 in
          let term_images1 = ref [] in
          Array.iteri
            (fun i term ->
               if term
               then (
                 Buffer.add_string term_buf1 glyphs.(Glyph.parent);
                 term_images1
                 := Notty.I.string Notty.A.empty glyphs.(Glyph.parent) :: !term_images1)
               else (
                 let pad_glyph = pad_line_to_glyph row.pad_lines.(i) in
                 Buffer.add_string term_buf1 glyphs.(pad_glyph);
                 term_images1
                 := Notty.I.string Notty.A.empty glyphs.(pad_glyph) :: !term_images1))
            term_row;
          let term_str1 = Buffer.contents term_buf1 |> String.trim in
          let term_img1 = !term_images1 |> List.rev |> Notty.I.hcat in
          let term_img1 = trim_graph_image ~graph_chars:term_str1 term_img1 in
          result
          := {
               graph_chars = term_str1
             ; graph_image = term_img1
             ; node = n
             ; row_type = classify_row_type term_str1
             }
             :: !result;
          let term_buf2 = Buffer.create 64 in
          let term_images2 = ref [] in
          Array.iteri
            (fun i term ->
               if term
               then (
                 Buffer.add_string term_buf2 glyphs.(Glyph.termination);
                 term_images2
                 := Notty.I.string Notty.A.empty glyphs.(Glyph.termination)
                    :: !term_images2)
               else (
                 let pad_glyph = pad_line_to_glyph row.pad_lines.(i) in
                 Buffer.add_string term_buf2 glyphs.(pad_glyph);
                 term_images2
                 := Notty.I.string Notty.A.empty glyphs.(pad_glyph) :: !term_images2))
            term_row;
          let term_str2 = Buffer.contents term_buf2 |> String.trim in
          let term_img2 = !term_images2 |> List.rev |> Notty.I.hcat in
          let term_img2 = trim_graph_image ~graph_chars:term_str2 term_img2 in
          result
          := {
               graph_chars = term_str2
             ; graph_image = term_img2
             ; node = n
             ; row_type = classify_row_type term_str2
             }
             :: !result;
          need_extra_pad := true
        | None ->
          ());
        let pad_buf = Buffer.create 64 in
        let pad_images = ref [] in
        Array.iter
          (fun entry ->
            let glyph_idx = pad_line_to_glyph entry in
             Buffer.add_string pad_buf glyphs.(glyph_idx);
             pad_images := Notty.I.string Notty.A.empty glyphs.(glyph_idx) :: !pad_images)
          row.pad_lines;
        let base_pad_line = Buffer.contents pad_buf in
        let base_pad_img = !pad_images |> List.rev |> Notty.I.hcat in
        (* Visible elided nodes intentionally stay on a single line. Emitting the
           following pad row would produce a blank spacer under `~ (elided revisions)`. *)
        if !need_extra_pad
        then extra_pad_line_ref := Some (base_pad_line, base_pad_img);
        let extra_rows = info_lines n in
       for _ = 1 to extra_rows do
         let info_pad_buf = Buffer.create 64 in
         let info_pad_images = ref [] in
         Array.iter
           (fun col ->
              let glyph_idx = pad_line_to_glyph (column_to_pad_line col) in
              Buffer.add_string info_pad_buf glyphs.(glyph_idx);
              info_pad_images
              := Notty.I.string Notty.A.empty glyphs.(glyph_idx) :: !info_pad_images)
           !columns;
         let info_pad_str = Buffer.contents info_pad_buf |> String.trim in
         let info_pad_img = !info_pad_images |> List.rev |> Notty.I.hcat in
         let info_pad_img = trim_graph_image ~graph_chars:info_pad_str info_pad_img in
         result
         := {
              graph_chars = info_pad_str
            ; graph_image = info_pad_img
            ; node = n
            ; row_type = classify_row_type info_pad_str
            }
            :: !result
       done)
    nodes;
  (match !extra_pad_line_ref with
   | Some (s, img) ->
     let trimmed = String.trim s in
     let trimmed_img = trim_graph_image ~graph_chars:trimmed img in
     let last_node = List.hd (List.rev nodes) in
     result
     := {
          graph_chars = trimmed
        ; graph_image = trimmed_img
        ; node = last_node
        ; row_type = classify_row_type trimmed
        }
        :: !result
   | None ->
     ());
  List.rev !result
;;

(* ============================================================================
   Public API - render_nodes_to_ui (Notty output)
   ============================================================================ *)

let render_nodes_to_ui ?(info_rows = fun _ -> 0) (state : state) (nodes : node list) :
  Notty.image
  =
  let str = render_nodes_to_string ~info_rows state nodes in
  let lines = String.split_on_char '\n' str in
  let images = List.map (fun line -> Notty.I.string Notty.A.empty line) lines in
  Notty.I.vcat images
;;
