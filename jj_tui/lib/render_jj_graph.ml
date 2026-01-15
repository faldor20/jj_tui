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
  let v = Util.make_uchar "│"
  let vr = Util.make_uchar "├"
  let vl = Util.make_uchar "┤"
  let t = Util.make_uchar "┬"
  let cross = Util.make_uchar "┼"
  let h = Util.make_uchar "─"
  let b = Util.make_uchar "┴"

  (* elbow down right *)
  let edr = Util.make_uchar "╮"
  let eur = Util.make_uchar "╯"
  let edl = Util.make_uchar "╭"
  let eul = Util.make_uchar "╰"
  let sp = Util.make_uchar " "
  let ancestor = Util.make_uchar "╷"
  let term = Util.make_uchar "~"

  module Node = struct
    let normal = Util.make_uchar "○"
    let working_copy = Util.make_uchar "@"
    let wip = Util.make_uchar "◌"
    let immutable = Util.make_uchar "◆"
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
  ; author_email : string
  ; author_timestamp : string
  ; empty : bool
  ; hidden : bool
  ; divergent : bool
  ; is_preview : bool
  ; change_id_prefix : string
  ; change_id_rest : string
  ; commit_id_prefix : string
  ; commit_id_rest : string
}

(** Special marker for elided nodes *)
let elided_marker = "~ELIDED~"

(** Create a special node representing an elided section *)
let make_elided_node () : node =
  {
    parents = []
  ; creation_time = Int64.zero
  ; working_copy = false
  ; immutable = false
  ; wip = false
  ; change_id = elided_marker
  ; commit_id = elided_marker
  ; description = "(elided revisions)"
  ; bookmarks = []
  ; author_email = ""
  ; author_timestamp = ""
  ; empty = false
  ; hidden = true
  ; divergent = false
  ; is_preview = false
  ; change_id_prefix = ""
  ; change_id_rest = ""
  ; commit_id_prefix = ""
  ; commit_id_rest = ""
  }
;;

(** Check if a node represents an elided section *)
let is_elided (n : node) : bool = n.commit_id = elided_marker

(* ============================================================================
   Preview Node Support
   ============================================================================ *)

(** Create a preview node with a label.
    Preview nodes are used to visualize where commits would land during
    rebase/move operations. *)
let make_preview_node ~label ?description ?target_commit_id () : node =
  let description = Option.value description ~default:label in
  {
    parents = []
  ; creation_time = Int64.zero
  ; working_copy = false
  ; immutable = false
  ; wip = false
  ; change_id = Printf.sprintf "preview:%s" label
  ; commit_id =
      (match target_commit_id with
       | Some id ->
         Printf.sprintf "preview:%s:%s" label id
       | None ->
         Printf.sprintf "preview:%s" label)
  ; description
  ; bookmarks = []
  ; author_email = ""
  ; author_timestamp = ""
  ; empty = false
  ; hidden = false
  ; divergent = false
  ; is_preview = true
  ; change_id_prefix = ""
  ; change_id_rest = ""
  ; commit_id_prefix = ""
  ; commit_id_rest = ""
  }
;;

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

let resolve_revs (nodes : node list) (revs : string list) : string list =
  revs
  |> List.concat_map (fun rev ->
    nodes
    |> List.filter (fun n -> node_matches_rev n rev)
    |> List.map (fun n -> n.commit_id))
  |> List.sort_uniq String.compare
;;

let build_parent_map (nodes : node list) =
  let map = Hashtbl.create (List.length nodes) in
  List.iter
    (fun n ->
       Hashtbl.replace map n.commit_id (List.map (fun p -> p.commit_id) n.parents))
    nodes;
  map
;;

let build_children_map parent_map =
  let children = Hashtbl.create (Hashtbl.length parent_map) in
  Hashtbl.iter
    (fun child_id parent_ids ->
       List.iter
         (fun parent_id ->
            let existing = Option.value (Hashtbl.find_opt children parent_id) ~default:[] in
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

let expand_preview_sources
      ~(mode : preview_source_mode)
      ~(sources : string list)
      ~(targets : string list)
      (nodes : node list) : string list
  =
  if sources = [] then []
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

let preview_description sources =
  match sources with
  | [ rev ] ->
    Printf.sprintf "preview: %s" rev
  | _ ->
    Printf.sprintf "preview: %d commits" (List.length sources)
;;

let apply_rebase_preview_multi
      ~(mode : preview_mode)
      ~(sources : string list)
      ~(targets : string list)
      (nodes : node list) : node list * string option
  =
  let parent_map_all = build_parent_map nodes in
  let children_map_all = build_children_map parent_map_all in
  let ancestors_of = build_ancestors parent_map_all in
  let source_ids = resolve_revs nodes sources in
  let target_ids = resolve_revs nodes targets in
  let source_set = StringSet.of_list source_ids in
  let invalid = ref None in
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
  List.iter (fun target_id ->
    if invalid_target target_id
    then invalid := Some "Preview blocked: cycle detected") target_ids;
  if !invalid <> None
  then nodes, !invalid
  else (
    let nodes_filtered =
      nodes |> List.filter (fun n -> not (StringSet.mem n.commit_id source_set))
    in
    let parent_map = Hashtbl.create (List.length nodes_filtered) in
    List.iter
      (fun n ->
         let parents =
           n.parents
           |> List.map (fun p -> p.commit_id)
           |> List.filter (fun id -> not (StringSet.mem id source_set))
         in
         Hashtbl.replace parent_map n.commit_id parents)
      nodes_filtered;
    let children_map = build_children_map parent_map in
    let base_nodes = Hashtbl.create (List.length nodes_filtered) in
    List.iter (fun n -> Hashtbl.replace base_nodes n.commit_id n) nodes_filtered;
    let heads =
      source_ids
      |> List.filter (fun id ->
        let children =
          Option.value (Hashtbl.find_opt children_map_all id) ~default:[]
        in
        not (List.exists (fun child -> StringSet.mem child source_set) children))
    in
    let source_order =
      nodes
      |> List.filter (fun n -> StringSet.mem n.commit_id source_set)
      |> List.map (fun n -> n.commit_id)
    in
    let preview_map = Hashtbl.create (List.length source_ids) in
    List.iter
      (fun source_id ->
         let preview_id = Printf.sprintf "preview:%s" source_id in
         let source_node = List.find (fun n -> n.commit_id = source_id) nodes in
         let preview_node =
           {
             source_node with
             commit_id = preview_id
           ; change_id = preview_id
           ; description = "preview: " ^ source_node.description
           ; is_preview = true
           }
         in
         Hashtbl.replace base_nodes preview_id preview_node;
         Hashtbl.replace preview_map source_id preview_id)
      source_ids;
    let preview_parent_ids source_id =
      let source_node = List.find (fun n -> n.commit_id = source_id) nodes in
      source_node.parents
      |> List.map (fun p -> p.commit_id)
      |> List.filter (fun id -> StringSet.mem id source_set)
      |> List.filter_map (fun id -> Hashtbl.find_opt preview_map id)
    in
    List.iter
      (fun source_id ->
         let preview_id = Hashtbl.find preview_map source_id in
         let parent_ids = preview_parent_ids source_id in
         Hashtbl.replace parent_map preview_id parent_ids)
      source_ids;
    let root_ids =
      source_ids
      |> List.filter (fun id ->
        let source_node = List.find (fun n -> n.commit_id = id) nodes in
        not (List.exists (fun p -> StringSet.mem p.commit_id source_set) source_node.parents))
    in
    let root_preview_ids = root_ids |> List.map (fun id -> Hashtbl.find preview_map id) in
    let head_preview_ids = heads |> List.map (fun id -> Hashtbl.find preview_map id) in
    let target_parent_union =
      target_ids
      |> List.concat_map (fun target_id ->
        Option.value (Hashtbl.find_opt parent_map target_id) ~default:[])
      |> List.sort_uniq String.compare
    in
    (match mode with
     | `Insert_before ->
       List.iter
         (fun preview_id -> Hashtbl.replace parent_map preview_id target_parent_union)
         root_preview_ids;
       List.iter
         (fun target_id -> Hashtbl.replace parent_map target_id head_preview_ids)
         target_ids
     | `Insert_after ->
       List.iter
         (fun preview_id -> Hashtbl.replace parent_map preview_id target_ids)
         root_preview_ids;
       List.iter
         (fun target_id ->
            let children =
              Option.value (Hashtbl.find_opt children_map target_id) ~default:[]
            in
            List.iter
              (fun child_id ->
                 let child_parents =
                   Option.value (Hashtbl.find_opt parent_map child_id) ~default:[]
                 in
                 let without_target =
                   List.filter (fun id -> id <> target_id) child_parents
                 in
                 Hashtbl.replace parent_map child_id (without_target @ head_preview_ids))
              children)
         target_ids
     | `Add_after ->
       List.iter
         (fun preview_id -> Hashtbl.replace parent_map preview_id target_ids)
         root_preview_ids);
    let preview_before = match mode with `Insert_after | `Add_after -> true | _ -> false in
    let preview_after = match mode with `Insert_before -> true | _ -> false in
    let first_target_id =
      List.find_map
        (fun n -> if List.mem n.commit_id target_ids then Some n.commit_id else None)
        nodes_filtered
      |> Option.value ~default:(List.hd target_ids)
    in
    let last_target_id =
      nodes_filtered
      |> List.fold_left
           (fun acc n ->
              if List.mem n.commit_id target_ids then Some n.commit_id else acc)
           None
      |> Option.value ~default:(List.hd target_ids)
    in
    let insertion_target_id =
      if preview_after then last_target_id else first_target_id
    in
    let inserted = ref false in
    let ordered_ids_rev =
      List.fold_left
        (fun acc n ->
           let id = n.commit_id in
           if (not !inserted) && id = insertion_target_id
           then (
             inserted := true;
             let preview_ids =
               source_order |> List.map (fun source_id -> Hashtbl.find preview_map source_id)
             in
             if preview_before
             then id :: (List.rev_append preview_ids acc)
             else if preview_after
             then (List.rev_append preview_ids (id :: acc))
             else id :: acc)
           else id :: acc)
        []
        nodes_filtered
    in
    let ordered_ids = List.rev ordered_ids_rev in
    let final_nodes = Hashtbl.create (List.length ordered_ids) in
    let rec build_node id =
      match Hashtbl.find_opt final_nodes id with
      | Some node ->
        node
      | None ->
        let base = Hashtbl.find base_nodes id in
        let parent_ids = Option.value (Hashtbl.find_opt parent_map id) ~default:[] in
        let parents = List.map build_node parent_ids in
        let node = { base with parents } in
        Hashtbl.replace final_nodes id node;
        node
    in
    let nodes = List.map build_node ordered_ids in
    nodes, !invalid)
;;

let apply_rebase_preview
      ~(mode : preview_mode)
      ~(sources : string list)
      ~(targets : string list)
      (nodes : node list) : node list * string option
  =
  if sources = [] || targets = [] then nodes, None
  else (
    let source_ids = resolve_revs nodes sources in
    let target_ids = resolve_revs nodes targets in
    if source_ids = [] || target_ids = []
    then nodes, None
    else (
      if List.length source_ids > 1
      then apply_rebase_preview_multi ~mode ~sources ~targets nodes
      else (
      let parent_map_all = build_parent_map nodes in
      let ancestors_of = build_ancestors parent_map_all in
      let removed_set = StringSet.of_list source_ids in
      let nodes_filtered =
        nodes |> List.filter (fun n -> not (StringSet.mem n.commit_id removed_set))
      in
      let parent_map = Hashtbl.create (List.length nodes_filtered) in
      List.iter
        (fun n ->
           let parents =
             n.parents
             |> List.map (fun p -> p.commit_id)
             |> List.filter (fun id -> not (StringSet.mem id removed_set))
           in
           Hashtbl.replace parent_map n.commit_id parents)
        nodes_filtered;
      let children_map = build_children_map parent_map in
      let invalid = ref None in
      let preview_by_target = Hashtbl.create (List.length target_ids) in
      let base_nodes =
        Hashtbl.create (List.length nodes_filtered + List.length target_ids)
      in
      List.iter (fun n -> Hashtbl.replace base_nodes n.commit_id n) nodes_filtered;
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
      List.iter
        (fun target_id ->
           if invalid_target target_id
           then invalid := Some "Preview blocked: cycle detected")
        target_ids;
      if !invalid <> None
      then nodes_filtered, !invalid
      else (
        let () =
          if List.length target_ids > 1
          then (
            let preview_id = "preview:multi" in
            let description = preview_description sources in
            let preview_node = make_preview_node ~label:"preview" ~description () in
            Hashtbl.replace base_nodes preview_id preview_node;
            let target_parent_union =
              target_ids
              |> List.concat_map (fun target_id ->
                Option.value (Hashtbl.find_opt parent_map target_id) ~default:[])
              |> List.sort_uniq String.compare
            in
            (match mode with
             | `Insert_before ->
               Hashtbl.replace parent_map preview_id target_parent_union;
               List.iter
                 (fun target_id -> Hashtbl.replace parent_map target_id [ preview_id ])
                 target_ids
             | `Insert_after ->
               Hashtbl.replace parent_map preview_id target_ids;
               List.iter
                 (fun target_id ->
                    let children =
                      Option.value (Hashtbl.find_opt children_map target_id) ~default:[]
                    in
                    List.iter
                      (fun child_id ->
                         let child_parents =
                           Option.value (Hashtbl.find_opt parent_map child_id) ~default:[]
                         in
                         let without_target =
                           List.filter (fun id -> id <> target_id) child_parents
                         in
                         Hashtbl.replace parent_map child_id (without_target @ [ preview_id ]))
                      children)
                 target_ids
             | `Add_after ->
               Hashtbl.replace parent_map preview_id target_ids);
            let first_target_id =
              List.find_map
                (fun n ->
                   if List.mem n.commit_id target_ids then Some n.commit_id else None)
                nodes_filtered
              |> Option.value ~default:(List.hd target_ids)
            in
            let last_target_id =
              nodes_filtered
              |> List.fold_left
                   (fun acc n ->
                      if List.mem n.commit_id target_ids then Some n.commit_id else acc)
                   None
              |> Option.value ~default:(List.hd target_ids)
            in
            let insertion_target_id =
              match mode with
              | `Insert_before ->
                last_target_id
              | `Insert_after | `Add_after ->
                first_target_id
            in
            Hashtbl.replace preview_by_target insertion_target_id preview_id)
          else (
            let add_preview_for_target target_id =
              if not (Hashtbl.mem parent_map target_id)
              then ()
              else (
                let preview_id = Printf.sprintf "preview:%s" target_id in
                if not (Hashtbl.mem preview_by_target target_id)
                then (
                  let label = "preview" in
                  let description = preview_description sources in
                  let preview_node =
                    make_preview_node ~label ~description ~target_commit_id:target_id ()
                  in
                  Hashtbl.replace base_nodes preview_id preview_node;
                  Hashtbl.replace preview_by_target target_id preview_id;
                  match mode with
                  | `Insert_before ->
                    let parents =
                      Option.value (Hashtbl.find_opt parent_map target_id) ~default:[]
                    in
                    Hashtbl.replace parent_map preview_id parents;
                    Hashtbl.replace parent_map target_id [ preview_id ]
                  | `Insert_after ->
                    Hashtbl.replace parent_map preview_id [ target_id ];
                    let children =
                      Option.value (Hashtbl.find_opt children_map target_id) ~default:[]
                    in
                    List.iter
                      (fun child_id ->
                         let child_parents =
                           Option.value (Hashtbl.find_opt parent_map child_id) ~default:[]
                         in
                         let updated =
                           List.map
                             (fun parent_id ->
                                if parent_id = target_id then preview_id else parent_id)
                             child_parents
                         in
                         Hashtbl.replace parent_map child_id updated)
                      children
                  | `Add_after ->
                    Hashtbl.replace parent_map preview_id [ target_id ]))
            in
            List.iter add_preview_for_target target_ids)
        in
      (* Order must follow topological log order: children appear before parents. *)
      let preview_before = match mode with `Insert_after | `Add_after -> true | _ -> false in
      let preview_after = match mode with `Insert_before -> true | _ -> false in
      let ordered_ids_rev =
        List.fold_left
          (fun acc n ->
             let id = n.commit_id in
             match Hashtbl.find_opt preview_by_target id with
             | Some preview_id when preview_before ->
               id :: preview_id :: acc
             | Some preview_id when preview_after ->
               preview_id :: id :: acc
             | Some _ ->
               id :: acc
             | None ->
               id :: acc)
          []
          nodes_filtered
      in
      let ordered_ids = List.rev ordered_ids_rev in
      let final_nodes = Hashtbl.create (List.length ordered_ids) in
      let rec build_node id =
        match Hashtbl.find_opt final_nodes id with
        | Some node ->
          node
        | None ->
          let base = Hashtbl.find base_nodes id in
          let parent_ids = Option.value (Hashtbl.find_opt parent_map id) ~default:[] in
          let parents = List.map build_node parent_ids in
          let node = { base with parents } in
          Hashtbl.replace final_nodes id node;
          node
      in
      let nodes = List.map build_node ordered_ids in
      nodes, !invalid))))
;;

(** Insert a preview node after the specified commit.
    The preview node will be inserted as a child of the target commit. *)
let insert_preview_after ~nodes ~after_commit_id ~preview : node list =
  let rec insert acc = function
    | [] ->
      List.rev acc
    | node :: rest when node.commit_id = after_commit_id ->
      (* Found the target node - insert preview after it *)
      let preview_with_parent = { preview with parents = [ node ] } in
      List.rev_append acc (node :: preview_with_parent :: rest)
    | node :: rest ->
      insert (node :: acc) rest
  in
  insert [] nodes
;;

(** Insert a preview node before the specified commit.
    The preview node will be inserted as a parent of the target commit,
    and will inherit the target's parents. *)
let insert_preview_before ~nodes ~before_commit_id ~preview : node list =
  let rec insert acc = function
    | [] ->
      List.rev acc
    | node :: rest when node.commit_id = before_commit_id ->
      (* Found the target node - insert preview before it *)
      let preview_with_parents = { preview with parents = node.parents } in
      let node_with_preview_parent = { node with parents = [ preview_with_parents ] } in
      List.rev_append acc (preview_with_parents :: node_with_preview_parent :: rest)
    | node :: rest ->
      insert (node :: acc) rest
  in
  insert [] nodes
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

let next_row ~(columns : column array ref) (n : node) : graph_row =
  let parents =
    n.parents |> List.map (fun p -> if is_elided p then A_Anonymous else A_Parent p)
    (* Elided parents are treated as anonymous to trigger termination lines *)
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
    if n.working_copy
    then P.Node.working_copy
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
  ; term_line = (if !need_term_line then Some term_line else None)
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
  if !need_extra_pad then extra_pad_line_ref := Some base_pad_line;
  Buffer.contents buf
;;

(* ============================================================================
   Public API - render_nodes_to_string
   ============================================================================ *)

let render_nodes_to_string ?(info_rows = fun _ -> 0) (_state : state) (nodes : node list)
  : string
  =
  let columns = ref [||] in
  let extra_pad_line_ref = ref None in
  let buf = Buffer.create 256 in
  List.iter
    (fun n ->
       let row = next_row ~columns n in
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

(** Render nodes to structured output for UI integration *)
let render_nodes_structured
      ?(info_lines = fun _ -> 0)
      ?(node_attr = fun _ -> Notty.A.empty)
      (_state : state)
      (nodes : node list) : graph_row_output list
  =
  let columns = ref [||] in
  let extra_pad_line_ref = ref None in
  let result = ref [] in
  List.iter
    (fun n ->
       let row = next_row ~columns n in
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
          ; row_type = classify_row_type node_str
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
                 := Notty.I.string Notty.A.empty glyphs.(Glyph.termination) :: !term_images2)
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
       if !need_extra_pad then extra_pad_line_ref := Some (base_pad_line, base_pad_img);
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
