module Make (Mutex : Mutex_backend.MUTEX) = struct
  let log_src = Logs.Src.create "lwd.impl" ~doc:"Lwd implementation"
  module Log = (val Logs.src_log log_src : Logs.LOG)

  (** Create-only version of [Obj.t] *)
  module Any : sig
    type t
    val any : 'a -> t
  end = struct
    type t = Obj.t
    let any = Obj.repr
  end
  
  type 'a eval =
    | Eval_none
    | Eval_progress
    | Eval_some of 'a
    | Eval_invalid_next
  
  type 'a t_ =
    | Pure of 'a
    | Operator : {
        mutex : Mutex.t;
        mutable value : 'a eval; (* cached value *)
        mutable trace : trace; (* list of parents this can invalidate *)
        mutable trace_idx : trace_idx; (* list of direct children that can invalidate this *)
        desc: 'a desc;
      } -> 'a t_
    | Root : {
        mutex : Mutex.t;
        mutable value : 'a eval; (* cached value *)
        mutable trace_idx : trace_idx; (* list of direct children that can invalidate this *)
        mutable on_invalidate : 'a -> unit;
        mutable acquired : bool;
        child : 'a t_;
      } -> 'a t_
  
  and _ desc =
    | Map  : 'a t_ * ('a -> 'b) -> 'b desc
    | Map2 : 'a t_ * 'b t_ * ('a -> 'b -> 'c) -> 'c desc
    | Pair : 'a t_ * 'b t_ -> ('a * 'b) desc
    | App  : ('a -> 'b) t_ * 'a t_ -> 'b desc
    | Join : { child : 'a t_ t_; mutable intermediate : 'a t_ option } -> 'a desc
    | Var  : { mutable binding : 'a } -> 'a desc
    | Prim : { acquire : 'a t -> 'a;
               release : 'a t -> 'a -> unit } -> 'a desc
    | Fix : { doc : 'a t_; wrt : _ t_ } -> 'a desc
  
  (* a set of (active) parents for a ['a t], used during invalidation *)
  and trace =
    | T0
    | T1 : _ t_ -> trace
    | T2 : _ t_ * _ t_ -> trace
    | T3 : _ t_ * _ t_ * _ t_ -> trace
    | T4 : _ t_ * _ t_ * _ t_ * _ t_ -> trace
    | Tn : { mutable active : int; mutable count : int;
             mutable entries : Any.t t_ array } -> trace
  
  (* a set of direct children for a composite document *)
  and trace_idx =
    | I0
    | I1 : { mutable idx : int ;
             obj : 'a t_;
             mutable next : trace_idx } -> trace_idx
  
  (* The type system cannot see that t is covariant in its parameter.
     Use the Force to convince it. *)
  and +'a t
  external inj : 'a t_ -> 'a t = "%identity"
  external prj : 'a t -> 'a t_ = "%identity"
  external prj2 : 'a t t -> 'a t_ t_ = "%identity"
  
  (* Basic combinators *)
  let return x = inj (Pure x)
  let pure x = inj (Pure x)
  
  let is_pure x = match prj x with
    | Pure x -> Some x
    | _ -> None
  
  let dummy = Pure (Any.any ())
  
  let operator desc =
    Operator { value = Eval_none; trace = T0; desc; trace_idx = I0 ;mutex= Mutex.create () }
  
  let map x ~f = inj (
      match prj x with
      | Pure vx -> Pure (f vx)
      | x -> operator (Map (x, f))
    )
  
  let map2 x y ~f = inj (
      match prj x, prj y with
      | Pure vx, Pure vy -> Pure (f vx vy)
      | x, y -> operator (Map2 (x, y, f))
    )
  
  
  let pair x y = inj (
      match prj x, prj y with
      | Pure vx, Pure vy -> Pure (vx, vy)
      | x, y -> operator (Pair (x, y))
    )
  
  let app f x = inj (
      match prj f, prj x with
      | Pure vf, Pure vx -> Pure (vf vx)
      | f, x -> operator (App (f, x))
    )
  
  let join child = inj (
      match prj2 child with
      | Pure v -> v
      | child -> operator (Join { child; intermediate = None })
    )
  
  let bind x ~f = join (map ~f x)
  
  let pp_eval_status fmt eval =
    match eval with
    | Eval_none -> Format.fprintf fmt "None"
    | Eval_progress -> Format.fprintf fmt "Progress"
    | Eval_some _ -> Format.fprintf fmt "Some"
    | Eval_invalid_next -> Format.fprintf fmt "Invalid_next"
  
  (* Management of trace indices *)
  
  let addr oc obj =
    Printf.fprintf oc "0x%08x" (Obj.magic obj : int)
  
  let pp_addr fmt obj =
    Format.fprintf fmt "0x%08x" (Obj.magic obj : int)
  
  external t_equal : _ t_ -> _ t_ -> bool = "%eq"
  external obj_t : 'a t_ -> Any.t t_ = "%identity"
  
  let rec dump_trace_format : type a. Format.formatter -> a t_ -> unit =
    fun fmt obj ->
    match obj with
    | Pure _ -> Format.fprintf fmt "%a: Pure _@." pp_addr obj
    | Operator t ->
      Format.fprintf fmt "%a: Operator _ -> %a@." pp_addr obj dump_trace_aux
        t.trace;
      begin
        match t.trace with
        | T0 -> ()
        | T1 a -> dump_trace_format fmt a
        | T2 (a, b) ->
          dump_trace_format fmt a;
          dump_trace_format fmt b
        | T3 (a, b, c) ->
          dump_trace_format fmt a;
          dump_trace_format fmt b;
          dump_trace_format fmt c
        | T4 (a, b, c, d) ->
          dump_trace_format fmt a;
          dump_trace_format fmt b;
          dump_trace_format fmt c;
          dump_trace_format fmt d
        | Tn t -> Array.iter (dump_trace_format fmt) t.entries
      end
    | Root t ->   
      Format.fprintf fmt "%a: Root _@." pp_addr obj

  and dump_trace_aux : type a. Format.formatter -> trace -> unit =
   fun fmt -> function
    | T0 -> Format.fprintf fmt "T0"
    | T1 a -> Format.fprintf fmt "T1 %a" pp_addr a
    | T2 (a, b) -> Format.fprintf fmt "T2 (%a, %a)" pp_addr a pp_addr b
    | T3 (a, b, c) ->
      Format.fprintf fmt "T3 (%a, %a, %a)" pp_addr a pp_addr b pp_addr c
    | T4 (a, b, c, d) ->
      Format.fprintf fmt "T4 (%a, %a, %a, %a)" pp_addr a pp_addr b pp_addr c
        pp_addr d
    | Tn t ->
      Format.fprintf fmt "Tn {active = %d; count = %d; entries = " t.active
        t.count;
      Array.iter (Format.fprintf fmt "(%a)" pp_addr) t.entries;
      Format.fprintf fmt "}"
  
  let dump_trace x = dump_trace_format Format.err_formatter (obj_t (prj x))
  
  let to_mermaid (type a) ?(max_nodes=100) (root : a t_) : string =
    let buf = Buffer.create 1024 in
    Buffer.add_string buf "graph TD;\n";
    let visited : (string, unit) Hashtbl.t = Hashtbl.create 16 in

    let get_id : type a. a t_ -> string = fun node -> Format.asprintf "%a" pp_addr node in

    let get_eval_status_str (type a) (eval : a eval) : string =
      match eval with
      | Eval_none -> "None"
      | Eval_progress -> "Progress"
      | Eval_some _ -> "Some"
      | Eval_invalid_next -> "Invalid_next"
    in

   (* Using breadth-first traversal with a queue to prevent stack overflows on deep graphs. *)
   let q : Any.t t_ Queue.t = Queue.create () in
   Queue.add (obj_t ( root)) q;
   let nodes_processed = ref 0 in

   let rec process_queue () =
     if not (Queue.is_empty q) && !nodes_processed < max_nodes then (
       let node = Queue.take q in
      let node_id_str = get_id node in
       if not (Hashtbl.mem visited node_id_str) then (
         Hashtbl.add visited node_id_str ();
         incr nodes_processed;
         let node_id_str = get_id node in

         let label, children =
           match  node with
           | Pure _ -> ("Pure", [])
           | Root t ->
               let status = get_eval_status_str t.value in
               (Printf.sprintf "Root\nstatus: %s" status, [ (obj_t t.child, "solid") ])
           | Operator t ->
               let status = get_eval_status_str t.value in
               let desc_str, children =
                 match t.desc with
                 | Map (x, _) -> "Map", [ (obj_t x, "solid") ]
                 | Map2 (x, y, _) -> "Map2", [ (obj_t x, "solid"); (obj_t y, "solid") ]
                 | Pair (x, y) -> "Pair", [ (obj_t x, "solid"); (obj_t y, "solid") ]
                 | App (f, x) -> "App", [ (obj_t f, "solid"); (obj_t x, "solid") ]
                 | Join { child; intermediate } ->
                     let c = [ (obj_t child, "solid") ] in
                     let c =
                       match intermediate with
                       | None -> c
                       | Some i -> (obj_t i, "dotted") :: c
                     in
                     "Join", c
                 | Var v ->
                     (Printf.sprintf "Var\nhash: %d" (Hashtbl.hash v.binding), [])
                 | Prim _ -> "Prim", []
                 | Fix { doc; wrt } -> "Fix", [ (obj_t doc, "solid"); (obj_t wrt, "solid") ]
               in
               (Printf.sprintf "%s\nstatus: %s" desc_str status, children)
         in

         let label = Printf.sprintf "%s\n%s" node_id_str label in
         Printf.bprintf buf "  %s[\"%s\"];\n" node_id_str label;

         if !nodes_processed < max_nodes then (
           List.iter (fun (child, style) ->
             let child_id_str = get_id child in
             let arrow = if style = "dotted" then "-.-> " else "-->" in
             Printf.bprintf buf "  %s %s %s;\n" node_id_str arrow child_id_str;
             Queue.add child q
           ) children
         ) else if children <> [] then (
           let ellipsis_id = node_id_str ^ "_ellipsis" in
           Printf.bprintf buf "  %s[\"...\"];\n" ellipsis_id;
           Printf.bprintf buf "  %s --> %s;\n" node_id_str ellipsis_id
         );
       );
       process_queue ()
     )
   in
   process_queue ();
   Buffer.contents buf

  let to_mermaid_trace (type a) ?(max_nodes=100) (start : a t_) : string =
    let buf = Buffer.create 1024 in
    Buffer.add_string buf "graph TD;\n";
    let visited : (Any.t t_, unit) Hashtbl.t = Hashtbl.create 16 in

    let get_id : type a. a t_ -> string = fun node -> Format.asprintf "%a" pp_addr node in

    let get_eval_status_str (type a) (eval : a eval) : string =
      match eval with
      | Eval_none -> "None"
      | Eval_progress -> "Progress"
      | Eval_some _ -> "Some"
      | Eval_invalid_next -> "Invalid_next"
    in

    (* Using breadth-first traversal following trace (parent) relationships *)
    let q : Any.t t_ Queue.t = Queue.create () in
    Queue.add (obj_t ( start)) q;
    let nodes_processed = ref 0 in

    let rec process_queue () =
      if not (Queue.is_empty q) && !nodes_processed < max_nodes then (
        let node = Queue.take q in
        if not (Hashtbl.mem visited node) then (
          Hashtbl.add visited node ();
          incr nodes_processed;
          let node_id_str = get_id node in

          let label, parents =
            match node with
            | Pure _ -> ("Pure", [])
            | Root t ->
                let status = get_eval_status_str t.value in
                (Printf.sprintf "Root\nstatus: %s" status, [])
            | Operator t ->
                let status = get_eval_status_str t.value in
                let desc_str =
                  match t.desc with
                  | Map (_, _) -> "Map"
                  | Map2 (_, _, _) -> "Map2"
                  | Pair (_, _) -> "Pair"
                  | App (_, _) -> "App"
                  | Join _ -> "Join"
                  | Var v -> Printf.sprintf "Var\nhash: %d" (Hashtbl.hash v.binding)
                  | Prim _ -> "Prim"
                  | Fix _ -> "Fix"
                in
                let parents =
                  match t.trace with
                  | T0 -> []
                  | T1 p1 -> [obj_t p1]
                  | T2 (p1, p2) -> [obj_t p1; obj_t p2]
                  | T3 (p1, p2, p3) -> [obj_t p1; obj_t p2; obj_t p3]
                  | T4 (p1, p2, p3, p4) -> [obj_t p1; obj_t p2; obj_t p3; obj_t p4]
                  | Tn t -> Array.to_list (Array.sub t.entries 0 t.active)
                in
                (Printf.sprintf "%s\nstatus: %s" desc_str status, parents)
          in

          let label = Printf.sprintf "%s\n%s" node_id_str label in
          Printf.bprintf buf "  %s[\"%s\"];\n" node_id_str label;

          if !nodes_processed < max_nodes then (
            List.iter (fun parent ->
              let parent_id_str = get_id parent in
              Printf.bprintf buf "  %s --> %s;\n" parent_id_str node_id_str;
              Queue.add parent q
            ) parents
          ) else if parents <> [] then (
            let ellipsis_id = node_id_str ^ "_ellipsis" in
            Printf.bprintf buf "  %s[\"...\"];\n" ellipsis_id;
            Printf.bprintf buf "  %s --> %s;\n" ellipsis_id node_id_str
          );
        );
        process_queue ()
      )
    in
    process_queue ();
    Buffer.contents buf

  let to_mermaid_trace_idx (type a) ?(max_nodes=100) (start : a t_) : string =
    let buf = Buffer.create 1024 in
    Buffer.add_string buf "graph TD;\n";
    let visited : (Any.t t_, unit) Hashtbl.t = Hashtbl.create 16 in

    let get_id : type a. a t_ -> string = fun node -> Format.asprintf "%a" pp_addr node in

    let get_eval_status_str (type a) (eval : a eval) : string =
      match eval with
      | Eval_none -> "None"
      | Eval_progress -> "Progress"
      | Eval_some _ -> "Some"
      | Eval_invalid_next -> "Invalid_next"
    in

    (* Using breadth-first traversal with a queue to prevent stack overflows on deep graphs. *)
    let q : Any.t t_ Queue.t = Queue.create () in
    Queue.add (obj_t (start)) q;
    let nodes_processed = ref 0 in

    let rec process_queue () =
      if not (Queue.is_empty q) && !nodes_processed < max_nodes then (
        let node = Queue.take q in
        if not (Hashtbl.mem visited node) then (
          Hashtbl.add visited node ();
          incr nodes_processed;
          let node_id_str = get_id node in

          let get_children_from_trace_idx trace_idx =
            let rec aux acc = function
              | I0 -> acc
              | I1 { obj; next; _ } -> aux ((obj_t obj, "solid") :: acc) next
            in
            aux [] trace_idx
          in

          let label, children =
            match node with
            | Pure _ -> ("Pure", [])
            | Root t ->
                let status = get_eval_status_str t.value in
                let children = get_children_from_trace_idx t.trace_idx in
                (Printf.sprintf "Root\nstatus: %s" status, children)
            | Operator t ->
                let status = get_eval_status_str t.value in
                let desc_str =
                  match t.desc with
                  | Map (_, _) -> "Map"
                  | Map2 (_, _, _) -> "Map2"
                  | Pair (_, _) -> "Pair"
                  | App (_, _) -> "App"
                  | Join _ -> "Join"
                  | Var v -> Printf.sprintf "Var\nhash: %d" (Hashtbl.hash v.binding)
                  | Prim _ -> "Prim"
                  | Fix _ -> "Fix"
                in
                let children = get_children_from_trace_idx t.trace_idx in
                (Printf.sprintf "%s\nstatus: %s" desc_str status, children)
          in

          let label = Printf.sprintf "%s\n%s" node_id_str label in
          Printf.bprintf buf "  %s[\"%s\"];\n" node_id_str label;

          if !nodes_processed < max_nodes then (
            List.iter (fun (child, style) ->
              let child_id_str = get_id child in
              let arrow = if style = "dotted" then "-.-> " else "-->" in
              Printf.bprintf buf "  %s %s %s;\n" node_id_str arrow child_id_str;
              Queue.add child q
            ) children
          ) else if children <> [] then (
            let ellipsis_id = node_id_str ^ "_ellipsis" in
            Printf.bprintf buf "  %s[\"...\"];\n" ellipsis_id;
            Printf.bprintf buf "  %s --> %s;\n" node_id_str ellipsis_id
          );
        );
        process_queue ()
      )
    in
    process_queue ();
    Buffer.contents buf

  let add_idx obj idx = function
    | Pure _ -> assert false
    | Root t' -> t'.trace_idx <- I1 { idx; obj; next = t'.trace_idx }
    | Operator t' -> t'.trace_idx <- I1 { idx; obj; next = t'.trace_idx }
  
  let rec rem_idx_rec obj = function
    | I0 -> assert false
    | I1 t as self ->
      if t_equal t.obj obj
      then (t.idx, t.next)
      else (
        let idx, result = rem_idx_rec obj t.next in
        t.next <- result;
        (idx, self)
      )
  
  (* remove [obj] from the lwd's trace. *)
  let rem_idx obj = function
    | Pure _ -> assert false
    | Root t' ->
      let idx, trace_idx = rem_idx_rec obj t'.trace_idx in
      t'.trace_idx <- trace_idx; idx
    | Operator t' ->
      let idx, trace_idx = rem_idx_rec obj t'.trace_idx in
      t'.trace_idx <- trace_idx; idx
  
  (* move [obj] from old index to new index. *)
  let rec mov_idx_rec obj oldidx newidx = function
    | I0 -> assert false
    | I1 t ->
      if t.idx = oldidx && t_equal t.obj obj
      then t.idx <- newidx
      else mov_idx_rec obj oldidx newidx t.next
  
  let mov_idx obj oldidx newidx = function
    | Pure _ -> assert false
    | Root t' -> mov_idx_rec obj oldidx newidx t'.trace_idx
    | Operator t' -> mov_idx_rec obj oldidx newidx t'.trace_idx
  
  let rec get_idx_rec obj = function
    | I0 -> assert false
    | I1 t ->
      if t_equal t.obj obj
      then t.idx
      else get_idx_rec obj t.next
  
  (* find index of [obj] in the given lwd *)
  let get_idx obj = function
    | Pure _ -> assert false
    | Root t' -> get_idx_rec obj t'.trace_idx
    | Operator t' -> get_idx_rec obj t'.trace_idx
  
  type status =
    | Neutral
    | Safe
    | Unsafe of (unit->unit) list ref
  
  (*
  Sensitivity is used to indicate to when reading a root node, that one of the child operater nodes was being evaluated.
  I think this is needed because the child cound have multiple roots and we need to indicate that to all of them
  *)
  type sensitivity =
    | Strong
    | Fragile
  
  let pp_sensitivity ppf = function
    | Strong -> Format.fprintf ppf "Strong"
    | Fragile -> Format.fprintf ppf "Fragile"
  
  (* Propagating invalidation recursively.
     Each document is invalidated at most once,
     and only if it has [t.value = Some _]. *)
  let rec invalidate_node : type a . status ref -> sensitivity -> a t_ -> unit =
    (*sensitivity indicates that a parent is being evaluated*)
    fun status sensitivity node ->

    match node, sensitivity with
    | Pure _, _ -> assert false
    | Root ({value; on_invalidate; _} as t), _ ->
        (match value with
         | Eval_none | Eval_invalid_next -> ()
         | Eval_progress ->
             t.value <- Eval_invalid_next
         | Eval_some x ->
             t.value <- Eval_none;
             on_invalidate x
        )
    | Operator { value = Eval_none | Eval_invalid_next; _ }, _ -> ()
    | Operator { desc = Fix { wrt = Operator { value = Eval_none | Eval_invalid_next; _ }; _ }; _ }, Fragile ->
      (match !status with
       | Safe | Unsafe _ -> ()
       | Neutral -> status := Safe)
    | Operator { desc = Fix { wrt = Operator { value = Eval_some _; _ }; _ }; _ }, Fragile
      -> ()
    | Operator t, _ ->
      let sensitivity =
        match t.value with Eval_progress -> Fragile | _ -> sensitivity
      in
      t.value <- Eval_none;
      (* invalidate parents recursively *)
      invalidate_trace status sensitivity t.trace
  
  (* invalidate recursively documents in the given trace *)
  and invalidate_trace status sensitivity = function
    | T0 -> ()
    | T1 x -> invalidate_node status sensitivity x
    | T2 (x, y) ->
      invalidate_node status sensitivity x;
      invalidate_node status sensitivity y
    | T3 (x, y, z) ->
      invalidate_node status sensitivity x;
      invalidate_node status sensitivity y;
      invalidate_node status sensitivity z
    | T4 (x, y, z, w) ->
      invalidate_node status sensitivity x;
      invalidate_node status sensitivity y;
      invalidate_node status sensitivity z;
      invalidate_node status sensitivity w
    | Tn t ->
      let active = t.active in
      t.active <- 0;
      for i = 0 to active - 1 do
        invalidate_node status sensitivity t.entries.(i)
      done
  
  let default_unsafe_mutation_logger () =
    let callstack = Printexc.get_callstack 20 in
    Printf.fprintf stderr
      "Lwd: unsafe mutation (variable invalidated during evaluation) at\n%a"
      Printexc.print_raw_backtrace callstack
  
  let unsafe_mutation_logger = ref default_unsafe_mutation_logger
  
  
  let do_invalidate sensitivity (node : 'a t_) =
    let status = ref Neutral in
    invalidate_node status sensitivity node;
  (* Variables *)
  type 'a var = 'a t_
  let var x = operator (Var {binding = x})
  let get x = inj x
  
  let get_parents_from_trace  (trace:trace) : (Any.t var) list =
    match trace with
    | T0 -> []
    | T1 p1 -> [obj_t p1]
    | T2 (p1, p2) -> [obj_t p1; obj_t p2]
    | T3 (p1, p2, p3) -> [obj_t p1; obj_t p2; obj_t p3]
    | T4 (p1, p2, p3, p4) -> [obj_t p1; obj_t p2; obj_t p3; obj_t p4]
    | Tn t ->
      let res = ref [] in
      for i = t.active - 1 downto 0 do
        res := t.entries.(i) :: !res
      done;
      !res

  let set (vx:'a var) x : unit =
    match vx with
    | (Operator ({desc = Var v; _;} as inner )) ->
      v.binding <- x;
      inner.value <- Eval_some x;
      (* [climb] traverses the graph upwards from a changed variable, invalidating
         parent nodes. The `parents` list is a list of work to do, containing tuples
         of `(node, has_seen_invalid_next)`.

         The `has_seen_invalid_next` flag is crucial for correctness. It tracks
         whether we have already encountered a node marked `Eval_invalid_next`
         in the current upward traversal path.

         When a node is being evaluated (`Eval_progress`), and a variable it
         depends on is `set`, the evaluation might be happening in a different
         thread. If another invalidation has already marked a node higher up in
         the chain as `Eval_invalid_next`, we don't want to also mark the
         current `Eval_progress` node as `Eval_invalid_next`. The higher-up
         invalidation will already cause a re-evaluation that will deal with
         this node. Setting it to `Eval_invalid_next` here could lead to
         incorrect state transitions.

         Therefore, `has_seen_invalid_next` is propagated upwards. Once it's
         `true` for a given path, it remains `true` for all ancestors in that
         path. We only set a node from `Eval_progress` to `Eval_invalid_next`
         if `has_seen_invalid_next` is `false`. 
         We should actually be able to stop propagating invalidation once we have seen an Eval_invalid_next node, but in reality things seem to be messier than that and so we keep going.
         Instead i reset the seen_eval node flag if we ever encounter a node that is not Eval_progress.
          *)
      (let rec climb (parents: (Any.t t_ * bool) list) =
        let new_parents : (Any.t t_ * bool) list = List.fold_left (fun acc (p, seen_eval_node) ->
            match p with
            | Pure _ -> acc
            | Root r ->
                if Mutex.try_lock r.mutex then (
                  (match r.value with
                   | Eval_some v ->
                     r.value <- Eval_none;
                     r.on_invalidate v
                   | Eval_none | Eval_invalid_next -> ()
                   | Eval_progress -> if not seen_eval_node then r.value <- Eval_invalid_next
                  );
                  Mutex.unlock r.mutex;
                  acc
                ) else (
                  (* if the root is currently being evaluated, we pro*)
                  (* Mutex.protect r.mutex (fun () -> *)
                  (* ); *)
                  (* ELI: try just skipping the lock and invalidating the root, it should be safe *)
                  if r.value = Eval_progress && not seen_eval_node then r.value <- Eval_invalid_next;
                  acc
                )
            | Operator o ->
                let (continue, seen_eval_node) =
                  if Mutex.try_lock o.mutex then (
                    let current_value = o.value in
                    let continue,this_node_has_seen_invalid_next =
                      match current_value with
                      | Eval_some _ -> o.value <- Eval_none; true,false
                      (* This shouldn't be needed, but sometimes it is so we do it anyway*)
                      | Eval_none -> true,false
                      | Eval_invalid_next -> true,true
                      | Eval_progress -> (if not seen_eval_node then 
                        o.value <- Eval_invalid_next); true,true
                    in
                    Mutex.unlock o.mutex;
                    (continue, this_node_has_seen_invalid_next )
                  ) else (
                    Mutex.protect o.mutex (fun () ->
                        if o.value = Eval_progress && not seen_eval_node then o.value <- Eval_invalid_next
                      );
                    (true, true)
                  )
                in
                if continue then (
                  let parents_of_o = get_parents_from_trace o.trace in
                  let new_acc_entries = List.map (fun p -> (obj_t p, seen_eval_node)) parents_of_o in
                  List.rev_append new_acc_entries acc
                ) else acc
          ) [] parents in
        if new_parents <> [] then climb new_parents
      in
      let initial_parents = get_parents_from_trace inner.trace in
      climb (List.map (fun p -> (obj_t p, false)) initial_parents)
      )
    | _ -> assert false
  
  let peek_stable = function
    | Operator ({desc = Var v; _}) -> v.binding
    | _ -> assert false
  
  let peek = function
    | Operator ({desc = Var v; _}) -> v.binding
    | _ -> assert false
  
  let update f v = set v (f (peek v))
  
  let may_update f v =
    match f (peek v) with
    | None -> ()
    | Some x -> set v x
  
  (* Primitives *)
  type 'a prim = 'a t
  let prim ~acquire ~release =
    inj (operator (Prim { acquire; release }))
  let get_prim x = x
  
  let invalidate x = match prj x with
    | Operator {desc = Prim p; value; _} as t ->
      (* the value is invalidated, be sure to invalidate all parents as well *)
      begin match value with
        | Eval_none | Eval_invalid_next -> ()
        | Eval_progress -> do_invalidate Fragile t;
        | Eval_some v ->
          do_invalidate Strong t;
          p.release x v
      end
    | _ -> assert false
  
  (* Fix point *)
  
  let fix doc ~wrt = match prj wrt with
    | Root _ -> assert false
    | Pure _ -> doc
    | Operator _ as wrt -> inj (operator (Fix {doc = prj doc; wrt}))
  
  type release_list =
    | Release_done
    | Release_more :
        { origin : 'a t_; element : 'b t_; next : release_list } -> release_list
  
  type release_queue = release_list ref
  let make_release_queue () = ref Release_done
  
  type release_failure = exn * Printexc.raw_backtrace
  
  (* [sub_release [] origin self] is called when [origin] is released,
     where [origin] is reachable from [self]'s trace.
     We're going to remove [origin] from that trace as [origin] is now dead.
  
     [sub_release] cannot raise.
     If a primitive raises, the exception is caught and a warning is emitted. *)
  let rec sub_release
    : type a b . release_failure list -> a t_ -> b t_ -> release_failure list
    = fun failures origin -> function
      | Root _ -> assert false
      | Pure _ -> failures
      | Operator t as self ->
        Mutex.protect t.mutex @@ fun () ->
        (* compute [t.trace \ {origin}] *)
        let trace = match t.trace with
          | T0 -> assert false
          | T1 x -> assert (t_equal x origin); T0
          | T2 (x, y) ->
            if t_equal x origin then T1 y
            else if t_equal y origin then T1 x
            else assert false
          | T3 (x, y, z) ->
            if t_equal x origin then T2 (y, z)
            else if t_equal y origin then T2 (x, z)
            else if t_equal z origin then T2 (x, y)
            else assert false
          | T4 (x, y, z, w) ->
            if t_equal x origin then T3 (y, z, w)
            else if t_equal y origin then T3 (x, z, w)
            else if t_equal z origin then T3 (x, y, w)
            else if t_equal w origin then T3 (x, y, z)
            else assert false
          | Tn tn as trace ->
            let revidx = rem_idx self origin in
            assert (t_equal tn.entries.(revidx) origin);
            let count = tn.count - 1 in
            tn.count <- count;
            if revidx < count then (
              let obj = tn.entries.(count) in
              tn.entries.(revidx) <- obj;
              tn.entries.(count) <- dummy;
              mov_idx self count revidx obj
            ) else
              tn.entries.(revidx) <- dummy;
            if tn.active > count then tn.active <- count;
            if count = 4 then (
              (* downgrade to [T4] to save space *)
              let a = tn.entries.(0) and b = tn.entries.(1) in
              let c = tn.entries.(2) and d = tn.entries.(3) in
              ignore (rem_idx self a : int);
              ignore (rem_idx self b : int);
              ignore (rem_idx self c : int);
              ignore (rem_idx self d : int);
              T4 (a, b, c, d)
            ) else (
              let len = Array.length tn.entries in
              if count <= len lsr 2 then
                Tn { active = tn.active; count = tn.count;
                     entries = Array.sub tn.entries 0 (len lsr 1) }
              else
                trace
            )
        in
        t.trace <- trace;
        match trace with
        | T0 ->
          (* [self] is not active anymore, since it's not reachable
             from any root. We can release its cached value and
             recursively release its subtree. *)
          let value = t.value in
          t.value <- Eval_none;
          begin match t.desc with
            | Map  (x, _) -> sub_release failures self x
            | Map2 (x, y, _) ->
              sub_release (sub_release failures self x) self y
            | Pair (x, y) ->
              sub_release (sub_release failures self x) self y
            | App  (x, y) ->
              sub_release (sub_release failures self x) self y
            | Join ({ child; intermediate } as t) ->
              let failures = sub_release failures self child in
              begin match intermediate with
                | None -> failures
                | Some child' ->
                  t.intermediate <- None;
                  sub_release failures self child'
              end
            | Var  _ -> failures
            | Fix {doc; wrt} ->
              sub_release (sub_release failures self wrt) self doc
            | Prim t ->
              begin match value with
                | Eval_none | Eval_invalid_next | Eval_progress -> failures
                | Eval_some x ->
                  begin match t.release (inj self) x with
                    | () -> failures
                    | exception exn ->
                      let bt = Printexc.get_raw_backtrace () in
                      (exn, bt) :: failures
                  end
              end
          end
        | _ -> failures
  
  (* [sub_acquire] cannot raise *)
  let rec sub_acquire : type a b . a t_ -> b t_ -> unit = fun origin ->
    function
    | Root _ -> assert false
    | Pure _ -> ()
    | Operator t as self ->
      (*lock the mutex, because we are making changes within this node *)

      Mutex.protect t.mutex @@ fun _-> 
      (* [acquire] is true if this is the first time this operator
         is used, in which case we need to acquire its children *)
      let acquire = match t.trace with T0 -> true | _ -> false in
      let trace = match t.trace with
        | T0 -> T1 origin
        | T1 x -> T2 (origin, x)
        | T2 (x, y) -> T3 (origin, x, y)
        | T3 (x, y, z) -> T4 (origin, x, y, z)
        | T4 (x, y, z, w) ->
          let obj_origin = obj_t origin in
          let entries =
            [| obj_t x; obj_t y; obj_t z; obj_t w; obj_t origin; dummy; dummy; dummy |]
          in
          for i = 0 to 4 do add_idx self i entries.(i) done;
          Tn { active = 5; count = 5; entries }
        | Tn tn as trace ->
          let index = tn.count in
          let entries, trace =
            (* possibly resize array [entries] *)
            if index < Array.length tn.entries then (
              tn.count <- tn.count + 1;
              (tn.entries, trace)
            ) else (
              let entries = Array.make (index * 2) dummy in
              Array.blit tn.entries 0 entries 0 index;
              (entries, Tn { active = tn.active; count = index + 1; entries })
            )
          in
          let obj_origin = obj_t origin in
          entries.(index) <- obj_origin;
          add_idx self index obj_origin;
          trace
      in
      t.trace <- trace;
      if acquire then (
        (* acquire immediate children, and so on recursively *)
        match t.desc with
        | Map  (x, _) -> sub_acquire self x
        | Map2 (x, y, _) ->
          sub_acquire self x;
          sub_acquire self y
        | Pair (x, y) ->
          sub_acquire self x;
          sub_acquire self y
        | App  (x, y) ->
          sub_acquire self x;
          sub_acquire self y
        | Fix  {doc; wrt} ->
          sub_acquire self doc;
          sub_acquire self wrt
        | Join { child; intermediate } ->
          sub_acquire self child;
          begin match intermediate with
            | None -> ()
            | Some _ ->
              assert false (* this can't initialized already, first-time acquire *)
          end
        | Var  _ -> ()
        | Prim _ -> ()
      )
  
  (* make sure that [origin] is in [self.trace], passed as last arg. *)
  let activate_tracing self origin = function
    | Tn tn ->
      let idx = get_idx self origin in (* index of [self] in [origin.trace_idx] *)
      let active = tn.active in
      (* [idx < active] means [self] is already traced by [origin].
         We only have to add [self] to the entries if [idx >= active]. *)
      if idx >= active then (
        tn.active <- active + 1;
      );
      if idx > active then (
        (* swap with last entry in [tn.entries] *)
        let old = tn.entries.(active) in
        tn.entries.(idx) <- old;
        tn.entries.(active) <- obj_t origin;
        mov_idx self active idx old;
        mov_idx self idx active origin
      )
    | _ -> ()
  
  let sub_is_damaged = function
    | Root _ -> assert false
    | Pure _ -> false
    | Operator {value; _} ->
      match value with
      | Eval_none | Eval_invalid_next -> true
      | Eval_some _ -> false
      | Eval_progress -> assert false
  
  (* [sub_sample origin self] computes a value for [self].
  
     [sub_sample] raise if any user-provided computation raises.
     Graph will be left in a coherent state but exception will be propagated
     to the observer. *)
  let rec sub_sample queue =
    let rec aux : type a b . a t_ -> b t_ -> b = fun origin ->
      function
      | Root _ -> assert false
      | Pure x -> x
      | Operator t as self ->
        (* lock the mutex, examine cached value *)

        Mutex.lock t.mutex;

        match t.value with
        | Eval_some value ->
            Mutex.unlock t.mutex;
            activate_tracing self origin t.trace;
            value
        | Eval_none ->
            t.value <- Eval_progress;
            Mutex.unlock t.mutex;
  
            (* compute value without holding the lock *)

            let result : b =
              match t.desc with
              | Map (x, f) -> f (aux self x)
              | Map2 (x, y, f) -> f (aux self x) (aux self y)
              | Pair (x, y) -> (aux self x, aux self y)
              | App (f, x) -> (aux self f) (aux self x)
              | Fix { doc; wrt } ->
                  let _ = aux self wrt in
                  let result = aux self doc in
                  if sub_is_damaged wrt then aux origin self
                  else (
                    if sub_is_damaged doc then do_invalidate Fragile self;
                    result)
              | Join x ->

                  let intermediate =
                    (* We haven't touched any state yet,
                       it is safe for [aux] to raise *)
                    aux self x.child
                  in
                  begin
                    match x.intermediate with
                    | None ->
                        x.intermediate <- Some intermediate;

                        sub_acquire self intermediate
                    | Some x' when x' != intermediate ->
                        queue :=
                          Release_more
                            { origin = self; element = x'; next = !queue };
                        x.intermediate <- Some intermediate;

                        sub_acquire self intermediate
                    | Some _ -> ()
                  end;
                  (*print mermaid*)

                  let mermaid=to_mermaid_trace_idx ~max_nodes:200 (  intermediate) in
                  (* let mermaid_2=to_mermaid ~max_nodes:200 (  intermediate) in *)
                  let mermaid_3=to_mermaid_trace_idx ~max_nodes:200 (  intermediate) in



                  aux self intermediate
              | Var x -> 

                x.binding
              | Prim t -> t.acquire (inj self)
            in
  
            (* lock again and finalize *)

            Mutex.lock t.mutex;
            begin
              match t.value with
              | Eval_progress -> t.value <- Eval_some result
              | Eval_invalid_next -> t.value <- Eval_none
              | Eval_none | Eval_some _ -> ()
            end;
            Mutex.unlock t.mutex;

  

            (* Re-evaluate if the node was invalidated during computation *)
            if t.value == Eval_none then (

              aux origin self
            ) else (

              (* [self] just became active, so it may invalidate [origin] in case its
                 value changes because of [t.desc], like if it's a variable and gets
                 mutated, or if it's a primitive that gets invalidated.
                 We need to put [origin] into [self.trace] in case it isn't there yet. *)
              activate_tracing self origin t.trace;
              result)
        | Eval_progress | Eval_invalid_next ->
            Mutex.unlock t.mutex;

            (* spin and retry *)
            let rec spin () =
              match t.value with
              | Eval_progress | Eval_invalid_next ->
                  Domain.cpu_relax ();
                  spin ()
              | Eval_none | Eval_some _ -> ()
            in
            spin ();

            aux origin self
    in
    aux
  
  type 'a root = 'a t
  
  let observe ?(on_invalidate = ignore) child : _ root =
    let root =
      Root
        { child = prj child
        ; value = Eval_none
        ; on_invalidate
        ; trace_idx = I0
        ; acquired = false
        ; mutex= Mutex.create()
        }
    in
    inj root
  
  exception Release_failure of exn option * release_failure list
  
  let raw_flush_release_queue queue =
    let rec aux failures = function
      | Release_done -> failures
      | Release_more t ->

        let failures = sub_release failures t.origin t.element in
        let return = aux failures t.next in

        return
    in
    aux [] queue
  
  let flush_release_queue queue =
    let queue' = !queue in
    queue := Release_done;
    raw_flush_release_queue queue'
  
  let sample queue x = match prj x with
    | Pure _ | Operator _ -> assert false
    | Root t as self ->

    (* debug log the whole tree *)

      let mermaid=to_mermaid_trace_idx ~max_nodes:200 ( t.child) in
      (* m "sample: graph: \n %s" mermaid); *)
    


      (*lock the root mutex while sampling*)

      let a=Mutex.protect t.mutex @@ fun _->
      match t.value with
      | Eval_some value -> value
      | _ ->
      (
        (* no cached value, compute it now *)
        if not t.acquired then (

          t.acquired <- true;
          let res = sub_acquire self t.child in

          res
        );
        
        t.value <- Eval_progress;
        let value = sub_sample queue self t.child in
        begin match t.value with
          | Eval_progress -> 

            t.value <- Eval_some value; (* cache value *)
          | Eval_none | Eval_some _ | Eval_invalid_next -> ()
        end;
        value
      )
      in

      a
  
  let is_damaged x =
    match prj x with
    | Pure _ | Operator _ -> assert false
    | Root {value;_}->
        (* NOTE:  I don't think i need a mutex here*)
        (match value with
        | Eval_some _ -> false
        | Eval_none | Eval_progress | Eval_invalid_next -> true
        )
  
  let release queue x = match prj x with
    | Pure _ | Operator _ -> assert false
    | Root t as self ->
      Mutex.protect t.mutex @@ fun _->
      if t.acquired then (
        (* release subtree, remove cached value *)
        t.value <- Eval_none;
        t.acquired <- false;
        queue := Release_more { origin = self; element = t.child; next = !queue }
      )
  
  let set_on_invalidate x f =
    match prj x with
    | Pure _ | Operator _ -> assert false
    | Root t ->
      t.on_invalidate <- f
  
  let flush_or_fail main_exn queue =
    match flush_release_queue queue with
    | [] -> ()
    | failures -> raise (Release_failure (main_exn, failures))
  
  let quick_sample root =
    let queue = ref Release_done in
    match sample queue root with
    | result -> flush_or_fail None queue; result
    | exception exn -> flush_or_fail (Some exn) queue; raise exn
  
  let quick_release root =
    let queue = ref Release_done in
    release queue root;
    flush_or_fail None queue
  
  module Infix = struct
    let (>>=) x f = bind x ~f
    let (>|=) x f = map x ~f
    let (<*>) = app
  end
  
  (*$R
    let x = var 0 in
    let y = map ~f:succ (get x) in
    let o_y = Lwd.observe y in
    assert_equal 1 (quick_sample o_y);
    set x 10;
    assert_equal 11 (quick_sample o_y);
    *)
  end