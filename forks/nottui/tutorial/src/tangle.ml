(** This is a system for templating code into markdown files for building tutorials. It's a little like literate programming but in reverse.
    The code has lines with comments that indicate the start and end of a templated section. ($#S10 $#E10)
    The markdown has lines that show where those sections should go ($#10) *)

open Printf

type code_block =
  { language : string
  ; content : string
  }

let write_file filename content =
  let ch = open_out filename in
  output_string ch content;
  close_out ch
;;

open! Base

module Parsing = struct
  open Angstrom

  (* Define the variant type for different sequences *)
  type tangle_type =
    | Start of int
    | End of int
    | Id of int
    | Fold of int * string

  (* Helper function to parse a number *)
  let parse_number =
    take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
    >>| Int.of_string
  ;;

  let parse_cmd letter = string ("$#" ^ letter) *> parse_number

  (* Parser for $#S<number> *)
  let parse_start = parse_cmd "S" >>| fun x -> Start x

  (* Parser for $#E<number> *)
  let parse_end = parse_cmd "E" >>| fun x -> End x

  (* Parser for $#<number> *)
  let parse_id = parse_cmd "" >>| fun x -> Id x

  (* Parser for $#<number> fold <name> *)
  let parse_fold =
    string "$#" *> parse_number
    <* string " fold "
    >>= fun n -> take_while1 Char.(fun c -> c <> '\n') >>| fun name -> Fold (n, name)
  ;;

  (* Combine all parsers *)
  let sequence_parser =
    choice
      [ parse_start
      ; parse_end
      ; parse_fold
      ; (* Use attempt to backtrack if it's not a fold *)
        parse_id
      ]
  ;;

  (* Parser for finding sequences in a line of text *)
  let line_parser = many (not_char '$' *> advance 1) *> sequence_parser

  (* Function to parse a line and return the first found sequence, if any *)
  let parse_line line =
    match parse_string ~consume:Prefix line_parser line with
    | Ok result -> Some result
    | Error _ -> None
  ;;

  let lang_from_ext ext =
    match ext with
    | "ml" | "mli" -> "ocaml"
    | "py" -> "python"
    | "js" -> "javascript"
    | "ts" -> "typescript"
    | "rb" -> "ruby"
    | "rs" -> "rust"
    | "go" -> "go"
    | "java" -> "java"
    | "c" | "h" -> "c"
    | "cpp" | "hpp" | "cxx" | "hxx" -> "cpp"
    | "cs" -> "csharp"
    | "php" -> "php"
    | "sh" | "bash" -> "bash"
    | "html" | "htm" -> "html"
    | "css" -> "css"
    | "json" -> "json"
    | "xml" -> "xml"
    | "md" -> "markdown"
    | "sql" -> "sql"
    | "hs" -> "haskell"
    | "swift" -> "swift"
    | "kt" | "kts" -> "kotlin"
    | _ -> "unknown"
  ;;
end

open Parsing

type pre_code_block =
  { id : int
  ; language : string
  ; lines : string list
  }

let extract_code_blocks filename output_tbl =
  let lines = Iter.IO.lines_of filename in
  let language = filename |> String.rsplit2_exn ~on:'.' |> snd |> lang_from_ext in
  let iter state line =
    match parse_line line with
    | Some (Start id) -> { id; language; lines = [] } :: state
    | Some (End id) ->
      state
      |> List.filter ~f:(fun x ->
        if x.id = id
        then (
          output_tbl
          |> Hashtbl.set
               ~key:id
               ~data:
                 { language = x.language
                 ; content = x.lines |> List.rev |> String.concat_lines
                 };
          false)
        else true)
    | None ->
      state |> List.map ~f:(fun block -> { block with lines = line :: block.lines })
    | _ -> failwith (Printf.sprintf "Found incorrect tangle command on line: %s" line)
  in
  lines |> Iter.fold iter [] |> ignore;
  output_tbl
;;

let process_markdown filename (code_blocks : (int, code_block) Hashtbl.t) =
  let open Option.Let_syntax in
  Iter.IO.lines_of filename
  |> Iter.map (fun line ->
    let out =
      let%bind id, formatter =
        match line |> parse_line with
        | Some (Id id) -> Some (id, fun x -> x)
        | Some (Fold (id, name)) ->
          Some
            ( id
            , fun content ->
                sprintf
                  "<details>\n  <summary>%s</summary>\n\n%s</details>\n"
                  name
                  content )
        | _ -> None
      in
      let%map block = Hashtbl.find code_blocks id in
      sprintf "```%s\n%s```\n" block.language block.content |> formatter
    in
    out |> Option.value ~default:(line ^ "\n"))
  |> Iter.concat_str
;;

let process_folder input_path output_path =
  let files = Stdlib.Sys.readdir input_path |> Array.to_list in
  let code_files =
    files
    |> List.filter_map ~f:(fun f ->
      let path = Stdlib.Filename.concat input_path f in
      if (not (Stdlib.Filename.check_suffix f ".md"))
         && not (Stdlib.Sys.is_directory path)
      then Some path
      else None)
  in
  let markdown_files =
    files |> List.filter ~f:(fun f -> Stdlib.Filename.check_suffix f ".md")
  in
  let code_blocks =
    Base.List.fold
      ~init:(Hashtbl.create (module Int))
      ~f:(fun hash_tbl path -> extract_code_blocks path hash_tbl)
      code_files
  in
  markdown_files
  |> List.iter ~f:(fun f ->
    let input_file = Stdlib.Filename.concat input_path f in
    let output_file =
      let out_path =
        Option.value
          output_path
          ~default:(input_path ^ Stdlib.Filename.dir_sep ^ "tangled")
      in
      if not (Stdlib.Sys.file_exists out_path) then Stdlib.Sys.mkdir out_path 0o755;
      Stdlib.Filename.concat out_path f
    in
    let processed_content = process_markdown input_file code_blocks in
    write_file output_file processed_content;
    printf "Processed %s -> %s\n" input_file output_file)
;;

let () =
  let open Stdlib in
  match Array.to_list Sys.argv with
  | [ _; input_path ] ->
    if Sys.is_directory input_path
    then process_folder input_path None
    else printf "Error: %s is not a valid directory\n" input_path
  | [ _; input_path; output_path ] ->
    if Sys.is_directory input_path
    then process_folder input_path (Some output_path)
    else printf "Error: %s is not a valid directory\n" input_path
  | _ -> printf "Usage: %s <input_folder_path> [output_folder_path]\n" Sys.argv.(0)
;;
