(** A version of the logging module that adds timestamps to the logs *)
module Log = struct
  let timestamp_tag =
    let now () = Unix.gettimeofday () |> Unix.localtime in
    let time_to_string tm =
      Printf.sprintf
        "%04d-%02d-%02d %02d:%02d:%02d"
        (tm.Unix.tm_year + 1900)
        (tm.Unix.tm_mon + 1)
        tm.Unix.tm_mday
        tm.Unix.tm_hour
        tm.Unix.tm_min
        tm.Unix.tm_sec
    in
    Logs.Tag.def "timestamp" ~doc:"Timestamp" (fun fmt tm ->
      time_to_string tm |> Format.pp_print_string fmt)
  ;;

  let timestamp_wrap fn : ('a, 'b) Logs.msgf =
    fun m ->
    fn (fun ?header ?(tags = Logs.Tag.empty) fmt ->
      let timestamp = Unix.gettimeofday () |> Unix.localtime in
      let tags = Logs.Tag.add timestamp_tag timestamp Logs.Tag.empty in
      m ?header ~tags fmt)
  ;;

  let debug ?src fn = Logs.debug ?src (timestamp_wrap fn)
  let info ?src fn = Logs.info ?src (timestamp_wrap fn)
  let warn ?src fn = Logs.warn ?src (timestamp_wrap fn)
  let err ?src fn = Logs.err ?src (timestamp_wrap fn)
  let app ?src fn = Logs.app ?src (timestamp_wrap fn)
end

module Internal =struct
let reporter ppf =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let with_stamp h tags k ppf fmt =
      let stamp = match tags with
      | None -> None
      | Some tags -> Logs.Tag.find Log.timestamp_tag tags
      in
      let dt = Format.pp_print_option (Logs.Tag.printer Log.timestamp_tag)  in
      Format.kfprintf k ppf ("%a[%a] @["^^fmt^^"@]@.")
        Logs.pp_header  (level, h) dt stamp
    in
    msgf @@ fun ?header ?tags fmt -> with_stamp header tags k ppf fmt
  in
  { Logs.report = report }
(** Removes old log files, keeping only the 20 most recent *)
let cleanup_logs () =
  let state_home = Unix.getenv "XDG_STATE_HOME" in
  let jj_tui_dir = Filename.concat state_home "jj_tui" in
  let log_files = Sys.readdir jj_tui_dir
    |> Array.to_list
    |> List.filter (fun file -> Filename.check_suffix file ".log")
    |> List.sort (fun a b -> String.compare b a) in
  if List.length log_files > 20 then
    List.iteri (fun i file ->
      if i >= 20 then
        let file_path = Filename.concat jj_tui_dir file in
        Unix.unlink file_path
    ) log_files
;;
let init_logging () =
  (*creates or opens the log file*)
  let get_log_file_channel () =
    let state_home = Unix.getenv "XDG_STATE_HOME" in
    let jj_tui_dir = Filename.concat state_home "jj_tui" in
    (try Unix.mkdir jj_tui_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    let timestamp = Unix.time () |> Unix.localtime in
    let timestamp_str = Printf.sprintf "%04d%02d%02d_%02d%02d%02d"
      (timestamp.tm_year + 1900) (timestamp.tm_mon + 1) timestamp.tm_mday
      timestamp.tm_hour timestamp.tm_min timestamp.tm_sec in
    let log_file = Filename.concat jj_tui_dir (Printf.sprintf "log_%s.log" timestamp_str) in
    let log_channel = open_out_gen [ Open_append; Open_creat ] 0o644 log_file in
    log_channel
  in
  (*Make a mutex for logging to prevent concurrency and threading issues *)
  let logging_mutex = Picos_std_sync.Mutex.create () in
  Logs.set_reporter_mutex
    ~lock:(fun () -> Picos_std_sync.Mutex.lock logging_mutex)
    ~unlock:(fun () -> Picos_std_sync.Mutex.unlock logging_mutex);
  (* log our logs into the log file*)
  let log_chan = get_log_file_channel () in
  let log_formatter = Format.formatter_of_out_channel log_chan in
  let reporter = reporter log_formatter in
  Logs.set_level (Some Debug);
  Logs.set_reporter reporter;
  (*make sure everything is working*)
  [%log info "Logging initialized"];
  cleanup_logs ();
  [%log debug "Old logs cleaned up"]
;;

end
(**
Initialize the logging system
*)
let init_logging= Internal.init_logging
