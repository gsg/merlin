(* DB format.
   "CMI " ^ filename -> MTIME * DIGEST
   "DIG " ^ DIGEST ->
   "FWD " ^ DIGEST -> DIGEST LIST
   "BKD " ^ DIGEST -> DIGEST LIST
string_of_float
*)

type digest = Digest.t

let file_mtime path =
  try (Unix.stat path).Unix.st_mtime
  with Not_found -> nan

module CMI = struct
  type info = {
    name: string;
    path: string;
    mtime: float;
    digest: digest;
    deps: digest list;
  }

  let get_info path =
    let open Cmi_format in
    let mtime = (Unix.stat path).Unix.st_mtime in
    let cmi = read_cmi path in
    let name = cmi.cmi_name in
    let rec deps mydigest acc = function
      | [] -> mydigest, List.rev acc
      | (_, None) :: xs ->
        deps mydigest acc xs
      | (name', Some mydigest) :: xs when name = name' ->
        deps mydigest acc xs
      | (_, Some digest) :: xs ->
        deps mydigest (digest :: acc) xs
    in
    let digest, deps = deps "" [] cmi.cmi_crcs in
    if digest = "" then
      raise Not_found
    else
      { name; path; mtime; digest; deps }
end

type update = { add : CMI.info list; remove : digest list }
let empty_update = { add = []; remove = [] }

module DB = struct
  type mtime = float
  type filename = string
  type modulename = string

  type ('a,'b) store = {
    put: Dbm.t -> 'a -> 'b -> unit;
    get: Dbm.t -> 'a -> 'b;
    remove: Dbm.t -> 'a -> unit;
  }

  let store name =
    let prepare key = (name ^ key) in
    let get dbm key =
      let raw = Dbm.find dbm (prepare key) in
      Marshal.from_string raw 0
    in
    let put dbm key value =
      let raw = Marshal.to_string value [] in
      Dbm.replace dbm (prepare key) raw
    in
    let remove dbm key =
      Dbm.remove dbm (prepare key)
    in
    { put; get; remove }

  let fwd_refs : (Digest.t, Digest.t list) store = store "FWD "
  let bwd_refs : (Digest.t, Digest.t list) store = store "BKD "
  let digest_info : (Digest.t, modulename * filename) store = store "DIG "
  let cmi_info : (filename, mtime * Digest.t) store = store "CMI "

  let remove_digest dbm digest =
    begin try
        let _, filename = digest_info.get dbm digest in
        digest_info.remove dbm digest;
        cmi_info.remove dbm filename
      with Not_found -> ()
    end;
    let fwd_refs =
      try
        let result = fwd_refs.get dbm digest in
        fwd_refs.remove dbm digest;
        result
      with Not_found -> []
    in
    let remove_bwd digest' =
      try
        let digests = bwd_refs.get dbm digest' in
        let digests = List.filter ((<>) digest) digests in
        bwd_refs.put dbm digest' digests
      with Not_found -> ()
    in
    List.iter remove_bwd fwd_refs

  let update_digests dbm {add; remove} =
    let to_remove = Hashtbl.create 7 in
    let to_update = Hashtbl.create 7 in
    let remove_digest digest =
      Hashtbl.add to_remove digest ();
      begin try
          let _, filename = digest_info.get dbm digest in
          digest_info.remove dbm digest;
          cmi_info.remove dbm filename
        with Not_found -> ()
      end;
      begin try
          let bkds = fwd_refs.get dbm digest in
          fwd_refs.remove dbm digest;
          List.iter
            (fun bkd -> Hashtbl.replace to_update bkd [])
            bkds
        with Not_found -> ()
      end
    in
    List.iter remove_digest remove;
    let add_cmi cmi =
      assert (not (Hashtbl.mem to_remove cmi.CMI.digest));
      digest_info.put dbm cmi.CMI.digest
        (cmi.CMI.name, cmi.CMI.path);
      cmi_info.put dbm cmi.CMI.path
        (cmi.CMI.mtime, cmi.CMI.digest);
      fwd_refs.put dbm cmi.CMI.digest
        cmi.CMI.deps;
      List.iter (fun digest ->
          let existing =
            try Hashtbl.find to_update digest
            with Not_found -> []
          in
          Hashtbl.replace to_update digest (cmi.CMI.digest :: existing)
        )
        cmi.CMI.deps
    in
    List.iter add_cmi add;
    let update_bwd digest deps =
      try
        let digests = bwd_refs.get dbm digest in
        let keep_it digest = not (Hashtbl.mem to_remove digest) in
        let digests = List.filter keep_it digests in
        let digests = deps @ digests in
        bwd_refs.put dbm digest digests
      with Not_found -> ()
    in
    Hashtbl.iter update_bwd to_update
end

let need_update dbm filename update =
  try
    let mtime, digest = DB.cmi_info.DB.get dbm filename in
    let mtime' = file_mtime filename in
    if mtime <> mtime' then
      {update with remove = digest :: update.remove}, true
    else
      update, false
  with
  | Not_found -> update, true
  | exn ->
    let msg = "need_update: Exception while processing " in
    let exc = Printexc.to_string exn in
    prerr_endline (msg ^ filename ^ ": " ^ exc);
    update, false

let process_file dbm filename update =
  let update, need_update = need_update dbm filename update in
  try
    if need_update then
      {update with add = CMI.get_info filename :: update.add}
    else
      update
  with exn ->
    let msg = "process_file: Exception while processing " in
    let exc = Printexc.to_string exn in
    prerr_endline (msg ^ filename ^ ": " ^ exc);
    update

let process_directory dbm dirname update =
  let files = Sys.readdir dirname in
  let filter_file update filename =
    if Filename.check_suffix filename ".cmi" then
      let filename = Filename.concat dirname filename in
      process_file dbm filename update
    else
      update
  in
  Array.fold_left filter_file update files

let process_line dbm line update =
  try
    if Sys.file_exists line && Sys.is_directory line then
      process_directory dbm line update
    else
      let msg = "Invalid input (unknown file or not a directory): " in
      prerr_endline (msg ^ line);
      update
  with exn ->
    let msg = "Exception while processing " in
    let exc = Printexc.to_string exn in
    prerr_endline (msg ^ line ^ ": " ^ exc);
    update

let read_line () =
  try Some (read_line ())
  with End_of_file -> None

let main filename =
  let dbm = Dbm.opendbm filename [Dbm.Dbm_rdwr; Dbm.Dbm_create] 0o666 in
  let rec loop update =
    match read_line () with
    | None -> update
    | Some line ->
      let update = process_line dbm line update in
      loop update
  in
  let update = loop empty_update in
  DB.update_digests dbm update;
  Dbm.close dbm

let () =
  if Array.length Sys.argv = 2 then
    let filename = Sys.argv.(1) in
    main filename
  else
    prerr_endline ("Usage: " ^ Sys.argv.(0) ^ " <filename.db>")
