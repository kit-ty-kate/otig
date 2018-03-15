open Lwt_result.Infix

module Reference = Git_unix.FS.Reference
module Ref = Git_unix.FS.Ref
module Value = Git_unix.FS.Value
module Commit = Git_unix.FS.Value.Commit
module Hash = Git_unix.FS.Value.Commit.Hash
module User = Git.User

let rec get_hash fs r =
  Ref.read fs r >>= function
  | _, Reference.Hash hash -> Lwt.return (Ok hash)
  | _, Reference.Ref r -> get_hash fs r

let rec get_commits fs hash =
  Git_unix.FS.read fs hash >>= function
  | Value.Commit x -> get_parents fs x >|= List.cons x
  | Value.Blob _ -> assert false
  | Value.Tree _ -> assert false
  | Value.Tag _ -> assert false
and get_parents fs commit =
  match Commit.parents commit with
  | [] -> Lwt.return (Ok [])
  | p::_ -> get_commits fs p (* TODO *)

let rec get_first_nonempty_string = function
  | ""::xs -> get_first_nonempty_string xs
  | x::_ -> x
  | [] -> ""

let get_commit_short_message commit =
  let msg = Commit.message commit in
  let msgs = String.split_on_char '\n' msg in
  get_first_nonempty_string msgs

let print_commit commit =
  let hash = Hash.to_hex (Commit.digest commit) in
  let hash = String.sub hash 0 (min 7 (String.length hash)) in
  let msg = get_commit_short_message commit in
  let author = Commit.author commit in
  let date, tz_offset_s = author.User.date in
  let date = match Ptime.of_float_s (Int64.to_float date) with
    | None -> assert false
    | Some x -> x
  in
  let (y, m, d), ((hh, mm, ss), _) = Ptime.to_date_time date in
  let tz_sign, tz_h, tz_m = match tz_offset_s with
    | Some {User.sign = `Plus; hours; minutes} -> ("+", hours, minutes)
    | Some {User.sign = `Minus; hours; minutes} -> ("-", hours, minutes)
    | None -> ("+", 0, 0)
  in
  let author = author.User.name in
  Printf.printf
    "%s - %s (%d/%02d/%02d %02d:%02d:%02d %s%02d:%02d) <%s>\n"
    hash msg y m d hh mm ss tz_sign tz_h tz_m author

let () =
  match
    Lwt_main.run begin
      Git_unix.FS.create () >>= fun fs ->
      get_hash fs Reference.head >>= fun hash ->
      get_commits fs hash >|=
      List.iter print_commit
    end
  with
  | Ok () -> ()
  | Error e -> Format.eprintf "Error: %a\n" Git_unix.FS.pp_error e; exit 1
