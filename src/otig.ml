open Lwt_result.Infix

module Reference = Git_unix.FS.Reference
module Ref = Git_unix.FS.Ref
module Value = Git_unix.FS.Value
module Commit = Git_unix.FS.Value.Commit
module Hash = Git_unix.FS.Value.Commit.Hash

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
  Printf.printf "%s - %s\n" hash msg

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
  | Error _ -> assert false (* TODO *)
