open Lwt_result.Infix

module Reference = Git_unix.FS.Reference
module Ref = Git_unix.FS.Ref
module Value = Git_unix.FS.Value
module Commit = Git_unix.FS.Value.Commit

let rec get_hash fs r =
  Ref.read fs r >>= function
  | _, Reference.Hash hash -> Lwt.return (Ok hash)
  | _, Reference.Ref r -> get_hash fs r

let rec get_commits fs hash =
  Git_unix.FS.read fs hash >>= function
  | Value.Commit x -> get_parents fs x >|= fun acc -> x :: acc
  | Value.Blob _ -> assert false
  | Value.Tree _ -> assert false
  | Value.Tag _ -> assert false
and get_parents fs commit =
  match Commit.parents commit with
  | [] -> Lwt.return (Ok [])
  | p::_ -> get_commits fs p (* TODO *)

let () =
  match
    Lwt_main.run begin
      Git_unix.FS.create () >>= fun fs ->
      get_hash fs Reference.head >>= fun hash ->
      get_commits fs hash >>= fun commits ->
      let msgs = List.map Commit.message commits in
      List.iter print_endline msgs;
      Lwt.return (Ok ())
    end
  with
  | Ok () -> ()
  | Error _ -> assert false (* TODO *)
