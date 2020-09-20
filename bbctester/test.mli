
(** Expected status of a test *)
type status =
  | CTError
    (* Compile-time error *)
  | RTError
    (* Run-time error *)
  | NoError
    (* No error *)

val status_of_string : string -> status
val string_of_status : status -> string


(** Ocaml representation of test files
 * All strings are enforced to be trimmed.
 * The expected string *)
type t =
  { name : string
  ; description : string
  ; params : string list
  ; status : status
  ; src : string
  ; expected : string }

val read_test : string -> t option

(* A compiler is a function taking an output formatter and a filename *)
type compiler = Format.formatter -> string -> unit

val make_test : string -> compiler:compiler -> interpreter:(string -> string) option -> string -> string * (unit -> unit)
val testfiles_in_dir : string -> string list

(* Given the path of a C runtime file [runtime], a [compiler] and
  the path [dir] of a directory containing tests files, produces 
  unit tests for each test files in [dir]  *)
val tests_from_dir : 
  runtime:string ->
  compiler:compiler ->
  ?interpreter:(string -> string) ->
  string -> (string * unit Alcotest.test_case list) list
