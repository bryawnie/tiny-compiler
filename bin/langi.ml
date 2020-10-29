open Compiler.Parse
open Compiler.Interp

let () =
  let args = Sys.argv in
  if Array.length args > 1 && Sys.file_exists args.(1)
  then
    sexp_list_from_file args.(1)
    |> parse_prog
    |> interp_prog
    |> pp_value Fmt.stdout
  else
    Printf.printf "usage: langi.exe <filename>\n"