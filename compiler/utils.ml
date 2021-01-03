let rec take (k:int) (l: 'a list) : 'a list =
  if k > 0 then
  begin match l with
  | [] -> []
  | x :: xs -> x::take (k-1) xs
  end else []

(* Removes repeated elements from list *)
let remove_repeated (l: 'a list): 'a list = 
  let unique_cons =
    fun x xs ->
      if List.mem x xs then xs else x :: xs
    in
  List.fold_right unique_cons l []
  