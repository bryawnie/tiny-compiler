let rec take (k:int) (l: 'a list) : 'a list =
  if k > 0 then
  begin match l with
  | [] -> []
  | x :: xs -> x::take (k-1) xs
  end else []