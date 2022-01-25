
exception ZipErr of string

let rec zip a b =
  match (a, b) with
  | [], [] -> []
  | x::xs, y::ys -> (x, y) :: (zip xs ys)
  | _ -> raise (ZipErr "badly matched lists in zip")

let arr_zip x y = Array.init (Array.length x) (fun i -> x.(i), y.(i))
