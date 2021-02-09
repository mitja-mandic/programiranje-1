let rec multiply sez = List.fold_left (fun x y -> x*y) 1 sez

let rec insert x k list = match list with
    | hd :: tl when k > 0 -> if k > 1 then hd :: (insert x (k - 1) tl) else hd :: x :: tl
    | _ -> x :: list


let rec max_on_components sez1 sez2 = match (sez1, sez2) with
    | (_,[]) | ([],_) -> []
    | (x::xs, y::ys) -> (if x > y then x else y) :: max_on_components xs ys