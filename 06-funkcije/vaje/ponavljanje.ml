let reverse l =
    let rec aux acc lst = match lst with
        | [] -> acc
        | x :: xs -> aux (x::acc) xs in
    aux [] l

let rec range n = if n >=0 then n :: range(n-1) else [] |> reverse

let rec map f lst = match lst with
    | [] -> []
    | x :: xs -> f x :: map f xs

let map_tlrec f seznam =
    let rec aux acc = function
        | [] -> acc
        | x :: xs -> aux (f x :: acc) xs in
    aux [] seznam

let mapi f lst =
    let rec aux acc index = function
        | [] -> acc |> reverse
        | x :: xs -> aux ((f x index)::acc) (index+1) xs in
    aux [] 0 lst

let rec zip l1 l2 = match (l1, l2) with
    | ([],[]) -> []
    | (x::xs, y::ys) -> (x,y) :: zip xs ys
    | _ -> failwith "ne dela"

let unzip_tlrec lst = 
    let rec aux acc1 acc2 = function
        | [] -> (acc1 |>reverse, acc2|>reverse)
        | (x,y)::xs -> aux (x::acc1) (y::acc2) xs in
    aux [] [] lst

let first f default list =
    let rec aux = function
        | [] -> default
        | x::xs -> if f x then x else aux xs in
    aux list