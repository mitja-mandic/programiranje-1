let angle_between (x1,y1) (x2,y2) =
    let dolzina_1  = (x1**2. +. y1**2.) ** (0.5) in
    let dolzina_2 = (x2**2. +. y2**2.) ** (0.5) in
    let skalarni = x1*.x2 +. y1*.y2 in
    Float.acos (skalarni /. (dolzina_1 *. dolzina_2))

(* let list_to_triple lst =
    if List.length lst <> 3 then None else
    let a::b::c::[] = lst in Some(a,b,c) *)

type counter = {lt: int;eq: int; gt: int}

let compare_with lst x =
    let rec aux lt eq gt = function
        | [] -> {lt = lt;eq = eq; gt = gt}
        | y :: ys -> if y = x then aux lt (eq+1) gt ys else 
                    if y > x then aux lt eq (gt + 1) ys else
                    aux (lt + 1) eq gt ys in
    aux 0 0 0 lst

let apply_all seznam_funkcij x =
    let rec aux sez acc = match sez with
        | [] -> acc
        | f::fs -> aux fs (f x) in
    aux seznam_funkcij x

type xytree = 
    | Xsplit of int * xytree * xytree
    | Ysplit of int * xytree * xytree
    | Elements of (int * int) list

