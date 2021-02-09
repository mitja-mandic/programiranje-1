let dot_prod (x1,y1,z1) (x2,y2,z2) = x1*.x2 +. y1*.y2 +. z1*.z2

let rec combine_and_filter f xs ys = match (xs,ys) with
    | (_,[]) | ([],_) -> []
    | (prvi :: xrep, drugi :: yrep) -> match f prvi drugi with
                                        | None -> combine_and_filter f xrep yrep
                                        | Some x -> x :: combine_and_filter f xrep yrep

let conditional_print f seznam_nizov =
    let rec aux acc sez = match sez with
        | [] -> print_endline acc
        | x :: xs -> if f x then aux (x^acc) xs else aux acc xs in
    aux "" seznam_nizov

type ('a,'b) tree = 
    | Empty
    | ANode of ('a,'b) tree * 'a * ('a, 'b) tree
    | BNode of ('a,'b) tree * 'b * ('a, 'b) tree

let test = ANode(
        BNode(Empty, true, Empty),
        12,
        ANode(
            ANode(Empty, 0, Empty),
            5,
            BNode(Empty,false,Empty)
            )
        )

let rec adepth = function
    | ANode(l,_,d) -> 1 + max (adepth l) (adepth d)
    | _ -> 0

let rec bdepth = function
    | BNode(l,_,d) -> 1 + max (bdepth l) (bdepth d)
    | _ -> 0

type result = {aNodes : int; bNodes : int}

let count tree =
    let rec prestej_a drevo = match drevo with
        | Empty -> 0
        | ANode(l,_,d) -> 1 + prestej_a l + prestej_a d
        | BNode(l,_,d) -> prestej_a l + prestej_a d in
    let rec prestej_b drevo = match drevo with
        | Empty -> 0
        | ANode(l,_,d) -> prestej_a l + prestej_a d
        | BNode(l,_,d) -> 1 + prestej_a l + prestej_a d in
    {aNodes = prestej_a tree;bNodes = prestej_b tree}

let rec is_typemirror t1 t2 = match (t1,t2) with
    | (ANode(l,_,d), BNode(l',_,d')) | (BNode(l,_,d), ANode(l',_,d')) -> is_typemirror l l' && is_typemirror d d'
    | (Empty, Empty) -> true
    | _-> false
