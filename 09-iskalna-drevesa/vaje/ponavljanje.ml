type 'a tree = Prazno | Vozlisce of 'a tree * 'a * 'a tree

let list x= Vozlisce(Prazno, x, Prazno)

let testno_drevo = Vozlisce( Vozlisce(list 0, 2, Prazno), 5, (Vozlisce(list 6,7,list 11)))

let rec mirror tree = match tree with
    | Vozlisce(l1,c,l2) -> Vozlisce(mirror l2, c, mirror l1)
    | _ -> Prazno

let rec height tree = match tree with
    | Prazno -> 0
    | Vozlisce(l1,c,l2) -> 1 + max (height l1) (height l2)

let rec size tree = match tree with
    | Prazno -> 0
    | Vozlisce(l1,_,l2) -> 1 + (size l1) + (size l2)

let rec map_tree f tree = match tree with
    | Prazno -> Prazno
    | Vozlisce(l, x, r) -> Vozlisce(map_tree f l, f x, map_tree f r)

let rec list_of_tree tree = match tree with
    | Prazno -> []
    | Vozlisce(l,x,r) -> (list_of_tree l) @ [x] @ (list_of_tree r)   

let is_bst tree =
    let seznam = list_of_tree tree in
    if seznam = List.sort compare seznam then true else false

let rec member int tree = match tree with
    | Prazno -> false
    | Vozlisce(l, x, r) -> if int = x then true
        else if int > x then member int l
        else member int r

let rec insert int tree = match tree with
    | Prazno -> list int
    | Vozlisce(l,x,r) -> if int > x then Vozlisce(insert int l, x, r)
        else if int < x then Vozlisce(l, x, insert int r) else
        failwith "je že notri"

let rec max tree = match tree with
    | Prazno -> None
    | Vozlisce(Prazno, x, _) -> Some x
    | Vozlisce(l,_,_) -> max l

let rec min tree = match tree with
    | Prazno -> None
    | Vozlisce(_,x,Prazno) -> Some x
    | Vozlisce(_,_,r) -> min r

let succ = function
    | Prazno -> None
    | Vozlisce(_,_, r) -> max r


