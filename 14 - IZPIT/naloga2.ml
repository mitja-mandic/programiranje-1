(*============================================================================*]
  Za učinkovitejše iskanje po leksikografsko urejenih parih bomo uporabili
  leksikografska drevesa, ki jih ustvarimo s pomočjo dvojiških dreves.

    type 'a tree = Empty | Node of 'a tree * 'a * 'a tree

  Leksikografsko drevo za pare tipa ['a * 'b] je dvojiško drevo, ki ima v
  vozlišču element tipa ['a] (da lahko primerjamo po prvi komponenti) in pa
  drevo tipa ['b tree] (za primerjanje po drugi komponenti).

    type ('a, 'b) lexi_tree = ('a * 'b tree) tree

  Par [(a, b)] se nahaja v leksikografskem drevesu, če imamo v drevesu vozlišče
  s parom [(a, subtree)] in se [b] nahaja v [subtree]. 

  Primer drevesa za pare (3, "g"), (3, "t"), (7, "a"), (10, "e"), (10, "r"),
  (10, "t") in (10, "z") je:
          
          (7)--------┐
           |   "a"   |
           └---------┘
          /           \
         /             \
    (3)-------┐     (10)-----------┐
     | "g"    |      |     "r"     |
     |    \   |      |    /   \    |
     |    "t" |      |  "e"   "z"  |
     └--------┘      |       /     |
                     |     "t"     |
                     └-------------┘

[*============================================================================*)

type 'a tree = Empty | Node of 'a tree * 'a * 'a tree

type ('a, 'b) lexi_tree = ('a * 'b tree) tree


(* a *)
(*============================================================================*]
  Definirajte primer, ki ustreza zgornjemu leksikografskemu drevesu.

[*============================================================================*)
let leaf x = Node(Empty, x, Empty)

let test = Node(leaf(3, Node(Empty, "g", leaf"t")),
				(7, leaf "a"),
				leaf(10, Node(leaf "e", "r", Node(leaf "t", "z", Empty)))
				)

(* b *)
(*============================================================================*]
  Napišite funkcijo, ki preveri ali je par prisoten v leksikografskem drevesu.
[*============================================================================*)
(*funkcija member je iz vaj *)
let rec member x = function
    | Empty -> false
    | Node (l, y, r) -> 
        if x < y then member x r else
        if y < x then member x l else
        true

let rec poisci_par (stevilka, crka) drevo = match drevo with
	| Empty -> false
	| Node(l, (int, drev) , d) ->
						if int = stevilka then member crka drev else
						
						if stevilka < int then poisci_par (stevilka, crka) l else
						
						poisci_par(stevilka, crka) d

(* c *)
(*============================================================================*]
  Napišite funkcijo za vstavljanje elementov v leksikografsko drevo.
[*============================================================================*)
(*funkcija insert je iz vaj *)
let rec insert x = function
    | Empty -> leaf x
    | Node (l, y, r) -> 
        if x < y then Node (insert x l,y,r) else
        if y < x then Node (l,y,insert x r) else
        Node(l,x,r)


let rec vstavi (stevilka, crka) drevo = match drevo with
	| Empty -> leaf(stevilka, leaf crka)
	| Node(l, (int, drev), d) -> 
				if int = stevilka then Node(l, (int, insert crka drev), d) else
				if stevilka < int then Node(vstavi (stevilka, crka) l, (int,drev), d) else
				Node(l, (int,drev), vstavi (stevilka, crka) d)
								
(* d *)
(*============================================================================*]
  Napišite funkcijo [lexi_fold], ki sprejme funkcijo [f] in začetno vrednost
  akumulatorja, nato pa funkcijo zloži preko leksikografskega drevesa. Vrstni
  red zlaganja je določen z leksikografsko urejenostjo.

    lexi_fold : ('a -> 'b -> 'c -> 'a) -> 'a -> ('b, 'c) lexi_tree -> 'a
[*============================================================================*)
(*funkcija map_tree je iz vaj *)

let rec list_of_tree st= function
     | Empty -> []
     | Node(l,x,r) -> (list_of_tree st l) @ [ (st,x)] @ (list_of_tree st r)

let rec sez_iz_drev = function
	| Empty -> []
	| Node(l, (int, drevo), d) -> (sez_iz_drev l) @ [list_of_tree int drevo] @ (sez_iz_drev d)

let rec lexi_fold f akumulator drevo =
	let sez_drevesa = sez_iz_drev drevo in
	let rec uporabi_na_sez funkcija acc = function
		| [] -> acc
		| x :: xs -> let nov_acc = funkcija x acc in
		uporabi_na_sez f nov_acc xs in
	uporabi_na_sez f akumulator (List.rev sez_drevesa)
(* e *)
(*============================================================================*]
  Napišite funkcijo, ki vrne urejen seznam vseh elementov, ki se nahajajo v
  leksikografskem drevesu.
[*============================================================================*)
let rec list_of_tree st= function
     | Empty -> []
     | Node(l,x,r) -> (list_of_tree st l) @ [ (st,x)] @ (list_of_tree st r)

let rec sez_iz_drev = function
	| Empty -> []
	| Node(l, (int, drevo), d) -> (sez_iz_drev l) @ [list_of_tree int drevo] @ (sez_iz_drev d)
