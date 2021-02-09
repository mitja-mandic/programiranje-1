
(* a *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo, ki za trojico celih števil preveri ali tvorijo
  pitagorejsko trojico. Trojica [(a, b, c)] je pitagorejska, če je [a^2 + b^2]
  enako [c^2].

    pitagorejska_trojica : int * int * int -> bool

    # pitagorejska_trojica (3, 4, 5);;
    - : bool = true
    # pitagorejska_trojica (5, 4, 3);;
    - : bool = false

[*----------------------------------------------------------------------------*)
let pitagorejska_trojica (a,b,c) = a * a + b * b = c * c


(* b *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo, ki za celo število [x] vrne celo število [a], kjer velja,
  da koren števila [x] leži na intervalu [a, a+1).

    priblizek_korena : int -> int

      # priblizek_korena 9;;
    - : int = 3
    # priblizek_korena 17;;
    - : int = 4

[*----------------------------------------------------------------------------*)

let priblizek_korena stevilo = 
	let rec aux st acc = match acc * acc with
		| n -> if n >= st + 1 then acc - 1 else aux st (acc + 1) in
	aux stevilo 0 


(* c *)
(*----------------------------------------------------------------------------*]
  Definirajte funkcijo, ki sprejme seznam celih števil in najprej IZPIŠE vsa
  soda števila v seznamu, nato pa IZPIŠE še vsa liha števila v seznamu.
  Števila naj bodo izpisana v isti vrstici in med njimi ne želimo presledkov.

    izpisi_soda_liha : int list -> unit

    # izpisi_soda_liha [3; 1; 4; 1; 5; 9; 2];;
    4231159- : unit = ()
    # izpisi_soda_liha [2; 7; 1; 8; 2; 8; 1];;
    2828711- : unit = ()

[*----------------------------------------------------------------------------*)

let izpisi_soda_liha seznam =
	let rec aux soda liha = function
		| [] -> print_endline (soda ^ liha)
		| x :: xs -> if x mod 2 = 0 then aux (soda ^ string_of_int x) liha xs else aux soda (liha ^string_of_int x) xs in
	aux "" "" seznam
(* d *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo, ki sprejme seznam elementov tipa [option] in preveri, da
  si v seznamu izmenično sledijo konstruktorji [None] in [Some].

    alternirajoci_konstruktorji : 'a option list -> bool

    # alternirajoci_konstruktorji [None; Some 1; None; Some 100; None];;
    - : bool = true
    # alternirajoci_konstruktorji [None; Some 1; Some 10];;
    - : bool = false
    # alternirajoci_konstruktorji [Some 1; None; Some 10; None];;
    - : bool = true

[*----------------------------------------------------------------------------*)

let alternirajoci_konstruktorji seznam =
	let rec aux zadnji = function 
		| [] -> true
		| x :: xs -> match x with
			| Some y -> if Option.is_none zadnji then aux (Some y) xs else false
			| None -> if Option.is_some zadnji then aux None xs else false in
	aux (List.hd seznam) (List.tl seznam)

(* e *)
(*----------------------------------------------------------------------------*]
  Funkcija [najmanjsi_rezultat] naj za element [x] in seznam funkcij [fs] vrne
  INDEKS funkcije, ki ima pri argumentu [x] najmanjšo vrednost izmed vseh
  funkcij v seznamu [fs]. Ker je seznam morda prazen, naj bo rezultat tipa
  [option].

  Za vse točke naj bo funkcija repno rekurzivna.

    najmanjsi_rezultat : 'a -> ('a -> 'b) list -> int option

    # najmanjsi_rezultat (-10) [(fun x -> 10 * x); succ; (fun y -> -10 * y)];;  
    - : int option = Some 0
    # najmanjsi_rezultat 10 [(fun x -> 10 * x); succ; (fun y -> -10 * y)];;    
    - : int option = Some 2
    # najmanjsi_rezultat 30 [];;
    - : int option = None

[*----------------------------------------------------------------------------*)

let najmanjsi_rezultat x sez_f =
	if sez_f = [] then None else
		let rec aux mini_indeks mini_vrednost funkcije trenutni_indeks = match funkcije with
			| [] -> Some mini_indeks
			| f :: fs -> if f x <= mini_vrednost 
							then aux trenutni_indeks (f x) fs (trenutni_indeks + 1) 
						else aux mini_indeks mini_vrednost fs (trenutni_indeks+1) in
	aux 0 9999 sez_f 0

