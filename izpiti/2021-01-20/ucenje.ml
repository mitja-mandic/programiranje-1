(* PRVA NALOGA*)
let razlika_produkta_in_vsote x y = x*y - (x + y)
let zlimaj_para par1 par2 =
    let (a,b) = par1 in
    let (c,d) = par2 in
    (a,b,c,d)

let nedeljivo_do x n =
    let rec aux indeks = 
    if indeks < n then
        if x mod indeks = 0 then false else aux (indeks + 1)
    else
        true in
    aux 2

let trojica_graficno (a,b,c) = 
    let option_to_string opt = match opt with
        | None -> "-"
        | Some x -> string_of_int x in
    "("^ option_to_string a ^"," ^option_to_string b ^","^option_to_string c ^")"

let razcepi_pri_None seznam = 
    let rec aux acc trenutni = function
        | [] -> acc
        | x :: xs -> match x with
            | None -> aux (acc @ [List.rev trenutni]) [] xs
            | Some x -> aux acc (x::trenutni) xs in
    aux [] [] seznam


(*druga naloga*)
type 'a kuhinjski_element = 
    | Ponev of 'a
    | Lonec of 'a * 'a
    | Omara of 'a list

let kuhinja = [Ponev("tuna"); Lonec("brokoli", "mango"); Omara["sir";"toast";"sok";"ragu"]]

let rec prestej = function
    | [] -> 0
    | Ponev(_) :: xs -> 1 + prestej xs
    | Lonec(_,_) :: xs -> 2 + prestej xs
    | Omara(lst) :: xs -> (List.length lst) + prestej xs

let pretvori f element = match element with
    | Ponev(x) -> Ponev(f x)
    | Lonec(x,y) -> Lonec (f x, f y)
    | Omara lst -> Omara (List.map f lst)

let rec zdruzi sez1 sez2 = match sez1 with
    | [] -> sez2
    | x :: xs -> zdruzi xs (x::sez2)

let pospravi seznam =
    let rec aux acc sez= match sez with
        | [] -> Omara(acc)
        | Ponev(x) :: xs -> aux (x::acc) xs
        | Lonec(x, y) :: xs -> aux (x::y::acc) xs
        | Omara(lst) :: xs -> aux (zdruzi lst acc) xs in
    aux [] seznam

let oceni seznam f =
    let rec aux trenutna_vsota = function
        | [] -> trenutna_vsota
        | Ponev(x) :: xs-> aux (trenutna_vsota + f x) xs
        | Lonec(x,y) :: xs -> aux (trenutna_vsota + 3 * ((f x) + (f y))) xs
        | Omara(lst) :: xs ->
            let predelan = List.map f lst in
            let vsota = 5 * List.fold_left (+) 0 predelan in
            aux (trenutna_vsota + vsota) xs in
    aux 0 seznam

