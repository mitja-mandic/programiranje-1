let odstej_trojici tr1 tr2=
    let (x,y,z) = tr1 in
    let (x',y',z') = tr2 in
    (x-x',y-y',z-z')

let rec max_rezultat_do_n f n =
    let rec aux seznam n = match n with
        | k when k < 0 -> seznam
        | _ -> aux ((f n)::seznam) (n - 1) in
    List.hd(List.sort compare (aux [] n))

let alt_max f n = 
    let rec aux trenutni_max trenutni_i =
        if trenutni_i < 0 then trenutni_max
        else (
            let y = f trenutni_i in
                if y > trenutni_max then aux y (trenutni_i - 1)
                else aux trenutni_max (trenutni_i - 1)
        ) in
    aux (f n) (n-1)

let rec pocisti_seznam seznam = match seznam with
    | [] -> []
    | x :: xs -> match x with
        | Some x -> x :: pocisti_seznam xs
        | None -> pocisti_seznam xs

let pocisti_seznam_tlrec seznam = 
    let rec aux acc = function
        | [] -> List.rev acc
        | (Some x)::xs -> aux (x::acc) xs
        | _::xs -> aux acc xs in
    aux [] seznam

let preveri_urejenost_neucinkovito sez = 
    let rec aux seznam lihi sodi = match seznam with
        | [] -> if (List.sort compare lihi = lihi) && (List.sort compare sodi = sodi) then true else false
        | x :: xs -> if x mod 2 = 0 then aux xs lihi (x::sodi) else aux xs (x::lihi) sodi in
    aux sez [] []

let preveri_urejenost_1 seznam = 
    let rec aux sez max_sod min_lih = match sez with
        | [] -> true
        | x :: xs -> if x mod 2 = 0 then
            if x < max_sod then false else aux xs x min_lih
            else
                if x > min_lih then false else aux xs max_sod x in
    aux [] 0 

let preveri_urejenost seznam =
    let rec aux sez min_sod max_lih = match sez with
        | [] -> true
        | x :: xs -> if x mod 2 = 0 then x > min_sod && aux xs x max_lih else x < max_lih && aux xs min_sod x in
    aux seznam min_int max_int

(*  DRUGA NALOGA  *)

type 'a gnezdenje = 
    | Element of 'a
    | Podseznam of 'a gnezdenje list

let gnezdenje_primer = Podseznam[ Element 1; Element 2;
    Podseznam[Element 3;Podseznam[Element 4]; Podseznam[]];
    Podseznam[Element 5]
    ]


let rec najvecja_globina_gnezdenje seznam = match seznam with
    | Element _ -> 0
    | Podseznam xs -> 1 + (List.fold_left max 1 (List.map najvecja_globina_gnezdenje xs))

let najvecja_globina g_list = 
    1 + (List.fold_left max 1 (List.map najvecja_globina_gnezdenje g_list))

let rec preslikaj f g = match g with
    | Element x -> Element (f x)
    | Podseznam xs -> Podseznam((List.map (preslikaj f)) xs)

let rec splosci = function
    | Element x -> [x]
    | Podseznam xs ->
        let splosceni = List.map splosci xs in
        (*List.concat splosceni*)
        List.fold_left (@) [] splosceni

let rec alternirajoci_konstruktorji l = function
    | [] -> true
    | [x] -> true
    | Element _ :: Podseznam p :: xs -> alternirajoci_konstruktorji ((Podseznam p)::xs)
    | Podseznam _ :: Element p :: xs -> alternirajoci_konstruktorji ((Element p)::xs)
    | _ -> false

(*let zlozi_preko_gnezdenja f acc g_list =*)