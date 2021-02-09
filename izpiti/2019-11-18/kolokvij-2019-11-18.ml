(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

let is_root int1 int2 = if int1 < 0 then false else int1 * int1 = int2

let pack3 x y z = (x,y,z)

let sum_if_not f lst =
    let rec aux acc = function
        | [] -> acc
        | x :: xs -> if f x then aux (acc + x) xs else aux acc xs in
    aux 0 lst

let apply funkcije lst = 
    let rec aux acc sez_f = match sez_f with
        | [] -> acc
        | x :: xs -> 
            let rec uporabi f = function
                | [] -> []
                | y::ys -> (f y) :: (uporabi f ys) in
            let preslikan = uporabi x lst in
            let zdruzen = [preslikan] :: acc in
            aux zdruzen xs
            in
    aux [] funkcije

(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

type vrsta_srecanja = Predavanja | Vaje

type srecanje = {predmet : string; vrsta : vrsta_srecanja; trajanje : int}

type urnik = (srecanje list) list

let vaje = {predmet = "Analiza 2a";vrsta = Vaje; trajanje=3}

let predavanja = {predmet = "Programiranje 1";vrsta = Predavanja; trajanje = 2}

let urnik_profesor = [[{predmet = "Algebra 1"; vrsta = Vaje; trajanje = 2}];[];[{predmet = "Verjetnost 1"; vrsta = Predavanja; trajanje = 1}];
                        [];[];[{predmet = "Algebra 1"; vrsta = Vaje;trajanje = 1}]]

let rec je_preobremenjen = function
    | [] -> true
    | danes :: ostalo -> 
        let rec preveri_dan sez vaje pred = match sez with
            | [] -> vaje > 4 && pred > 4
            | {predmet = a; vrsta = c; trajanje = st} :: xs -> if c = Vaje then preveri_dan xs (vaje + st) pred else preveri_dan xs vaje (pred + st) in
        (preveri_dan danes 0 0) && je_preobremenjen ostalo

let bogastvo urnik =
    let rec tedensko zasluzek seznam = match seznam with
        | [] -> zasluzek
        | x :: xs -> 
        let rec dnevno_placilo acc sez = match sez with
            | [] -> acc
            | {predmet = a; vrsta = c; trajanje = st} :: xs -> if c = Predavanja then dnevno_placilo (acc + 2*st) xs else dnevno_placilo (acc + st) xs in
        tedensko (zasluzek + dnevno_placilo 0 x) xs in
    tedensko 0 urnik