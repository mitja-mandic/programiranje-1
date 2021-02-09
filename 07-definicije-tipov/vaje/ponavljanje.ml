let zmnozi x y = x * y

type intbool_list =
    | Int of int * intbool_list
    | Bool of bool * intbool_list
    | Empty

let testni = Int(5,Bool(true,Bool(false,Int(7, Empty))))

let rec intbool_map f_int f_bool ib_list = match ib_list with
    | Empty -> Empty
    | Int(st, rep) -> Int(f_int st,intbool_map f_int f_bool rep)
    | Bool(b, rep) -> Bool(f_bool b, intbool_map f_int f_bool rep)

let intbool_reverse lst = 
    let rec aux acc = function
        | Empty -> acc
        | Int(x, rep) -> aux (Int(x, acc)) rep
        | Bool(x, rep) -> aux (Bool(x, acc)) rep in
    aux Empty lst

let intbool_separate lst = 
    let rec aux acc1 acc2 = function
        | Empty -> (intbool_reverse acc1,intbool_reverse acc2)
        | Int(x, rep) -> aux (Int(x, acc1)) acc2 rep
        | Bool(x, rep) -> aux acc1 (Bool(x, acc2)) rep in
    aux Empty Empty lst

type magic = Fire | Frost | Arcane

type specialisation = Researcher | Historian | Teacher

type status =
    | Newbie
    | Student of magic * float
    | Employed of magic * specialisation

type wizard = {name : string; status: status}

let professor = {name = "Matija"; status = Employed(Fire, Teacher)}

type magic_counter = {fire : int; frost: int; arcane: int}

let update counter magic = match magic with
    | Fire -> {fire = counter.fire + 1; frost = counter.frost; arcane = counter.arcane}
    | Frost -> {fire = counter.fire; frost = counter.frost + 1; arcane = counter.arcane}
    | Arcane -> {fire = counter.fire; frost = counter.frost; arcane = counter.arcane + 1}


let count_magic sez = 
    let rec aux seznam count= match seznam with
        | [] -> count
        | carovnik :: ostali -> match carovnik.status with
            | Student(mag, _) -> aux ostali (update count mag)
            | Employed (mag, _) -> aux ostali (update count mag)
            | _ -> aux ostali count in
    aux sez {fire = 0; frost = 0; arcane = 0}

let zahtevana_leta = function
    | Historian -> 3.0
    | Researcher -> 4.0
    | Teacher -> 5.0


let rec find_candidate magic status lst = match lst with
    | [] -> None
    | kandidat :: ostali -> match kandidat.status with
        | Student(magija, yrs) when magija = magic -> if yrs >= zahtevana_leta status then Some kandidat.name else find_candidate magic status ostali
        | _ -> find_candidate magic status ostali
