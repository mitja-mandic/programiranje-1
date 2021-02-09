let rec randlist len max = 
    if len <= 0 then []
    else (Random.int max) :: randlist (len-1) max

let rec insert seznam x = match seznam with
    | [] -> [x]
    | y :: ys -> if x >= y then y :: insert ys x else x :: seznam

let insert_sort' l = List.fold_left (fun ze_sorted x -> insert ze_sorted x) [] l

let insert_sort l =
    let rec aux acc sez = match sez with
        | [] -> acc
        | x :: xs -> 
            aux (insert acc x) xs in
    aux [] l


let rec odstrani_prvo sez x = match sez with
    | [] -> []
    | y :: ys -> if y = x then ys else y :: odstrani_prvo ys x

let min_and_rest l = 
    if l = [] then None else
    let rec min sez trenutni = match sez with
        | [] -> trenutni
        | x :: xs -> if x >= trenutni then min xs trenutni else min xs x in
    let minimum = min l (List.hd l) in
    Some (minimum, odstrani_prvo l minimum)

let rec selection_sort l = match min_and_rest l with
    | None -> []
    | Some (x, ostali) -> x :: selection_sort ostali

let swap array i j =
    let z = array.(j) in
    array.(j) <- array.(i);
    array.(i) <- z

let index_min a lower upper =
    let trenutni_min = ref lower in
    for i = lower to upper do
        if a.(i) < a.(!trenutni_min) then
        trenutni_min := i else ()
        done;
    !trenutni_min

let selection_sort_array a =
    let dolzina = (Array.length a) - 1 in
    for i = 0 to dolzina do
        let indeks_min = index_min a i dolzina in
        if a.(i) < a.(indeks_min) then swap a i indeks_min else ()
        done