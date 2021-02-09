let option_sum st1 st2 = match (st1, st2) with
    | (Some x, Some y) -> Some (x + y)
    | _ -> None

let twostep_map f g h a =
    let prvi, drugi = f a in
    (g prvi, h drugi) 

let function_repeat f xs = 
    let rec aux sez acc = match sez with
        | [] -> acc
        | y :: ys ->
            if f y <= 0 then aux ys acc 
            else 
                let rec ponovitve x n seznam= match n with
                    | 0 -> seznam
                    | m -> ponovitve x (m - 1) (x::seznam) in
                aux ys (ponovitve y (f y) acc) in
    aux xs []

let rec iterate f stop x = if not (stop x) then iterate f stop (f x) else x

type 'a  improved_list = 
    | Prazno
    | Vozlisce of ('a array * 'a improved_list)

let test = Vozlisce( [| 1; 2; 20 |], Vozlisce([|17;19;20;30|], Vozlisce([|100|],Prazno)))

let rec count lst = match lst with
    | Prazno -> 0
    | Vozlisce(tab,sez) -> (Array.length tab) + count sez

let rec nth n lst = match(n, lst) with
    | (_,Prazno) -> None
    | (n, Vozlisce(tabela, preostanek)) -> 
                if n <= (Array.length tabela) 
                    then Some tabela.(n)
                else
                    nth (n - Array.length tabela) preostanek

let rec update lst index a = match (index, lst) with
    | (n , Vozlisce(tabela, preostanek)) -> 
        let nova_tabela = tabela in
        if n <= (Array.length tabela) then
            (nova_tabela.(n) <- a;
            Vozlisce(nova_tabela, preostanek))
        else
            Vozlisce(nova_tabela, update preostanek (n - Array.length tabela) a)
    | _ -> failwith "yikes"
    
