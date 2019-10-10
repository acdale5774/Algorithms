(* merge sort *)

let rec merge l1 l2 cmp =
    match l1,l2 with
        [],[] -> []
        | h1::t1,[] -> [h1] @ merge t1 l2 cmp
        | [],h2::t2 -> [h2] @ merge l1 t2 cmp
        | h1::t1,h2::t2 ->
                if cmp h1 h2
                    then
                    [h1] @ merge t1 l2 cmp
                else
                    [h2] @ merge l1 t2 cmp;;

let rec makeSingles l =
    match l with
        [] -> []
        | h::t -> [[h]] @ makeSingles t;;

(* assumes the list of lists is already sorted *)
let rec merger ll tmp cmp =
    match ll,tmp with
        [],[] -> []
        | h1::t1,[] -> merger t1 h1 cmp
        | [],h2::t2 -> [merge [] tmp cmp]
        | h1::t1,h2::t2 -> [merge h1 tmp cmp] @ merger t1 [] cmp;;

(* assumes a list of lists *)
let rec mergeSortInner ll cmp =
    match ll with
        [] -> []
        | h::t -> let merged = (merger ll [] cmp) in
            match merged with
                [l] -> l
                | _ -> mergeSortInner merged cmp;;

let mergeSort l cmp = mergeSortInner (makeSingles l) cmp;;

