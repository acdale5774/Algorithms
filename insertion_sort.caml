(* insertion sort *)

let rec insert x l cmp =
    match l with
        [] -> [x]
        | h::t -> if cmp x h
            then 
                [x] @ l
            else
                [h] @ insert x t cmp;;

let rec i_sort_inner l s_l cmp =
    match l with
        [] -> s_l
        | h::t -> i_sort_inner t (insert h s_l cmp) cmp;;

let insertion_sort l cmp = i_sort_inner l [] cmp;;
