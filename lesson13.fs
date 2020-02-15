// 39.1
let rec rmodd = function
    | [] -> []
    | [_] -> []
    | _ :: x1 :: tail -> x1 :: rmodd tail

// 39.2
let rec del_even = function
    | [] -> []
    | x :: xs when x % 2 = 0 -> del_even xs
    | x :: xs -> x :: del_even xs

// 39.3
let rec multiplicity x xs =
    match xs with
        | [] -> 0
        | head :: tail when head = x -> 1 + multiplicity x tail
        | head :: tail -> multiplicity x tail

// 39.4
let rec split = function
    | [] -> ([], [])
    | [x] -> ([x], [])
    | x1 :: x2 :: tail ->
        let (xs1,xs2) = split tail
        (x1 :: xs1, x2 :: xs2)

// 39.5
exception ZipExcLengthUnequal
let rec zip (xs1,xs2) =
    if List.length xs1 <> List.length xs2 then raise ZipExcLengthUnequal
    match (xs1,xs2) with
        | ([],[]) -> []
        | (x1 :: tail1, x2 :: tail2) -> (x1,x2) :: zip (tail1,tail2)
