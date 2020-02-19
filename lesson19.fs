// 48.4.1
let rec fibo1 n n1 n2 =
    match n with
        | 0 -> n2
        | 1 -> n1
        | n -> fibo1 (n - 1) (n1 + n2) n1

// 48.4.2
let rec fibo2 n c =
    match n with
        | 0 -> c 0
        | 1 -> c 1
        | n -> fibo2 (n - 1)
                     (fun x1 -> fibo2 (n - 2)
                                      (fun x2 -> c (x1 + x2)))

// 48.4.3
let rec bigList n k =
    List.fold (fun acc _ -> 1 :: acc) (k []) [1..n]
