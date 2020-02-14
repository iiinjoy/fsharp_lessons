// 20.3.1
let vat n x =
    if n <= 0 then x
    elif n >= 100 then x * 2.0
    else x * (1.0 + (float n)/100.0)

// 20.3.2
let unvat n x =
    if n <= 0 then x
    elif n >= 100 then x / 2.0
    else x / (1.0 + (float n)/100.0)

// 20.3.3
let rec min f =
    let rec min_rec f n =
        if f n = 0 then n
        else min_rec f (n+1)
    min_rec f 0
