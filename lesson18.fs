// 47.4.1
let f n =
    if n < 0 then 0
    else
        let i = ref n
        let mutable p = 1;
        while ! i > 0 do
            p <- p * ! i
            i := ! i - 1
        p

// 47.4.2
let fibo n =
    if n <= 1 then n
    else
        let mutable n1 = 0
        let mutable n2 = 1
        let mutable f = n1 + n2
        let mutable i = 1
        while i < n do
            f <- n1 + n2
            n1 <- n2
            n2 <- f
            i <- i+1
        f
