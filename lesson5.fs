// 16.1
let notDivisible = function
    | (0,m) -> false
    | (n,m) -> m % n = 0

// 16.2
let prime n =
    let rec isPrime = function
        | (n,_) when n < 2 -> false
        | (2,_) -> true
        | (n,i) when i*i <= n && n % i = 0 -> false
        | (n,i) when i*i <= n -> isPrime (n,i+1)
        | _ -> true
    isPrime (n,2)
