// 34.1
let upto n =
    let rec upto_rec = function
        | (0,arr,_) -> arr
        | (i,arr,n) -> n-i+1 :: upto_rec (i-1,arr,n)
    upto_rec (n,[],n)

// 34.2
let rec dnto = function
    | 0 -> []
    | n -> n :: dnto (n-1)

// 34.3
let evenn n =
    let rec even_rec = function
        | (0,arr,_) -> arr
        | (i,arr,n) -> (n-i)*2 :: even_rec (i-1,arr,n)
    even_rec (n,[],n)
