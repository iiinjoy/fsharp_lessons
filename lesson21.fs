// 50.2.1
let fac_seq =
    let rec fact acc n = seq {
        yield acc
        yield! fact (acc * n) (n + 1)
    }
    fact 1 1

// 50.2.2
let seq_seq =
    let rec f n = seq {
        if n % 2 = 0 then yield n/2
        else yield -(n+1)/2
        yield! f (n+1)
    }
    f 0
