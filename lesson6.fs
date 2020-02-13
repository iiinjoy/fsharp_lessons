// 17.1
let rec pow = function
    | (_,n) when n <= 0 -> ""
    | (s,1) -> s
    | (s,n) -> s + pow (s,n-1)

// 17.2
let rec isIthChar = function
    | (_,n,_) when n < 0 -> false
    | (s,n,_) when n >= (String.length s) -> false
    | (s,n,c) -> s.[n] = c

// 17.3
let rec occFromIth = function
    | (_,n,_) when n < 0 -> 0
    | (s,n,_) when n >= (String.length s) -> 0
    | (s,n,c) when isIthChar (s,n,c) -> 1 + occFromIth (s,n+1,c)
    | (s,n,c) -> occFromIth (s,n+1,c)
