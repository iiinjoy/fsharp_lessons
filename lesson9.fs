// 23.4.1
let to_coppers (g, s, c) = g * 240 + s * 12 + c
let from_coppers cc =
    let g = cc / 240
    let s = (cc - g * 240) / 12
    let c = cc - g * 240 - s * 12
    (g, s, c)
let (.+.) x y = (to_coppers x) + (to_coppers y) |> from_coppers
let (.-.) x y = (to_coppers x) - (to_coppers y) |> from_coppers

// 23.4.2
let (.+) x y =
    let (a,b): float*float = x
    let (c,d): float*float = y
    (a+c, b+d)
let (.-) x y =
    let (a,b): float*float = x
    let (c,d): float*float = y
    (.+) (a,b) (-c,-d)
let (.*) x y =
    let (a,b): float*float = x
    let (c,d): float*float = y
    (a*c - b*d, b*c + a*d)
let (./) x y =
    let (c,d): float*float = x
    let (a,b): float*float = y
    (.*) (c,d) (a/(a*a+b*b),-b/(a*a+b*b))
