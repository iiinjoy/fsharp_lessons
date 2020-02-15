type TimeOfDay = { hours: int; minutes: int; f: string }

let (.>.) x y =
    let {hours = h1; minutes = m1; f = f1} = x
    let {hours = h2; minutes = m2; f = f2} = y
    f1 > f2 || h1 > h2 || m1 > m2
