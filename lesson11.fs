type F =
    | AM
    | PM

type TimeOfDay = { hours : int; minutes : int; f: F }

let (.>.) x y =
    let to_minutes (t: TimeOfDay) =
        if t.f = AM then (t.hours * 60 + t.minutes) % (24*60)
        else ((t.hours + 12) * 60 + t.minutes) % (24*60)
    (to_minutes x) > (to_minutes y)
