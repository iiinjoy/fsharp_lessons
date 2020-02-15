type TimeOfDay = { hours: int; minutes: int; f: string }

let (.>.) x y =
    let to_minutes {hours = hh; minutes = mm; f = ff} =
        if ff = "AM" then (hh * 60 + mm) % (24*60)
        else ((hh + 12) * 60 + mm) % (24*60)
    let from_minutes m =
            let ff = if m >= (12*60) then "PM" else "AM"
            let hh = (m % (12*60)) / 60
            let mm = (m % (12*60)) - hh * 60
            { hours = hh; minutes = mm; f = ff }
    (to_minutes x) > (to_minutes y)
