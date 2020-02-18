// 42.3
let rec allSubsets n k =
    match (n,k) with
        | (_,0) -> Set.empty
        | (n,1) -> Set.fold (fun acc x -> Set.add (set [x]) acc) Set.empty (set [1..n])
        | (n,k) when n = k -> set [set [1..k]]
        | (n,k) -> Set.union (allSubsets (n-1) k) (Set.fold (fun acc x -> Set.add (Set.add n x) acc) Set.empty (allSubsets (n-1) (k-1)))
