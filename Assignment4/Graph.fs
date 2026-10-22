module Graph

let islands<'T when 'T: comparison> (mapNext: 'T -> 'T list) =
    let rec dfs visited point =
        mapNext point
        |> List.fold
            (fun visited point ->
                match visited |> Set.add point with
                | set when set.Count > visited.Count -> dfs set point
                | _ -> visited)
            visited

    Seq.mapFold
        (fun visited point ->
            match visited |> Set.add point with
            | set when set.Count > visited.Count -> dfs Set.empty point |> Set.union set |> (fun set -> set, set)
            | _ -> Set.empty, visited)
        Set.empty
    >> fst
    >> Seq.filter (Seq.isEmpty >> not)
    >> Set.ofSeq


//let shortest<'T when 'T: comparison> (mapNext: 'T -> 'T list) (graph: 'T list) =
//    let rec bfs visited queue path point =
//        match visited |> Set.add point with
//        | set when set.Count > visited.Count -> bfs set point
//        | _ -> visited)
