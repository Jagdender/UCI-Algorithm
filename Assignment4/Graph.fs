module Graph

let islands<'T when 'T: comparison> (mapNext: 'T -> 'T seq) =
    let rec dfs visited point =
        mapNext point
        |> Seq.fold
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


let shortest<'T when 'T: comparison> (mapNext: 'T -> 'T list) start target =
    let rec bfs visited queue =
        match queue with
        | path :: _ when path |> List.last = target -> Some path
        | path :: rest ->
            bfs
                (visited |> Set.add (path |> List.last))
                (rest
                 @ (path
                    |> List.last
                    |> mapNext
                    |> List.filter (fun x -> visited |> Set.contains x |> not)
                    |> List.map (fun x -> path @ [ x ])))
        | _ -> None

    bfs Set.empty [ [ start ] ]
