module Task2

open Xunit
open FsUnit.Xunit

let data =
    seq {
        yield [ 1; 2; 3; 4; 5; 6; 7; 8 ], 7
        yield [ 1; 1; 1; 1; 1; 1; 1; 1 ], 7
    }

let start borad =
    let board = borad |> List.map ((+) (-1))

    let check board =
        match board |> List.distinct with
        | list when list.Length < board.Length -> false
        | board ->
            List.indexed board
            |> List.forall (fun (c1, r1) ->
                List.indexed board
                |> List.forall (fun (c2, r2) ->
                    match c1 >= c2 with
                    | true -> true
                    | false -> abs (c1 - c2) <> abs (r1 - r2)))

    let rec permute rest =
        seq {
            match rest with
            | [] -> yield []
            | list ->
                for (index, element) in list |> List.indexed do
                    yield! permute (rest |> List.removeAt index) |> Seq.map ((@)[element])
        }

    let count = Seq.zip board >> Seq.filter (fun (x, y) -> x <> y) >> Seq.length

    permute [ 0..7 ] |> Seq.filter check |> Seq.map count


[<Fact>]
let Solution_Count_Test () =
    data
    |> Seq.iter (fun (input, _) -> input |> start |> Seq.length |> should equal 92)


[<Fact>]
let Task_Test () =
    data
    |> Seq.iter (fun (input, output) -> input |> start |> Seq.min |> should equal output)
