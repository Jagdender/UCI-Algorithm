module Task1

open Xunit
open FsUnit.Xunit

let data =
    seq {
        yield [| 4; 9; 10; 13; 17; 17; 19; 21 |], 17, (4, 5)
        yield [| 2; 4; 6; 8; 10; 14; 16 |], 12, (-1, -1)
        yield [| 1; 3 |], 3, (1, 1)
        yield [| 1 |], 1, (0, 0)
        yield [||], 0, (-1, -1)
    }

/// Also adapted for non-sorted list
let rec _tryFind nums target baseIndex comparison =
    match nums with
    | [ one ] when one = target -> Some baseIndex
    | []
    | [ _ ] -> None
    | nums ->
        let middle = nums.Length / 2
        let left = _tryFind nums[0 .. middle - 1] target baseIndex comparison
        let right = _tryFind nums[middle..] target (middle + baseIndex) comparison

        match left, right with
        | None, None -> None
        | left, None -> left
        | None, right -> right
        | Some left, Some right -> comparison left right |> Some


let rec tryFind (nums: int array) target left right comparison =
    match right - left with
    | 0 when nums[left] = target -> Some left
    | i when i <= 0 -> None
    | _ ->
        let index = (left + right) / 2

        match nums[index] with
        | middle when middle < target -> tryFind nums target (index + 1) right comparison
        | middle when middle > target -> tryFind nums target left (index - 1) comparison
        | _ ->
            let min = tryFind nums target left index comparison
            let max = tryFind nums target (index + 1) right comparison

            match min, max with
            | Some left, Some right -> comparison left right |> Some
            | None, other
            | other, None -> other

[<Fact>]
let Test () =
    data
    |> Seq.iter (fun (nums, target, output) ->
        let boundary = nums.Length - 1
        let left = tryFind nums target 0 boundary min |> Option.defaultValue -1
        let right = tryFind nums target 0 boundary max |> Option.defaultValue -1
        (left, right) |> should equal output)
