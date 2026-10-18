module Task2

open Xunit
open FsUnit.Xunit

let data =
    seq {
        yield [| [| 1; 3; 5; 7 |]; [| 10; 11; 16; 20 |]; [| 23; 30; 34; 60 |] |], 3, true
        yield [| [| 1; 3; 5; 7 |]; [| 10; 11; 16; 20 |]; [| 23; 30; 34; 60 |] |], 13, false
        yield [| [| 1; 3; 5; 7 |]; [| 23; 30; 34; 60 |] |], 3, true
        yield [| [| 1; 3; 5; 7 |]; [| 10; 11; 16; 20 |] |], 1, true
        yield [| [| 1; 3; 9 |] |], 3, true
        yield [| [||]; [||] |], 1, false
        yield [||], 0, false
    }


/// Outer side array (1st-dimension)
let rec tryFindArray (matrix: int array array) target left right =
    /// Inner side array (2nd-dimension)
    let rec tryFindElement (nums: int array) target left right =
        match right - left with
        | i when i <= 0 -> nums[left] = target
        | _ ->
            let index = (left + right) / 2

            match nums[index] with
            | middle when middle < target -> tryFindElement nums target (index + 1) right
            | middle when middle > target -> tryFindElement nums target left (index - 1)
            | _ -> true

    match right - left with
    | 0 ->
        match matrix[left] with
        | [||] -> false
        | [| one |] -> one = target
        | nums when target < nums[0] -> false
        | nums when target > Array.last nums -> false
        | nums -> tryFindElement nums target 0 (nums.Length - 1)
    | i when i < 0 -> false // exception avoid
    | _ ->
        let index = (left + right) / 2

        match matrix[index] with
        | [||] -> false // for matrix, every sub array is empty
        | nums when target < nums[0] -> tryFindArray matrix target left (index - 1)
        | nums when target > Array.last nums -> tryFindArray matrix target (index + 1) right
        | nums -> tryFindElement nums target 0 (nums.Length - 1)



[<Fact>]
let Test () =
    data
    |> Seq.iter (fun (matrix, target, output) ->
        tryFindArray matrix target 0 (matrix.Length - 1) |> should equal output)
