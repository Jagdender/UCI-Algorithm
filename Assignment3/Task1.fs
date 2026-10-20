module Task1

open System
open Xunit
open FsUnit.Xunit

let data =
    seq {
        [ "bucket"; "rat"; "mango"; "tango"; "ogtan"; "tar" ],
        [ [ "bucket" ]; [ "rat"; "tar" ]; [ "mango" ]; [ "tango"; "ogtan" ] ]
    }

let groupAnagram sort (strings: string list) : string list list =
    strings |> List.groupBy sort |> List.map snd

// using lib api
let _sortString (str: string) =
    str.ToCharArray() |> Array.sort |> String


[<Fact>]
let QuickSortTest () =
    let sortString (str: string) : string =
        let rec sortInPlace left right (elements: 'T[]) =
            let inline swap left right (array: 'T[]) =
                let t = array[left]
                array[left] <- array[right]
                array[right] <- t

            if left < right then
                let pivot = elements[(left + right) / 2]
                let mutable l = left
                let mutable r = right

                while l <= r do
                    while l <= right && elements[l] < pivot do
                        l <- l + 1

                    while r >= left && elements[r] > pivot do
                        r <- r - 1

                    if l <= r then
                        swap l r elements
                        l <- l + 1
                        r <- r - 1

                sortInPlace left r elements
                sortInPlace l right elements

        str.ToCharArray()
        |> (fun x ->
            x |> sortInPlace 0 (x.Length - 1)
            x |> String)

    data
    |> Seq.iter (fun (input, output) ->
        input
        |> groupAnagram sortString
        |> Set.ofList
        |> should equal (output |> Set.ofList))


[<Fact>]
let MergeSortTest () =
    let sortString (str: string) : string =
        let rec sort (elements: 'T array) =
            let rec merge (left: 'T array) (right: 'T array) i j (merged: 'T array) =
                if i >= left.Length then
                    if j >= right.Length then
                        merged
                    else
                        merged[i + j] <- right[j]
                        merge left right i (j + 1) merged
                elif j >= right.Length then
                    merged[i + j] <- left[i]
                    merge left right (i + 1) j merged
                elif left[i] < right[j] then
                    merged[i + j] <- left[i]
                    merge left right (i + 1) j merged
                else
                    merged[i + j] <- right[j]
                    merge left right i (j + 1) merged


            match elements with
            | [||]
            | [| _ |] -> elements
            | elements ->
                let left, right = elements |> Array.splitAt (elements.Length / 2)

                merge (sort left) (sort right) 0 0 (Array.zeroCreate (left.Length + right.Length))

        str.ToCharArray() |> (sort >> String)


    data
    |> Seq.iter (fun (input, output) ->
        input
        |> groupAnagram sortString
        |> Set.ofList
        |> should equal (output |> Set.ofList))
