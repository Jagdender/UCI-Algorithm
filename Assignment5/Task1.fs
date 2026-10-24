module Task1

open Xunit

let data =
    seq {
        yield "ab", "eidbaooo", true
        yield "ab", "eidboaoo", false
    }



let hasPermutationOf (s1: string) (s2: string) =
    let count = Array.countBy id >> Map.ofArray
    let counts = s1.ToCharArray() |> count

    s2.ToCharArray()
    |> Array.windowed s1.Length
    |> Array.map count
    |> Array.exists ((=) counts)





[<Fact>]
let Test () =
    data
    |> Seq.iter (fun (s1, s2, output) -> hasPermutationOf s1 s2 = output |> Assert.True)
