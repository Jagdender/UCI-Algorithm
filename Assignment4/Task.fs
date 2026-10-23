module Task

open Xunit
open System
open System.IO

[<CustomEquality>]
[<CustomComparison>]
type City =
    {
        name: string
        population: int
        mutable cities: City list
    }

    interface IEquatable<City> with
        member this.Equals(other: City | null) : bool =
            match other with
            | null -> false
            | other -> other.name = this.name

    interface IComparable<City> with
        member this.CompareTo(other: City | null) : int =
            match other with
            | null -> this.name.CompareTo null
            | other -> this.name.CompareTo other.name

    interface IComparable with
        member this.CompareTo(obj: obj | null) : int =
            match obj with
            | :? City as city -> (this: IComparable<City>).CompareTo city
            | _ -> ArgumentException() |> raise

    override this.Equals(obj: obj | null) : bool =
        match obj with
        | :? City as city -> (this: IEquatable<City>).Equals city
        | _ -> ArgumentException() |> raise

    override this.GetHashCode() : int = this.name.GetHashCode()

let read population_file network_file =
    let cities =
        population_file
        |> File.ReadAllLines
        |> Array.filter (System.String.IsNullOrWhiteSpace >> not)
        |> Array.map (
            _.Split(" : ")
            >> fun a ->
                a[0],
                {
                    name = a[0]
                    population = a[1] |> int
                    cities = []
                }
        )
        |> Map.ofArray

    network_file
    |> File.ReadAllLines
    |> Array.filter (System.String.IsNullOrWhiteSpace >> not)
    |> Array.map (_.Split(" : ") >> fun a -> Map.find a[0] cities, Map.find a[1] cities)
    |> Array.iter (fun (x, y) ->
        x.cities <- x.cities @ [ y ]
        y.cities <- y.cities @ [ x ])

    cities |> Map.toSeq |> Seq.map snd



let graph = read "./city_population.txt" "./road_network.txt"

[<Fact>]
let Task3 () =
    graph |> Graph.islands _.cities |> Seq.length |> printf "%d"

[<Fact>]
let Task4 () =
    graph |> Graph.islands _.cities |> Seq.map (Seq.countBy _.population)

[<Fact>]
let Task5 () =
    Graph.shortest _.cities (Seq.item 111 graph) (Seq.item 333 graph)
    |> Option.map _.Length
    |> Option.iter (printf "%d")
