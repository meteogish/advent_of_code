open System
open System.Linq

let input = 289326
//General
let right = (1, 0)
let left = (-1, 0)
let up = (0, 1)
let down = (0, -1)

let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

let rec circularTransformations = 
    let rec transformations i = seq {
        yield! Enumerable.Repeat(right, i)
        yield! Enumerable.Repeat(up, i)
        yield! Enumerable.Repeat(left, i + 1)
        yield! Enumerable.Repeat(down, i + 1)
        yield! transformations <| i + 2
    }
    transformations 1

let points = circularTransformations |> Seq.scan add (0,0)

let sumAbsPointParts (x, y) = (x |> abs) + (y |> abs)

//part 1
printfn "Part one: %d" (points |> Seq.skip (input - 1) |> Seq.head |> sumAbsPointParts)

let neighboursTransformatioins = Seq.allPairs [-1; 0; 1] [1; 0; -1]

//part 2
let neighbours (x2, y2) =
    neighboursTransformatioins 
    |> Seq.map (fun (x1, y1) -> (x1 + x2, y1 + y2)) 

let sumNeighbours (map: Map<int*int, int>) point =
    point
    |> neighbours
    |> Seq.choose map.TryFind
    |> Seq.sum

let sumNeighboursAcc firstMap =
    let mutable a = firstMap 
    fun _ point -> 
        let sum = sumNeighbours a point
        a <- a.Add (point, sum)
        sum

let acc = (Map.empty.Add ((0,0), 1))
let result = points |> Seq.skip 1 |> Seq.scan (sumNeighboursAcc acc) 0 

printfn "Part two: %d" (result |> Seq.skipWhile (fun sum -> input > sum) |> Seq.head)