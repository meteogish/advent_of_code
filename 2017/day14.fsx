open System.Collections
open System.Linq
#load "day10.fsx"

let input = "oundnydw"

let width = 128
let gridCells = { 0 .. width - 1 }

let getBits input =
    let toBits c =
        let hex = System.Convert.ToInt32 (c |> string, 16) 
        System.Convert.ToString(hex, 2).PadLeft(4, '0')

    input |> Seq.collect toBits

let prepareGridBits input =
    gridCells |> Seq.collect (sprintf "%s-%d" input >> Day10.knotHash >> getBits)

let getNeighbours (i, j) =
    let previousRow = i - 1
    let nextRow = i + 1
    let left = j - 1
    let right = j + 1

    seq {
        if previousRow >= 0 then yield (previousRow, j)
        if nextRow < width then yield (nextRow, j)
        if left >= 0 then yield (i, left)
        if right < width then yield (i, right)        
    }

let bits = prepareGridBits input |> Array.ofSeq

let isSet (i, j) =
    bits.[i * width + j] = '1'

let rec findFriendsOf set item =
    if (Set.contains item set) then
        set
    else
        item |> getNeighbours |> Seq.filter isSet |> Seq.fold findFriendsOf (set.Add item)

let rec countConnectedRegions (count, set) point =
    if Set.contains point set || (point |> isSet |> not) then 
        (count, set)
    else 
        (count + 1, findFriendsOf set point)

let result = 
    gridCells 
    |> Seq.allPairs gridCells 
    |> Seq.fold countConnectedRegions (0, Set.empty) 

result |> snd |> Set.count |> printfn "AnswerPartOne: %d"
result |> fst |> printfn "AnswerPartTwo: %d"