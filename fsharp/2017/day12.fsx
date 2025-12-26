//https://adventofcode.com/2017/day/12

open System.Text.RegularExpressions

let day12 = System.IO.File.ReadAllLines("inputs/input_day12.txt")

let regex = Regex ("(\d+) <-> ((?:\d+)(?:,\s*\d+)*)?", RegexOptions.Compiled);

let parseLine str =
    let m = regex.Match (str)
    let get (n: int) = (m.Groups.[n]).Value 

    (get 1 |> int, (get 2).Split(", ") |> Array.map int)

let items = day12 |> Seq.map parseLine |> Map.ofSeq 

let rec findFriendsOf set item =
    if Set.contains item set then
        set
    else 
        items.[item] |> Seq.fold findFriendsOf (set.Add item)

let countIndependentGroups (count, set) item =
    if Set.contains item set then
        (count, set)
    else 
        (count + 1, findFriendsOf set item)

findFriendsOf Set.empty 0 |> Seq.length 
|> printfn "AnswerPartOne: %d"

items
|> Map.toSeq
|> Seq.map fst
|> Seq.fold countIndependentGroups (0, Set.empty)
|> fst
|> printfn "AnswerPartTwo: %d" 