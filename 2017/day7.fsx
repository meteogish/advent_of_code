open System
open System.Text.RegularExpressions
open System.Collections.Generic
#nowarn "57"

let inputLines =
    System.IO.File.ReadAllLines("input_day7.txt")

printfn "%d" (inputLines |> Array.length)

type Program = { Name: string; Weight: int; Children: string list; Refs: int }

type ProgramTree = Node of Program * int * ProgramTree seq

let getWeight (Node (_, weight, _)) = weight

let parseLineToProgram =
    let rx = Regex ("(\w+) \((\d+)\)(?: -> ((?:\w+)(?:,\s*\w+)*))?", RegexOptions.Compiled); 
    let parse line =
        let createProgram name weightStr children = { Name = name; Weight = weightStr |> int; Children = children; Refs = 0 }
        
        let m = rx.Match(line);
        let matchChildren = 
            let str = if m.Groups.Count = 4 then Some (m.Groups.[3]).Value else None

            match str with
            | Some s -> s.Split(", ") |> List.ofArray 
            | _ -> List.empty 

        //m.Groups.Count |> printfn "Count of groups:%d"
        let name = (m.Groups.[1]).Value
        let weightStr = (m.Groups.[2]).Value
        let children = matchChildren 
        //printfn "name:%s weight:%s children:%A" name weightStr children

        createProgram name weightStr children
    parse

let convertToMap inputLines = 
    let map = inputLines |> Seq.map (parseLineToProgram >> (fun p -> (p.Name, p))) |> Map.ofSeq
    
    let folder acc (kvp: KeyValuePair<string, Program>) =
        let updateChild (acc2 : Map<string, Program>) id =
            acc2.Change (id, (fun o -> match o with | Some old -> Some { old with Refs = old.Refs + 1 } | None -> None))

        kvp.Value.Children |> Seq.fold updateChild acc 

    map |> Seq.fold folder map

let findRoot (map: Map<string, Program>) = 
    (map |> Seq.find (fun kvp -> (kvp.Value.Refs = 0))).Value

let convertToTree (map: Map<string, Program>) =
    let rec toNode p = 
        let childNodes = p.Children |> Seq.map (fun c -> map.[c] |> toNode)
        let childsSum = childNodes |> Seq.sumBy getWeight 
        (p, childsSum + p.Weight, childNodes) |> Node

    map
    |> findRoot
    |> toNode 

let findUnbalansedNode tree =
    let rec fNode (Node (p, w, subTree)) =
        match Seq.length (subTree |> Seq.distinctBy getWeight) with
        | 2 -> ((subTree |> Seq.map (fun (Node (p, w, _)) -> (p, w))) :: (Seq.collect fNode subTree |> List.ofSeq)) |> List.ofSeq
        | _ -> Seq.collect fNode subTree |> List.ofSeq

    tree |> fNode
    
inputLines |> Array.take 4 |> Array.map parseLineToProgram |> printfn "%A"
let map = inputLines |> convertToMap 

let answerPartOne = findRoot map 
printfn "%A" answerPartOne

map |> convertToTree |> findUnbalansedNode |> printfn "%A" 

