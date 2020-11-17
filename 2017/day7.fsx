open System
open System.Text.RegularExpressions
open System.Collections.Generic
#nowarn "57"

let inputLines =
    System.IO.File.ReadAllLines("input_day7.txt")

printfn "%d" (inputLines |> Array.length)

type Program = { Name: string; Weight: int; Children: string list; Refs: int }

type ProgramTree = Node of Program * int * ProgramTree list 

let getWeight (Node (_, weight, _)) = weight

let parseLineToProgram =
    let rx = Regex ("(\w+) \((\d+)\)(?: -> ((?:\w+)(?:,\s*\w+)*))?", RegexOptions.Compiled); 
    let parse line =
        let createProgram name weightStr children = { Name = name; Weight = weightStr |> int; Children = children; Refs = 0 }
        
        let m = rx.Match(line);
        let matchChildren = 
            let str = if m.Groups.Count = 4 then (m.Groups.[3]).Value else String.Empty

            if String.IsNullOrEmpty(str) then List.Empty else str.Split(", ") |> List.ofArray

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
        let childNodes = p.Children |> List.map (fun c -> map.[c] |> toNode)
        let childsSum = childNodes |> Seq.sumBy getWeight 
        (p, childsSum + p.Weight, childNodes) |> Node

    map
    |> findRoot
    |> toNode 

let findUnbalansedNode tree =
    let rec fNode (Node (_, _, subTree)) =
        match Seq.length (subTree |> Seq.distinctBy getWeight) with
        | 2 -> (subTree |> List.map (fun (Node (p, w, _)) -> (p.Name, p.Weight, w))) :: List.collect fNode subTree
        | _ -> List.collect fNode (subTree |> List.ofSeq)

    let groups = 
        tree 
        |> fNode 
        |> List.tail 
        |> List.collect id 
        |> List.groupBy (fun (_, _, subTreeWeight) -> subTreeWeight)

    let [(firstW, firstItems); (secondW, secondItems)] = groups

    let diff = abs(firstW - secondW)

    let (name, w, subTreeWeight) = 
        if firstItems |> List.length = 1 then firstItems |> List.head 
        else secondItems |> List.head

    (name, w - diff, subTreeWeight)
    
inputLines |> Array.take 4 |> Array.map parseLineToProgram |> printfn "%A"
let map = inputLines |> convertToMap 

let answerPartOne = findRoot map 
printfn "AnswerPartOne: %s" answerPartOne.Name

let (_, answerPartTwo, _) = map |> convertToTree |> findUnbalansedNode
printfn "AnswerPartTwo: %d" answerPartTwo
