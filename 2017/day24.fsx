
let toNode (str : string) =
    let parts = str.Split('/')
    
    let first = parts.[0] |> int
    let second = parts.[1] |> int
    
    (first, second)
    
let day24 = System.IO.File.ReadAllLines("inputs/input_day24.txt") |> Array.map toNode |> List.ofArray

let strength chain =
    chain |> List.sumBy (fun node -> (node |> fst) + (node |> snd))

let rec proceed chainInProcess lookFor restNodes =
    let possibleNodesFolder acc node = 
        if (node |> fst = lookFor) then 
            ((node, snd) :: acc) 
        else if (node |> snd = lookFor) then 
            ((node, fst) :: acc) 
        else acc

    let findNextNode (node, fLookFor) =
        let chain = (node :: chainInProcess)
        let nextLookFor = (node |> fLookFor)
        let nodesWithoutThatOne = restNodes |> List.filter (fun n -> n <> node)
        proceed chain nextLookFor nodesWithoutThatOne

    let possibleNodes = restNodes |> List.fold possibleNodesFolder []
    
    if possibleNodes.IsEmpty then 
        let bridge = chainInProcess |> List.rev
        [(bridge |> strength, bridge)]
    else
        possibleNodes 
        |> List.collect findNextNode 
        
let bridges = proceed [] 0 day24
let (resultPartOne, _) = bridges |> List.maxBy fst
let (resultPartTwo, _) = bridges |> List.maxBy (fun (strength, bridge) -> bridge.Length * 1000 + strength)

resultPartOne |> printfn "PartOne: %d"
resultPartTwo |> printfn "PartTwo: %d"