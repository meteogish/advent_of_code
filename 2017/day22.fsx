type Direction = 
    | Up
    | Down
    | Left
    | Right
    
let getPositionForDirection dir (posY, posX) =
    match dir with
    | Up -> (posY - 1, posX)
    | Down -> (posY + 1, posX)
    | Left -> (posY, posX - 1)
    | Right -> (posY, posX + 1)

let getNewDir dir isLeft =
    match dir with
    | Up when isLeft -> Left 
    | Up when not isLeft -> Right
    | Down when isLeft -> Right 
    | Down when not isLeft -> Left 
    | Right when isLeft -> Up 
    | Right when not isLeft -> Down 
    | Left when isLeft -> Down 
    | Left when not isLeft -> Up 

let reverseDir = function 
    | Up -> Down
    | Down -> Up
    | Left -> Right
    | Right -> Left

let day22 = System.IO.File.ReadAllLines("inputs/input_day22.txt") |> Array.map (fun str -> str.ToCharArray())

let xSize = day22.[0] |> Array.length
let ySize = day22 |> Array.length

let startPositon = (ySize / 2, xSize / 2)
printfn "day22 length: %d" day22.Length
let map = day22 
        |> Array.mapi (fun i row -> row |> Array.mapi (fun j cell -> (i - fst startPositon, j - snd startPositon), cell)) 
        |> Array.collect id 
        |> Map.ofArray

printfn "map length: %d" map.Count
map |> Map.iter (fun k v -> printfn "pos: %A, v: %c" k v)

let infectedCellValue = '#'

let firstState = (0, map, (0, 0), Up)

let isInfected map pos =
    match map |> Map.tryFind pos with
    | Some v -> v = infectedCellValue 
    | None -> false
     
let count = 10000 
(*let rec proceed i state =
    if i >= count then
        state
    else
        let (numberOfInfected, map, pos, dir) = state
        let isInfected = isInfected map pos 
        
        //printfn "burstNo: %d, beforeDir: %A, beforePos: %A, beforenumberOfInfected: %d" i dir pos numberOfInfected 
        let newNumberOfInfected = if isInfected then numberOfInfected else numberOfInfected + 1
        
        let newValue = if isInfected then '.' else infectedCellValue
        let newMap = map |> Map.change pos (fun _ -> Some newValue)

        let newDir = getNewDir dir (not isInfected)
        let newPos = pos |> getPositionForDirection newDir 
        
        //printfn "isInfected: %b, newDir: %A, newPos: %A, numberOfInfected: %d" isInfected newDir newPos newNumberOfInfected

        let newState = (newNumberOfInfected, newMap, newPos, newDir)

        proceed (i + 1) newState 
*)
//let (numberOfInfected, _, pos, dir) = proceed 0 firstState
//printfn "start: %A, numberOFInfected = %d" startPositon (numberOfInfected) 
//5538

type NoneState =
    | Clean
    | Weakened
    | Infected
    | Flagged

let getNodeState map pos =
    match map |> Map.tryFind pos with
    | Some v -> 
        match v with
        | '.' -> Clean
        | 'F' -> Flagged
        | 'W' -> Weakened
        | _ -> Infected
    | None -> Clean
    
let getNodeNextState = function
    | Clean -> Weakened
    | Weakened -> Infected
    | Infected -> Flagged
    | Flagged -> Clean

let stateToNodeValue = function
    | Clean -> '.'
    | Weakened -> 'W'
    | Infected -> infectedCellValue
    | Flagged -> 'F'

let nodeValueToState = function
    | Some '.' -> Clean
    | Some 'W' -> Weakened
    | Some 'F' -> Flagged
    | Some _ -> Infected
    | None -> Clean
 
//let count2 = 100 
let count2 = 10000000  
let rec proceed2 i state =
    if i >= count2 then
        state
    else
        let (numberOfInfected, map, pos, dir) = state
        let currentState = getNodeState map pos 
        
        let (newNumberOfInfected, newDir)=
            match currentState with
            | Clean -> (numberOfInfected, getNewDir dir true)
            | Weakened -> (numberOfInfected + 1, dir)
            | Infected -> (numberOfInfected, getNewDir dir false)
            | Flagged -> (numberOfInfected, reverseDir dir)

        //printfn "burstNo: %d, beforeDir: %A, beforePos: %A, beforenumberOfInfected: %d" i dir pos numberOfInfected 
        
        let newMap = map |> Map.change pos (fun p -> p |> nodeValueToState |> getNodeNextState |> stateToNodeValue |> Some)

        let newPos = pos |> getPositionForDirection newDir 
        
        //printfn "isInfected: %b, newDir: %A, newPos: %A, numberOfInfected: %d" isInfected newDir newPos newNumberOfInfected

        let newState = (newNumberOfInfected, newMap, newPos, newDir)

        proceed2 (i + 1) newState 

let (numberOfInfected2, _, _, _) = proceed2 0 firstState

printfn "numberOFInfected2 = %d" (numberOfInfected2) 