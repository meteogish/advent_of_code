open System

type Grid = string*int

type Rule = Grid * Grid

let startingGrid = (".#...####", 3) |> Grid

let splitIntoSubGrids convert (grid:string, size) =
    printfn "split into subgrids started: %s;%d" grid size
    let _splitIntoSubGrids subSize =
        let subSizeAfterConvert = if subSize = 2 then 3 else 4
        let subSizeSquared = subSize * subSize
        let countOfSubGrids = grid.Length / subSizeSquared
        let countOfSubGridsInOneRow = size / subSize

        let toSubGrid subGridI =
            let startingIndex = subGridI * subSizeSquared - (subSize * (subGridI % subSize))
            printfn "subGridI %i, startingIndex: %i" subGridI startingIndex
            
            { 0..subSize - 1}
            |> Seq.map (fun i -> 
                let curreStInd = startingIndex + i * 2 * subSize
                printfn "currentStInd: %i" curreStInd
                grid.Substring (curreStInd, subSize))
            |> String.concat ""
            |> convert
            |> fun gr -> (gr, subSize)
        
        let gmergeSubGrids (subGrids: Grid seq) =
            let rec _toSingeRow subGridsInOneRow i result =
                printfn "merging, subGroup: %A, i: %i, result:\n%s\n" subGridsInOneRow i result
                if i = subSizeAfterConvert then
                    result
                else
                    let next = 
                        subGridsInOneRow
                        |> Seq.map (fun (subGrid : string, _) -> subGrid.Substring (i * subSizeAfterConvert, subSizeAfterConvert))
                        |> String.concat ""
                    
                    _toSingeRow subGridsInOneRow (i + 1) (result + next)

            printfn "count of subGrids in one row: %i" countOfSubGridsInOneRow
            subGrids 
            |> Seq.windowed countOfSubGridsInOneRow 
            |> Seq.map (fun subGroup -> _toSingeRow subGroup 0 "")
            |> String.concat ""
            
        if countOfSubGrids = 1 then 
            let res = grid |> convert
            let s = if res |> String.length = 9 then 3 else 4
            (res, s)
        else { 0..countOfSubGrids - 1 } |> Seq.map toSubGrid |> gmergeSubGrids |> fun m -> (m, countOfSubGridsInOneRow * subSizeAfterConvert)

    if size % 2 = 0 then 
        _splitIntoSubGrids 2
    else _splitIntoSubGrids 3

let flipH (grid:string) =
    if grid.Length = 4 then
        grid.Substring(2, 2) + grid.Substring(0,2) 
    else 
        grid.Substring(6, 3) + grid.Substring(3,3) + grid.Substring(0,3) 

let flipV (grid:string) =
    if grid.Length = 4 then
        [1;0;3;2]
        |> Seq.map (fun i -> grid.[i])
        |> Array.ofSeq
        |> (fun p -> new String(p))
    else 
        [2;1;0;5;4;3;8;7;6]
        |> Seq.map (fun i -> grid.[i])
        |> Array.ofSeq
        |> (fun p -> new String(p))
        
let rotateLeft (grid:string) =
    if grid.Length = 4 then
        [1;3;0;2]
        |> Seq.map (fun i -> grid.[i])
        |> Array.ofSeq
        |> (fun p -> new String(p))
    else 
        [2;5;8;1;4;7;0;3;6]
        |> Seq.map (fun i -> grid.[i])
        |> Array.ofSeq
        |> (fun p -> new String(p))

let rotateRight =
    rotateLeft >> (fun s -> String(s.ToCharArray() |> Array.rev))

let convertLine (line:string) =
    let parts = line.Split(" => ")
    
    let one = parts.[0].Replace("/", "")
    let two = parts.[1].Replace("/", "")

    let result = one, two
    //printfn "%A" result
    result

let rulesMap = 
    let populateRules line =
        let rec _pppulareRules i max from = seq {
            if i = max then 
                yield! Seq.empty
            else 
                let r = rotateRight from
                yield r
                yield flipH r
                yield flipV r
                yield! _pppulareRules (i + 1) max r
        }
                
        seq {
            let (from, _to) = convertLine line
            yield! seq {
                yield from
                yield! _pppulareRules 0 4 from
            }
            |> Seq.distinct
            |> Seq.map (fun r -> (r, _to))
        }

    System.IO.File.ReadAllLines("input_day21.txt")
    //System.IO.File.ReadAllLines("test21.txt")
    |> Seq.collect populateRules
    |> Map.ofSeq

let printConvesions grid =
    printfn "%s" "FlipH"
    grid |> flipH |> printfn "%A"

    printfn "%s" "FlipV"
    grid |> flipV |> printfn "%A"

    printfn "%s" "Rotate left"
    grid |> rotateLeft |> printfn "%A"

    printfn "%s" "Rotate right"
    grid |> rotateRight |> printfn "%A"

let convert key = 
    let result = Map.find key rulesMap
    printfn "Converting : %s ===> %s" key result
    result

let rec iterate i max grid =
    if i = max then grid
    else 
        let next = splitIntoSubGrids convert grid 
        printfn "result on the %i iteration is: %s" i (next |> fst)
        iterate (i+1) max next


//rulesMap |> Map.count |> printfn "%i"
//rulesMap |> Map.toArray |> printfn "%A"

iterate 0 5 startingGrid  |> printfn "end result %A"
//splitIntoSubGrids convert startingGrid |> printfn "%A"

//"../.. => .##/..#/##." |> convertLine |> printfn "%A"


(* (".#...####", 3) |> printSubGrids *)

//printConvesions (startingGrid|>fst)
