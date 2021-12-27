open System

type Grid = string*int

let startingGrid = (".#...####", 3) |> Grid

let ex = ("#..#........#..#", 4) |> Grid

//#..#
//....
//....
//#..#

(*

#.|.#
..|..
--+--
..|..
#.|.#

0: 0 1 4 5
1: 2 3 6 7
2: 8 9 12 13
3: 10 11 14 15


///////////////////////////
/// 
/// ///////////////////////
/// 
##.|##.
#..|#..
...|...
---+---
##.|##.
#..|#..
...|...

0: 0 1 2 6 7 8 12 13 14
1: 3 4 5 9 10 11 15 16 17
2: 18 19 20 24 25 26 30 31 32
3: 21 22 23 27 28 29 33 34 35


let (grid, size) = grid

if size % 2 == 0 then
    let countOfSubGrids = grid.length / 4; // 4
    
    let startingIndex = subGridI * subSize * subSize + subSize * (subGridI % subSize)

    { 0..subSize }
    |> Seq.fold 
*)

let splitIntoSubGrids convert (grid:string, size) =
    let _splitIntoSubGrids subSize =
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
                if i = subSize then
                    result
                else
                    let next = 
                        subGridsInOneRow
                        |> Seq.map (fun (subGrid : string, _) -> subGrid.Substring (i * subSize, subSize))
                        |> String.concat ""
                    
                    _toSingeRow subGridsInOneRow (i + 1) (result + next)

            printfn "count of subGrids in one row: %i" countOfSubGridsInOneRow
            subGrids 
            |> Seq.windowed countOfSubGridsInOneRow 
            |> Seq.map (fun subGroup -> _toSingeRow subGroup 0 "")
            |> String.concat ""
            
        if countOfSubGrids = 1 then (grid |> convert, size) 
        else { 0..countOfSubGrids - 1 } |> Seq.map toSubGrid |> gmergeSubGrids |> fun m -> (m, countOfSubGridsInOneRow * subSize)

    if size % 2 = 0 then 
        _splitIntoSubGrids 2
    else _splitIntoSubGrids 3

let convert from =
    ".##."

let printSubGrids grid =
    grid
    |> splitIntoSubGrids convert
    
    |> printfn "%A"

("#..#........#..#", 4) |> printSubGrids
(* (".#...####", 3) |> printSubGrids *)

