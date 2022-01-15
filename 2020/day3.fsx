open System.Text.RegularExpressions
let target = 2020

let lines = System.IO.File.ReadAllLines("input_day3.txt")

let start = 0,0

let step = 3

let width = lines.[0].Length

let rec loop (x, y) (stepx, stepy) treesCount =
    if y >= lines.Length then 
        treesCount
    else 
        let nextX = (x + stepx) % width
        let nextY = (y + stepy)
        let nextTreesCount = if lines.[y].[x] = '#' then treesCount + 1 else treesCount
        loop (nextX, nextY) (stepx, stepy) nextTreesCount
        
let partOne = loop start (3, 1) 0

printfn "Part one: %d" partOne

#time    
let partTwo = 
    [ (1,1); (3,1); (5,1); (7,1); (1, 2)] 
    |> Seq.map (fun step -> loop start step 0 |> bigint)  
    |> Seq.reduce (*)

printfn "Part two: %A" partTwo