open System
open System.Collections.Generic

let lines = System.IO.File.ReadAllLines("input_day5.txt")
let numbers = lines |> Seq.map int


let nums = numbers |> Array.ofSeq


let rec proc currentPos =
    //printfn "CurentPos: %d" currentPos
    if currentPos < 0 || currentPos >= nums.Length then 
        0
    else
        let offset = nums.[currentPos]
        //printfn "Moves to: %d" offset 
        nums.[currentPos] <- offset + 1
        1 + (proc <| currentPos + offset)

//nums.[0] <- 2
printfn "%d" (proc 0) //378980
//printfn "%A" nums

