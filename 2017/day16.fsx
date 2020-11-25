//https://adventofcode.com/2017/day/16

let day16 = System.IO.File.ReadAllText("inputs/input_day16.txt") 

type DanceMove = 
    | Spin of int
    | Exchange of int * int
    | Partner of char * char

let parseDanceMove (str: string) =
    if str.StartsWith ("s") then
        str.Substring(1) |> int |> Spin
    else if str.StartsWith ("x") then
        let [|l; r|] = str.Substring(1).Split('/') |> Array.map int
        Exchange (l, r)
        //Exchange (4,5)
    else if str.StartsWith ("p") then
        let [|l; r|] = str.Substring(1).Split('/') |> Array.map char
        Partner (l, r)
    else failwith "Cound not parse"

let toString chars = chars |> Seq.map string |> String.concat ""

let folder (acc: string) move =
    let spin i = 
        //printfn "Spin: %d; Before: %A; " i (acc)
        
        let skip = acc.Length - i
        Seq.append ( acc |> Seq.skip skip |> Seq.take i) (acc |> Seq.take skip) |> toString

    let exchange from toI (arr: char array) =
        //printfn "Exchange: (%d,%d); Before: %A; " from toI (acc)
        let t = arr.[toI]
        arr.[toI] <- arr.[from]
        arr.[from] <- t
        arr |> toString
    
    let partner fromC toC (arr: char array) = 
        //printfn "Partner %c -> %c , BEfore %A" fromC toC arr
        let fromI = arr |> Array.findIndex ((=) fromC)
        let toI = arr |> Array.findIndex ((=) toC)
        exchange fromI toI arr
    
    match move with 
    | Spin s -> spin s 
    | Exchange (from, toI) -> exchange from toI (acc.ToCharArray ())
    | Partner (fromC, toC) -> partner fromC toC (acc.ToCharArray ())

let replicateAll n s = s |> Seq.collect (Seq.replicate n)

let oneDance programs moves =
    moves 
    |> Seq.fold folder programs

let danceSequence programs moves =
    seq {
        yield programs
        yield! Seq.unfold (fun p -> 
            let next = oneDance p moves
            Some(next, next)) programs
    }

let programs = { 'a' .. 'p' } |> Array.ofSeq
let moves = day16.Split(",") |> Seq.map parseDanceMove 

let cycleCount = 
    danceSequence (programs |> toString) moves 
    |> Seq.skip 1
    |> Seq.takeWhile ((<>) (programs |> toString)) 
    |> Seq.length 
    |> (+) 1

cycleCount |> printfn "Cycle: %d"

let oneBillion =  1_000_000_000
let shouldTake = oneBillion % cycleCount
shouldTake |> printfn "ShouldTake: %d"

moves |> oneDance (programs |> toString) |> printfn "AnswerPartOne: %s"

danceSequence (programs |> toString) moves 
|> Seq.item shouldTake 
|> printfn "AsnwerPartTwo: %s"




