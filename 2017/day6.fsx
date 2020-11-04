open System.Collections.Generic
let input =
    (System.IO.File.ReadAllLines("input_day6.txt")
     |> Seq.head).Split('\t')
    |> Seq.map int
    |> Array.ofSeq

printfn "%A" input

let len = input.Length

let equals b1 b2 =
    (b1 |> Seq.exists2 (<>) b2)
    |> not

let printBanks = 
    Array.ofSeq >> printfn "%A"

let findMax banks =
    banks
    |> Seq.mapi (fun i b -> (i, b))
    |> Seq.maxBy snd

let increaseBanksIn indexToIncrease ( banks : List<'a> ) =
    //banks
    //|> Seq.mapi (fun i b -> if i = indexToIncrease then b + 1 else b)
    banks.[indexToIncrease] <- banks.[indexToIncrease] + 1
    banks 

let zeroBanksIn indexToZero banks =
    banks
    |> Seq.mapi (fun i b -> if i = indexToZero then 0 else b)

let redistribute len currentBanks =
    let (maxIndex, max) = currentBanks |> findMax 
    //printfn "Redistribute: max: %d; atIndex: %d" max maxIndex 
    let mutableBanks = List (currentBanks)
    mutableBanks.[maxIndex] <- 0
    [ 1 .. max ]
    |> Seq.fold (fun acc i -> acc |> increaseBanksIn ((i + maxIndex) % len)) mutableBanks :> seq<int>

let proc banks len =
    let rec procAcc len stack bs =
        let redbtd = redistribute len bs 
        //printBanks redbtd

        if ((stack |> List.length) % 10) = 0 then
            printfn "Stack size: %d " stack.Length
        
        match stack |> List.tryFindIndex (equals redbtd) with 
        | Some i -> (stack, i)
        | None -> procAcc len (redbtd :: stack) redbtd

    procAcc len [banks] banks


let testInput = seq { 0; 2; 7; 0; }

let processed = proc testInput (testInput |> Seq.length)
processed |> fst |> Seq.rev |> Seq.iter printBanks   
printfn "%A" (processed |> fst |> List.length)

let answerPair = proc input len
let answerPartOne = answerPair |> fst |> List.length 
printfn "AnswerPartOne: %d" answerPartOne

let answerPartTwo = answerPair |> snd
printfn "AnswerPartTwo: %d" answerPartTwo




