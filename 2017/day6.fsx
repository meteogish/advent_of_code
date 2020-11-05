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

let printBanks : (seq<int> -> unit)= 
    Array.ofSeq >> printfn "%A"

let findMax banks =
    banks
    |> Seq.mapi (fun i b -> (i, b))
    |> Seq.maxBy snd

let increaseBanksIn indexToIncrease ( banks : List<int> ) =
    banks.[indexToIncrease] <- banks.[indexToIncrease] + 1
    banks 

let redistribute len currentBanks =
    let (maxIndex, max) = currentBanks |> findMax 
    let mutableBanks = List (currentBanks)
    mutableBanks.[maxIndex] <- 0
    [ 1 .. max ]
    |> Seq.fold (fun acc i -> acc |> increaseBanksIn ((i + maxIndex) % len)) mutableBanks :> seq<int>

let proc banks len =
    let rec procAcc len stack bs =
        let nextBanks = redistribute len bs 
        
        match stack |> List.tryFindIndex (equals nextBanks) with 
        | Some i -> (stack, i)
        | None -> procAcc len (nextBanks :: stack) nextBanks

    procAcc len [banks] banks

let floydsCycleDetection banks len =
    let f = redistribute len
    let notEquals b1 b2 = b1 |> equals b2 |> not 
    let mutable t = banks |> f
    let mutable h = t |> f

    while h |> notEquals t do
        t <- f t
        h <- f (f h) 

    let mutable mu = 0
    h <- banks 

    while h |> notEquals t do
        t <- f t
        h <- f h
        mu <- mu + 1
    
    let mutable lambda = 1
    h <- f t 
    while h |> notEquals t do
        h <- f h 
        lambda <- lambda + 1
    
    (mu, lambda)

#time
let answerPair = proc input len
#time
let answerPartOne = answerPair |> fst |> List.length 
printfn "AnswerPartOne: %d" answerPartOne
let answerPartTwo = answerPair |> snd |> (+) 1
printfn "AnswerPartTwo: %d" answerPartTwo

#time
let floydsAnswer = floydsCycleDetection input len
#time

printfn "mu=%d; lambda=%d" (floydsAnswer |> fst) (floydsAnswer |> snd)

//answerPartFirst = mu + lambda
//answerToPartSecond = lambda




