let input = 
    (System.IO.File.ReadAllLines("input_day6.txt")
    |> Seq.head).Split('\t')
    |> Seq.map int
    |> Array.ofSeq

printfn "%A" input

let len = input.Length

let findMaxIndex banks =
    banks 
    |> Seq.mapi (fun i b -> (i, b))
    |> Seq.maxBy snd
    |> fst

let increaseBanksIn indexToIncrease banks =
    banks 
    |> Seq.mapi (fun i b -> if i = indexToIncrease then b + 1 else b)

let zeroBanksIn indexToZero banks =
    banks 
    |> Seq.mapi (fun i b -> if i = indexToZero then 0 else b)

let redistribute len currentBanks maxIndex =
   let banksAmount = currentBanks |> Seq.item maxIndex
   let rest = banksAmount % (len - 1) 
   let by = (banksAmount - rest) / (len - 1)
   printfn "Redistribute: max: %d; len: %d; rest: %d; by: %d" banksAmount len rest by 

   currentBanks
   |> Seq.mapi (fun i b -> if i = maxIndex then rest else b + by)  

let proc banks len =

    let rec procAcc len stack bs  =
        let redbtd = redistribute len bs ( bs |> findMaxIndex )
        if stack |> List.exists (fun b -> (redbtd |> Seq.compareWith Operators.compare b) = 0) then
            (redbtd :: stack)
        else
            procAcc len (redbtd :: stack) redbtd
    
    procAcc len List.empty banks

   

let testInput = seq { 0; 2; 7; 0 }
let firstCycleMaxIndex = testInput |> findMaxIndex
//let firstCycle = redistribute firstCycleMaxIndex testInput (testInput |> Seq.length)
//printfn "%A" (firstCycle |> Array.ofSeq)

//let processed = proc testInput (testInput |> Seq.length) 
//processed |> Seq.rev |> Seq.iter (fun line -> printfn "%A" (line |> Array.ofSeq))
//printfn "%A" processed.Length


let answerPartOne = proc input len
answerPartOne |> Seq.rev |> Seq.iter (fun line -> printfn "%A" (line |> Array.ofSeq))
printfn "AnswerPartOne: %d" answerPartOne.Length


