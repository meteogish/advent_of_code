open System.Collections
open System.Collections.Generic
open System.IO

let windowSize = 25
type Window = Queue<bigint>

let defaultFoundNumber = -1 |> bigint

let rec findSumUsingTwoPointers (queue: Window) target =
    let sorted = queue |> Seq.sort |> Array.ofSeq
    let rec loop left right =
        if (left = right) then 
            None
        else
            let sum = sorted.[left] + sorted.[right]
            if sum = target then Some (left, right)
            else if sum < target then
                loop (left+1) right
            else 
                loop left (right - 1)
    loop 0 (queue.Count - 1)

let loopThroughLines (reader: StreamReader) = 
    let rec _loop (queue: Window, foundNumber: bigint, allNumbers: Window) =
        if (reader.EndOfStream) then (foundNumber, allNumbers)
        else 
            let nextNumber = reader.ReadLine() |> bigint.Parse
            allNumbers.Enqueue(nextNumber)
            if foundNumber <> defaultFoundNumber then 
                //number found but continue to populate all numbers for part 2
                _loop (queue, foundNumber, allNumbers)
            else 
                if (queue.Count < windowSize) then
                    queue.Enqueue(nextNumber)
                    // the window is not populated yet, just continue to populate window
                    _loop (queue, foundNumber, allNumbers)
                else 
                    match (findSumUsingTwoPointers queue nextNumber) with
                    | Some _ -> 
                        queue.Dequeue() |> ignore
                        queue.Enqueue(nextNumber)
                        _loop (queue, foundNumber, allNumbers)
                    // the first "wrong" number is found, continue to populate all numbers for part 2
                    | None -> _loop (queue, nextNumber, allNumbers)
            
        
    _loop (new Queue<bigint>(windowSize), defaultFoundNumber, new Queue<bigint>(1000))

let reader = new StreamReader(File.OpenRead("input_day9.txt"))

let (resultPart1, allNumbers)= loopThroughLines reader

let folder (sumSoFar, numbers: Window) nextNumber =
    let rec loopedDequeue (sumSoFar, numbers: Window) =
        let prevNumber = numbers.Dequeue()
        let nextSumSoFar = sumSoFar - prevNumber 
        if nextSumSoFar <= resultPart1 then
            (nextSumSoFar, numbers)
        else loopedDequeue (nextSumSoFar, numbers)
    
    if sumSoFar = resultPart1 then
        (sumSoFar, numbers)
    else 
        let nextSumSoFar = sumSoFar + nextNumber
        
        if nextSumSoFar > resultPart1 then
            numbers.Enqueue(nextNumber)
            loopedDequeue (nextSumSoFar, numbers)
        else 
            numbers.Enqueue (nextNumber)
            (nextSumSoFar, numbers)


let (_, consecutiveNumbers) = allNumbers |> Seq.fold (folder) (0I, new Window())

let (min, max) = 
    consecutiveNumbers 
    |> Seq.fold (fun (minSoFar, maxSoFar) nextNumber -> 
        if minSoFar = 0I || nextNumber < minSoFar then 
            (nextNumber, maxSoFar)
        else if maxSoFar = 0I || nextNumber > maxSoFar then
            (minSoFar, nextNumber)
        else (minSoFar, maxSoFar)
        ) (0I, 0I)

let resultPart2 = min + max

printfn "Result part 1: %A" resultPart1 
printfn "Result part 2: %A" resultPart2 
