open System.IO

let lines = File.ReadAllLines("input_day10.txt") |> Array.map int |> Array.sort

let reducer1 (prev, ones, threes) curr =
    if curr - prev = 1 then
        (curr, ones + 1, threes)
    else if curr - prev = 3 then
        (curr, ones, threes + 1)
    else
        (curr, ones, threes)

let (_, ones, threes) = lines |> Seq.fold reducer1 (0, 0, 1)
printfn "Part1: %d" (ones * threes)


let reducer2 (prev, subSeqLength, result) curr =
    if curr - prev = 1 then
        (curr, subSeqLength + 1, result)
    else if curr - prev = 3 && subSeqLength > 1 then
        let onesLenght = subSeqLength - 1
        let combinationsMultiplier = if onesLenght = 3 then (1 <<< onesLenght) - 1 else (1 <<< onesLenght)
        (curr, 0, result * (combinationsMultiplier |> bigint))
    else 
        (curr, 0, result)

let linesWithMax = seq {
    yield! lines
    yield (lines |> Array.last) + 3
}

let (_, _, part2) = linesWithMax |> Seq.fold reducer2 (0, 0, 1I);

printfn "Part2: %A" part2
