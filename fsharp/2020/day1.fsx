let target = 2020

let list = System.IO.File.ReadAllLines("input_day1.txt") |> Array.map int |> Array.sort

let rec loop start left right =
    if (left = right) then 
        0
    else
        let sum = start + list.[left] + list.[right]
        if sum = target then 
            list.[left] * list.[right]
        else if sum < target then
            loop start (left+1) right
        else 
            loop start left (right - 1)

let answerPartOne = loop 0 0 (list.Length - 1)

printfn "Answer part one: %d" answerPartOne

let rec loop2 i =
    if i = list.Length then
        0
    else
        let start = list.[i]
        let res = loop start 0 (list.Length-1)
        if res = 0 then
            loop2 (i+1)
        else 
            start*res

let answerPartTwo = loop2 0
printfn "Answer part two: %d" answerPartTwo
            
            