//https://adventofcode.com/2017/day/17
open System.Collections.Generic

// Part One
let steps = 301
let count = 2017

let initialList () = 
    let list = List<int> (count)
    list.Add(0)
    list

let rec repeat1 i currentPos (list : List<int>) =
    if i > count then 
        (currentPos, list)
    else 
        let newCurrentPos = (currentPos + steps) % list.Count + 1
        list.Insert (newCurrentPos, i)
        repeat1 (i + 1) newCurrentPos list

let (lastPos, list) = repeat1 1 0 (initialList())

list |> Seq.item (lastPos + 1) |> printfn "AnswerPartOne: %d"


// Part Two
let fiftyMillion = 50_000_000

let mutable itemAfterZero = 0

let rec repeat2 i currentPos =
    if i > fiftyMillion then 
        currentPos
    else
        let newCurrentPos = (currentPos + steps) % (i) + 1
        itemAfterZero <- if newCurrentPos = 1 then i else itemAfterZero
        repeat2 (i + 1) newCurrentPos

repeat2 1 0 |> printfn "AnswerPartTwo: %d, CurrentPos: %d" itemAfterZero