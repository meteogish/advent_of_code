open System.Text.RegularExpressions
let target = 2020

let regex = new Regex("(\d*)-(\d*) ([a-z]): ([a-z]*)");

let toParts line =
    let m = regex.Match(line);
    let from = m.Groups.[1].Value |> int
    let _to = m.Groups.[2].Value |> int
    let ch = m.Groups.[3].Value.[0]
    let str = m.Groups.[4].Value
    (from, _to, ch, str)

let validatePartOne line =
    let (from, _to, ch, str) = line |> toParts 
    let countOfCh = str.Split(ch).Length - 1

    if countOfCh <= _to && countOfCh >= from then
        1
    else
        0

let validatePartTwo line =
    let (from, _to, ch, str) = line |> toParts 
    
    let fromEquals = str.[from - 1] = ch
    let toEquals = str.[_to - 1] = ch

    if fromEquals <> toEquals then 1 else 0
    
let validateTwoParts (one, two) line =
    (one + (validatePartOne line), two + (validatePartTwo line))
    
let lines = System.IO.File.ReadAllLines("input_day2.txt") 

let (partOne, partTwo) = lines |> Seq.fold validateTwoParts (0,0)
printfn "%A" partOne
printfn "%A" partTwo


