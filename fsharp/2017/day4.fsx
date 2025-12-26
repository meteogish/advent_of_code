//https://adventofcode.com/2017/day/4
open System

let lines = System.IO.File.ReadAllLines("input_day4.txt")
printfn "Cound of lines %d" lines.Length

let boolToInt b = if b then 1 else 0

let isValidPasswordPartOne (password: string) =
    let parts = password.Split(" ")
    (parts |> Set.ofArray).Count = parts.Length 
    |> boolToInt 

let answerPartOne = lines |> Seq.sumBy isValidPasswordPartOne

let isValidPasswordPartTwo (password: string) =
    let f = Seq.sort >> Seq.toArray >> String

    password.Split(" ")
    |> Seq.countBy (Seq.sort >> Seq.toArray >> String)
    |> Seq.exists (fun pair -> (pair |> snd) > 1) 
    |> not
    |> boolToInt 

let answerPartTwo = lines |> Seq.sumBy isValidPasswordPartTwo 

printfn "AnswerPartOne: %d" answerPartOne //325 
printfn "AnswerPartTwo: %d" answerPartTwo //119

