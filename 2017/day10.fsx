//https://adventofcode.com/2017/day/9

open System.Collections.Generic

let day10PartOne = System.IO.File.ReadAllText("inputs/input_day10.txt").Split(',') |> Array.map int
let day10PartTwo = [| 17; 31; 73; 47; 23 |] |> Array.append (System.IO.File.ReadAllBytes("inputs/input_day10.txt") |> Array.map int)

type State = {
    Elements: int array;
    CurrentPosition: int;
    SkipSize: int;
}

let initialState () = {
    Elements = { 0 .. 255 } |> Array.ofSeq;
    CurrentPosition = 0;
    SkipSize = 0; 
}

let knotBy state length =
    let len = state.Elements.Length

    let mutable left = state.CurrentPosition
    let mutable ml = length

    //printfn "left = %d; state = %A; length = %d;" left state length
    while ml > 1 do 
        let right = (left + ml - 1) % len  
        //printfn "left: %d; right: %d" left right
        let t = state.Elements.[left]
        state.Elements.[left] <- state.Elements.[right]
        state.Elements.[right] <- t
        left <- (left + 1) % len
        ml <- ml - 2 

    {
        Elements = state.Elements;
        CurrentPosition = (state.CurrentPosition + length + state.SkipSize) % len;
        SkipSize = state.SkipSize + 1
    } 

let elements state = state.Elements

let knot input repeat state =
    if repeat = 1 then Seq.singleton 1 else { 1 .. repeat } 
    |> Seq.fold 
        (fun acc _ -> 
            input |> Seq.fold knotBy acc) state

let answerPartOne = 
    initialState () 
    |> knot day10PartOne 1 
    |> elements |> Seq.take 2 
    |> Seq.reduce (*)

answerPartOne |> printfn "AnswerPartOne: %d"

let sparseHash = initialState () |> knot day10PartTwo 64 |> elements

let denseHash = 
    sparseHash 
    |> Array.chunkBySize 16 
    |> Array.map (fun block -> block |> Seq.reduce (^^^) |> sprintf "%02x")
    |> String.concat ""

denseHash |> printfn "AnswerPartTwo: %s"

