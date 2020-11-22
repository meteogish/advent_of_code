//https://adventofcode.com/2017/day/9

open System.Collections.Generic

let day10 = System.IO.File.ReadAllText("inputs/input_day10.txt").Split(',') |> Array.map int

type State = {
    Elements: int array;
    CurrentPosition: int;
    SkipSize: int;
}

let initialState = {
    Elements = { 0 .. 255 } |> Array.ofSeq;
    CurrentPosition = 0;
    SkipSize = 0; 
}

let rotateBetween state length =
    let len = state.Elements.Length

    let mutable left = state.CurrentPosition
    let mutable right = (state.CurrentPosition + length) % len - 1

    printfn "left = %d; right=%d; state = %A; length = %d;" left right state length
    while left <> right do 
        printfn "left: %d; right: %d" left right
        let t = state.Elements.[left]
        state.Elements.[left] <- state.Elements.[right]
        state.Elements.[right] <- t
        left <- (left + 1) % len
        right <- (len + right - 1) % len

    {
        Elements = state.Elements;
        CurrentPosition = (state.CurrentPosition + length + state.SkipSize) % len;
        SkipSize = state.SkipSize + 1
    } 

let f input state =
    let folder = rotateBetween 
    
    input |> Seq.fold folder state 

let testState = {
    Elements = { 0 .. 4 } |> Array.ofSeq;
    SkipSize = 0;
    CurrentPosition = 0;
}
let lengths = [ 3; 4; 1; 5 ]
f lengths testState
|> printfn "%A"

//initialState |> f day10 |> printfn "%A"

//let testElements = { 0 .. 4 } |> Array.ofSeq
//printfn "%A" <| rotateBetween testElements 0 2
//printfn "%A" <| rotateBetween testElements 3 1
