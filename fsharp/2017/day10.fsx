//https://adventofcode.com/2017/day/10

let day10PartOne = System.IO.File.ReadAllText("inputs/input_day10.txt").Split(',') |> Array.map int
let day10Text = System.IO.File.ReadAllText("inputs/input_day10.txt")

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

    while ml > 1 do 
        let right = (left + ml - 1) % len  
        
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
    |> Seq.fold (fun acc _ -> input |> Seq.fold knotBy acc) state

let knotHash (input:string) =
    let t = input |> Seq.map int |> Array.ofSeq
    let ints = [| 17; 31; 73; 47; 23 |] |> Array.append t 
    let sparseHash = 
        initialState () 
        |> knot ints 64 
        |> elements

    let denseHash = 
        sparseHash 
        |> Array.chunkBySize 16 
        |> Array.map (Seq.reduce (^^^) >> sprintf "%02x")
        |> String.concat "" 

    denseHash

let answerPartOne = 
    initialState () 
    |> knot day10PartOne 1 
    |> elements |> Seq.take 2 
    |> Seq.reduce (*)

let answers () =
    answerPartOne |> printfn "AnswerPartOne: %d"
    knotHash day10Text |> printfn "AnswerPartTwo: %s"

