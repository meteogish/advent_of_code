open System.IO
open System.Collections.Generic

let input = File.ReadAllText("input6")

printfn "Length: %i" input.Length

let addOrUpdate (set: Dictionary<char,uint>) currChar = 
    if set.ContainsKey (currChar) then
        set[currChar] <- set[currChar] + 1u
    else 
        set[currChar] <- 1u

let rec iter window (input: string) (set: Dictionary<char, uint>) i =
    if set.Count = window then
        i
    else 
        let prevChar = input.Chars (i-window)
        let prevCount = set[prevChar]
        if prevCount = 1u then
            set.Remove(prevChar) |> ignore
        else 
            set[prevChar] <- (prevCount - 1u)

        let currChar = input.Chars i
        addOrUpdate set currChar

        iter window input set (i + 1)

let set = Dictionary<char, uint>()

let partOneDistinctWindow = 4
let partTwoDistinctWindow = 14

let distinctWindow = partTwoDistinctWindow

{ 0..distinctWindow - 1 } |> Seq.iter (fun i -> addOrUpdate set (input.Chars i))

printfn "%A" (set |> Array.ofSeq)

iter distinctWindow input set distinctWindow
|> fun r -> printfn "%i" r


