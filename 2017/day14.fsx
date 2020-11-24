open System.Collections
open System.Linq
#load "day10.fsx"

let input = "oundnydw"

let countBitsOfHash hash =
    let ints = hash |> Seq.map (fun c -> System.Byte.Parse (c |> string, System.Globalization.NumberStyles.HexNumber)) |> Array.ofSeq
    let b = BitArray (ints)
    b.Cast<bool>() |> Seq.sumBy (fun b -> if b then 1 else 0)

let countBits input i =
    let inp = sprintf "%s-%d" input i
    let hash = sprintf "%s-%d" input i |> Day10.knotHash
    
    let countBitsOfHash = countBitsOfHash hash
    printfn "Inp: %s ; Hash: %s ; BitsCount: %d" inp hash countBitsOfHash
    countBitsOfHash

{ 0 .. 127 } |> Seq.sumBy (countBits input) |> printfn "%d"

"a0c2017" |> countBitsOfHash |> printfn "%d"
"ff" |> countBitsOfHash |> printfn "%d"
