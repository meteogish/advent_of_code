open System
open System.Text.RegularExpressions
open System.Collections.Generic
#nowarn "57"

let inputLines =
    System.IO.File.ReadAllLines("input_day8.txt")

printfn "%d" (inputLines |> Array.length)