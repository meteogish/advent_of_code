open System.Collections.Generic

let steps = 301

let list = List<int> (2018)
list.Add(0)


let rec repeat1 i currentPos (list : List<int>) =
    if i > 2017 then 
        currentPos
    else 
        let newCurrentPos = (currentPos + steps) % list.Count + 1
        list.Insert (newCurrentPos, i)
        repeat1 (i + 1) newCurrentPos list

let lastPos = repeat1 1 0 list

list |> Seq.skip (lastPos - 1) |> Seq.take 3 |> printfn "%A"








