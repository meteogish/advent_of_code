
let inputA = 703
let inputB = 516

let rec enumerator pass factor init =
    let f factor (i: uint) =
        let i = (uint64) i
        (i * factor ) % 2147483647UL |> uint32

    seq {
        let cur = f factor init
        if pass cur then yield cur
        yield! enumerator pass factor cur
    }

let getRightmost16Bits x = (x) &&& (uint32 0x0000FFFF)

let genAPartOne =  enumerator (fun _ -> true) 16807UL (uint32 inputA)
let genBPartOne =  enumerator (fun _ -> true) 48271UL (uint32 inputB)

let genAPartTwo =  enumerator (fun i  -> i % 4u = 0u) 16807UL (uint32 inputA)
let genBPartTwo =  enumerator (fun i ->  i % 8u = 0u) 48271UL (uint32 inputB)

let compare left right =
    let leftBits = left |> getRightmost16Bits 
    let rightBits = right |> getRightmost16Bits
    
    leftBits = rightBits

let judge count genA genB = 
    Seq.zip genA genB
    |> Seq.take count 
    |> Seq.fold (fun count (left, right) -> if compare left right then count + 1 else count) 0 


judge 40_000_000 genAPartOne genBPartOne |> printfn "AnswerPartOne: %d"
judge 5_000_000 genAPartTwo genBPartTwo |> printfn "AnswerPartTwo: %d"
