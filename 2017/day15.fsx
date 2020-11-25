let inputA = 703u
let inputB = 516u

let getRightmost16Bits x = (x) &&& (uint32 0x0000FFFF)

let rec enumerator pass factor init =
    let f factor (i: uint) =
        let i = (uint64) i
        (i * factor ) % 2147483647UL |> uint32

    seq {
        let cur = f factor init
        if pass cur then yield cur
        yield! enumerator pass factor cur
    }

let getGen factor input pass = enumerator pass factor input
let noCheck _ = true
let genACheck i = i % 4u = 0u
let genBCheck i = i % 8u = 0u

let genA =  getGen 16807UL inputA 
let genB =  getGen 48271UL inputB

let compare left right =
    let leftBits = left |> getRightmost16Bits 
    let rightBits = right |> getRightmost16Bits
    leftBits = rightBits

let judge count genA genB = 
    Seq.zip genA genB
    |> Seq.take count 
    |> Seq.fold (fun count (left, right) -> if compare left right then count + 1 else count) 0 

judge 40_000_000 (genA noCheck) (genB noCheck) |> printfn "AnswerPartOne: %d"
judge 5_000_000 (genA genACheck) (genB genBCheck) |> printfn "AnswerPartTwo: %d"
