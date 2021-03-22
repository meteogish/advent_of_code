open System.Text.RegularExpressions
type Operation =
    | SetValue of char * int
    | SetRegValue of char * char
    | SubValue of char * int
    | SubRegValue of char * char
    | MulValue of char * int
    | MulRegValue of char * char
    | JnzVal of int * int
    | JnzReg of char * int

let rx = Regex ("(set|mul|jnz|sub) ([a-z0-9]) ([a-z]|(-?\d*))", RegexOptions.Compiled); 
    
let toOperation str =
    
    let m = rx.Match (str)
    
    let opStr = m.Groups.[1].Value
    let firstArgStr = m.Groups.[2].Value
    let secondArgStr = m.Groups.[3].Value
    
    let firstArg = System.Int32.TryParse (m.Groups.[2].Value)
    let secondArg = System.Int32.TryParse (m.Groups.[3].Value)
    
    match (opStr, firstArg, secondArg) with
    | ("set", (false, _), (true, secArgNumber)) -> SetValue (firstArgStr.[0], secArgNumber)
    | ("set", (false, _), (false, _)) -> SetRegValue (firstArgStr.[0], secondArgStr.[0])
    | ("sub", (false, _), (true, subArgNumber)) -> SubValue (firstArgStr.[0], subArgNumber)
    | ("sub", (false, _), (false, _)) -> SubRegValue (firstArgStr.[0], secondArgStr.[0])
    | ("mul", (false, _), (true, mulArgNumber)) -> MulValue (firstArgStr.[0], mulArgNumber)
    | ("mul", (false, _), (false, _)) -> MulRegValue (firstArgStr.[0], secondArgStr.[0])
    | ("jnz", (true, jnzNumberValue), (true, jnzArgNumber)) -> JnzVal(jnzNumberValue, jnzArgNumber)
    | ("jnz", (false, _), (true, jnzSecondArgNumber)) -> JnzReg (firstArgStr.[0], jnzSecondArgNumber)
    
let day23 = 
    System.IO.File.ReadAllLines("inputs/input_day23.txt") 
    |> Array.map toOperation

//day23 |> printfn "%A"

let regsValues = ['a'..'h'] |> List.map (fun reg -> (reg, 0)) |> Map.ofList

regsValues |> printfn "%A"
//(currentOpIndex, regs, countOfMuls)
let firstState = (regsValues, 0)

let rec proceed i state =
    if i < 0 || i >= day23.Length then
        printfn "Last i value is: %d" i
        state
    else
        let (regs, mulsCount) = state
        
        let currenOp = day23.[i]
        
        printfn "CurrentOp: %A, regs: %A, mulsCount: %d" currenOp regs mulsCount

        let updateRegs reg f =
            regs |> Map.change reg (fun pOption -> match pOption with | Some p -> f p |> Some | None -> pOption)

        let (newOpIndex, newRegs, newMulsCount) = 
            match currenOp with
            | SetValue (setRegTo, setValueNumber) -> (i + 1, updateRegs setRegTo (fun _ -> setValueNumber), mulsCount)
            | SetRegValue (setRegTo2, setValueOfReg) ->
                let valueOfReg = regs.[setValueOfReg] 
                (i + 1, updateRegs setRegTo2 (fun _ -> valueOfReg), mulsCount)
            | SubValue (subOfReg, subValueNumber) -> (i + 1, updateRegs subOfReg (fun pValue -> pValue - subValueNumber), mulsCount)
            | SubRegValue (subOfReg2, subValueOfReg) -> 
                let valueOfSubOfReg = regs.[subValueOfReg]
                (i + 1, updateRegs subOfReg2 (fun pValue -> pValue - valueOfSubOfReg), mulsCount)
            | MulValue (mulOfReg, mulValueNumber) -> (i + 1, updateRegs mulOfReg (fun pValue -> pValue * mulValueNumber), mulsCount + 1)
            | MulRegValue (mulOfReg2, mulValueOfReg) -> 
                let valueOfMulOfReg = regs.[mulValueOfReg]
                (i + 1, updateRegs mulOfReg2 (fun pValue -> pValue * valueOfMulOfReg), mulsCount + 1)
            | JnzVal (jnzValueCompare, jnzOffsetValue) ->
                let newOpIndex = if jnzValueCompare <> 0 then i + jnzOffsetValue else (i + 1)
                (newOpIndex, regs, mulsCount)
            | JnzReg (jnzRegCompare, jnzOffSet) ->
                let valueOfReg = regs.[jnzRegCompare]
                let newOpI = if valueOfReg <> 0 then i + jnzOffSet else (i + 1)
                (newOpI, regs, mulsCount)
            
        proceed newOpIndex (newRegs, newMulsCount) 

//let (latestRegs, mulsCount) = proceed 0 (regsValues, 0)
//printfn "mulsCount: %d" mulsCount


let b = 108400
let c = 125400

let rec findMul num x =
    if x >= num then
        None
    else if num % x = 0 then
        Some x
    else
        (x + 1) |> findMul num

let h = 
    seq { b .. 17 .. (c + 1) }
    |> Seq.sumBy (fun num -> match findMul num 2 with | Some _ -> 1 | None -> 0)
printfn "h: %d" h

