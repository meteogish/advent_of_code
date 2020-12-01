
type Register = Int of int | Char of char

type Operation =
    | Snd of Register
    | Set of Register * Register
    | Add of Register * Register
    | Mul of Register * Register
    | Mod of Register * Register
    | Rcv of Register
    | JumpGtZero of Register * Register

let toOperation (str: string) =
    //str |> printfn "%s" 
    let parts = str.Split(' ')

    let getRegister (str: string) = 
        match System.Int32.TryParse str with
        | true, int -> Int int
        | _ -> Char (str.Chars 0)

    let pair () =  (parts.[1] |> getRegister, parts.[2] |> getRegister)
    let single () = parts.[1] |> getRegister 

    match parts.[0] with
    | "snd" -> single () |> Snd 
    | "set" -> pair () |> Set  
    | "add" -> pair () |> Add 
    | "mul" -> pair () |> Mul 
    | "mod" -> pair () |> Mod 
    | "rcv" -> single () |> Rcv 
    | "jgz" -> pair () |> JumpGtZero 
    | _ -> failwith "Invalid operation"

let operations = System.IO.File.ReadAllLines("inputs/input_day18.txt") |> Array.map toOperation 
let testOperations= System.IO.File.ReadAllLines("inputs/input_day18_test.txt") |> Array.map toOperation 

type State = {
    CurrentOperation: int;
    Operations: Operation array;
    LastSound: int64;
    Registers: Map<char, int64>
}

let rec repeat (state: State) =
    let getReg reg : int64 =
        match reg with 
        | Int regValue -> int64 regValue
        | Char regName -> match Map.tryFind regName state.Registers with | Some value -> value | None -> 0L
    
    let change operation value fromReg =
        match fromReg with
        | Some regValue -> Some (operation regValue value)
        | None -> None
    
    let changeRegValue operation reg = 
        match reg with 
        | Int _ -> state.Registers
        | Char regName -> state.Registers |> Map.change regName operation
    
    if state.CurrentOperation > state.Operations.Length then
        state
    else
        let nextOp = state.Operations.[state.CurrentOperation] 
        //printfn "NextOp: %d:%A; Registers: %A" state.CurrentOperation nextOp state.Registers
        match state.Operations.[state.CurrentOperation] with
        | Snd reg ->  repeat { state with CurrentOperation = state.CurrentOperation + 1; LastSound=reg |> getReg }
        | Set (reg, value) -> repeat { state with CurrentOperation = state.CurrentOperation + 1; Registers = changeRegValue (fun _ -> value |> getReg |> Some) reg }       
        | Add (reg, value) -> repeat { state with CurrentOperation = state.CurrentOperation + 1; Registers = changeRegValue (change (+) (value |> getReg)) reg }
        | Mul (reg, value) -> repeat { state with CurrentOperation = state.CurrentOperation + 1; Registers = changeRegValue (change (*) (value |> getReg)) reg }
        | Mod (reg, value) -> repeat { state with CurrentOperation = state.CurrentOperation + 1; Registers = changeRegValue (change (%) (value |> getReg)) reg }
        | JumpGtZero (reg, value) -> repeat { state with CurrentOperation = state.CurrentOperation + if reg |> getReg > 0L then value |> getReg |> int else 1 }
        | Rcv reg -> if reg |> getReg > 0L then state else repeat { state with CurrentOperation = state.CurrentOperation + 1 }

let initState ops = 
    {
        CurrentOperation = 0;
        Operations = ops;
        LastSound = 0L;
        Registers = Map.empty;
    }

//testOperations |> printfn "%A"

let result = repeat (operations |> initState)

result.LastSound |> printfn "AnswerPartOne: %d"

