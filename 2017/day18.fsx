//https://adventofcode.com/2017/day/18

open System.Collections.Generic

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

type State = {
    CurrentOperation: int;
    Operations: Operation array;
    LastSound: int64;
    Registers: Map<char, int64>
}

let getRegisterValue registers reg : int64 =
    match reg with 
    | Int regValue -> int64 regValue
    | Char regName -> match Map.tryFind regName registers with | Some value -> value | None -> 0L

let change operation value fromReg =
    match fromReg with
    | Some regValue -> Some (operation regValue value)
    | None -> None

let changeRegisterValue registers operation reg = 
    match reg with 
    | Int _ -> registers
    | Char regName -> registers |> Map.change regName operation

let rec repeatPartOne (state: State) =
    let getReg = getRegisterValue state.Registers
    let changeRegValue = changeRegisterValue state.Registers

    if state.CurrentOperation > state.Operations.Length then
        state
    else
        let nextOp = state.Operations.[state.CurrentOperation] 
        //printfn "NextOp: %d:%A; Registers: %A" state.CurrentOperation nextOp state.Registers
        match nextOp with
        | Snd reg ->  repeatPartOne { state with CurrentOperation = state.CurrentOperation + 1; LastSound=reg |> getReg }
        | Set (reg, value) -> repeatPartOne { state with CurrentOperation = state.CurrentOperation + 1; Registers = changeRegValue (fun _ -> value |> getReg |> Some) reg }       
        | Add (reg, value) -> repeatPartOne { state with CurrentOperation = state.CurrentOperation + 1; Registers = changeRegValue (change (+) (value |> getReg)) reg }
        | Mul (reg, value) -> repeatPartOne { state with CurrentOperation = state.CurrentOperation + 1; Registers = changeRegValue (change (*) (value |> getReg)) reg }
        | Mod (reg, value) -> repeatPartOne { state with CurrentOperation = state.CurrentOperation + 1; Registers = changeRegValue (change (%) (value |> getReg)) reg }
        | JumpGtZero (reg, value) -> repeatPartOne { state with CurrentOperation = state.CurrentOperation + if reg |> getReg > 0L then value |> getReg |> int else 1 }
        | Rcv reg -> if reg |> getReg > 0L then state else repeatPartOne { state with CurrentOperation = state.CurrentOperation + 1 }

let initStatePartOne ops = 
    {
        CurrentOperation = 0;
        Operations = ops;
        LastSound = 0L;
        Registers = Map.empty;
    }

(repeatPartOne (operations |> initStatePartOne)).LastSound |> printfn "AnswerPartOne: %d"


// Part Two

type RegisterState = {
    CurrentOperation: int;
    Queue: Queue<int64>;
    CountSend: int;
    Registers: Map<char, int64> 
}


let rec repeatPartTwo (operations: Operation array) (first: RegisterState, second: RegisterState) =
    let move op state send =
        let getReg = getRegisterValue state.Registers
        let changeRegValue = changeRegisterValue state.Registers 

        match op with 
        | Snd reg -> 
            reg |> getReg |> send
            { state with CurrentOperation = state.CurrentOperation + 1; CountSend= state.CountSend + 1 }
        | Set (reg, value) -> 
            { state with 
                CurrentOperation = state.CurrentOperation + 1; 
                Registers = changeRegValue (fun _ -> value |> getReg |> Some) reg }       
        | Add (reg, value) -> { state with CurrentOperation = state.CurrentOperation + 1; Registers = changeRegValue (change (+) (value |> getReg)) reg }
        | Mul (reg, value) -> { state with CurrentOperation = state.CurrentOperation + 1; Registers = changeRegValue (change (*) (value |> getReg)) reg }
        | Mod (reg, value) -> { state with CurrentOperation = state.CurrentOperation + 1; Registers = changeRegValue (change (%) (value |> getReg)) reg }
        | JumpGtZero (reg, value) -> { state with CurrentOperation = state.CurrentOperation + if reg |> getReg > 0L then value |> getReg |> int else 1 }
        | Rcv reg -> if state.Queue.Count = 0 then state else { state with CurrentOperation = state.CurrentOperation + 1; Registers = changeRegValue (fun _ -> state.Queue.Dequeue() |> Some) reg  }

    let firstNextOperation = operations.[first.CurrentOperation]
    let secondNextOperation = operations.[second.CurrentOperation]

    match firstNextOperation, secondNextOperation with 
    | Rcv _, Rcv _ when first.Queue.Count = 0 && second.Queue.Count = 0 -> first, second
    | _, _ ->
        repeatPartTwo operations (move firstNextOperation first (fun value -> second.Queue.Enqueue(value)), move secondNextOperation second (fun value -> first.Queue.Enqueue(value)))


let initStatePartTwo pValue =
    {
        CurrentOperation = 0;
        Queue = Queue<int64> ();
        CountSend = 0;
        Registers = Map.add 'p' pValue Map.empty;
    }

let countSend state = state.CountSend

repeatPartTwo operations (initStatePartTwo 0L, initStatePartTwo 1L) |> snd |> countSend |> printfn "AnswerPartTwo: %d"
