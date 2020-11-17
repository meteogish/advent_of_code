open System
open System.Text.RegularExpressions
open System.Collections.Generic
#nowarn "57"

let inputLines =
    System.IO.File.ReadAllLines("input_day8.txt")

let regex = Regex ("(\w+) (dec|inc) (-?\d*) if (\w+) (==|!=|<=|>=|<|>) (-?\d*)", RegexOptions.Compiled);

type Instruction = Dec | Inc

type Condition =
    | Eq
    | NotEq
    | Lte 
    | Gte
    | Lt
    | Gt

type InstructionLine = {
    Register: string;
    Instruction: Instruction;
    Value: int;
    ConditionRegister: string;
    Condition: Condition;
    ConditionValue: int;
}

let mapInstruction str =
    match str with
    | "dec" -> Dec
    | "inc" -> Inc
    | _ -> failwith "Invalid instruction"

let mapCondition str =
    match str with 
    | "==" -> Eq
    | "!=" -> NotEq
    | "<=" -> Lte
    | ">=" -> Gte
    | "<" -> Lt
    | ">" -> Gt
    | _ -> failwith "Invalid condition"

let mapInstructionLine str =
    let m = regex.Match (str)
    let get (n: int) = (m.Groups.[n]).Value

    {
        Register = 1 |> get;
        Instruction = 2 |> get |> mapInstruction;
        Value = 3 |> get |> int;
        ConditionRegister = 4 |> get;
        Condition = 5 |> get |> mapCondition;
        ConditionValue = 6 |> get |> int;
    }

let instructions = inputLines |> Array.map mapInstructionLine

let processInstructions () =
    let folder (acc: Dictionary<string, int>) instruction =
        let get key =
            if acc.ContainsKey key then 
                acc.[key]
            else
                acc.[key] <- 0
                0

        let conditionF condition = 
            match condition with
            | Eq -> fun s v -> s |> get = v
            | NotEq -> fun s v -> s |> get <> v
            | Lte -> fun s v -> s |> get <= v
            | Gte -> fun s v -> s |> get >= v
            | Lt -> fun s v -> s |> get < v
            | Gt -> fun s v -> s |> get > v 

        let pass = instruction.Condition |> conditionF

        if pass instruction.ConditionRegister instruction.ConditionValue then
            let currentValue = instruction.Register |> get 
            let newValue = 
                match instruction.Instruction with
                | Inc -> currentValue + instruction.Value
                | Dec -> currentValue - instruction.Value

            acc.[instruction.Register] <- newValue 
            acc
        else 
            acc

    instructions |> Array.fold folder (Dictionary<string, int>(inputLines.Length))

let answerPartOne = (processInstructions ()).Values |> Seq.max

printfn "AnswerPartOne: %d" answerPartOne