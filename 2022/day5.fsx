open System
open System.Collections.Generic

type Stacks = Stack<char> array

let readFileLines path =
    seq {
        use file = System.IO.File.OpenRead(path)
        use reader = new IO.StreamReader(file)

        while not reader.EndOfStream do
            yield reader.ReadLine()
    }

let rec poppulateStacks (stacks: Stacks) (line: string) =
    match stacks |> Array.isEmpty with
    | true -> 
        let newStacks = Array.init (line.Length / 4 + 1) (fun _ -> new Stack<char>())
        poppulateStacks newStacks line
    | false ->
        let s = seq {0 .. line.Length / 4 }
        s
        |> Seq.map (fun i -> line.Chars ((i * 4)+1))
        |> Seq.fold (fun (stacks, i) cell -> 

            if Char.IsWhiteSpace cell then
                (stacks, i+1)
            else 
                let stack: Stack<char> = Array.get stacks i
                stack.Push cell
                (stacks, i+1)

        ) (stacks, 0)
        |> fst

let parseMoveLine (line: string) =
    line.Split ' '
    |> Seq.choose (fun part -> match Int32.TryParse part with | (true, v) -> Some v | _ -> None)
    
let movePart1 (count, fromI, toI) (stacks: Stacks) =
    let fromStack =  Array.get stacks fromI
    let toStack =  Array.get stacks toI

    for i in 0..(count - 1) do
        let x = fromStack.Pop()
        toStack.Push(x)

    stacks

let movePart2 (count, fromI, toI) (stacks: Stacks) =
    let fromStack =  Array.get stacks fromI
    let toStack =  Array.get stacks toI

    { 0..(count - 1) }
    |> Seq.map (fun _ -> fromStack.Pop())
    |> Seq.rev
    |> Seq.iter (fun ch -> toStack.Push(ch))
    |> ignore

    stacks

let rearrange move stacks (line: string) =
    
    if line.StartsWith ("move") then
        let [| count; fromIndex; toIndex |] = (parseMoveLine line |> Array.ofSeq)
        
        //printfn "%i, %i, %i" count fromIndex toIndex
        move (count, fromIndex - 1, toIndex - 1) stacks
    else if line.Contains ('[') then
        poppulateStacks stacks line
    else if String.IsNullOrEmpty line then
        stacks
        |> Array.map (fun stack -> Stack<char>(stack))
    else 
        stacks

//readFileLines "example5"
readFileLines "input5"
//|> Seq.take 8
|> Seq.fold (fun state line -> rearrange movePart2 state line) (Array.empty) 
//|> Seq.iter (fun stack -> printfn "\n\n%s" (String.Join(',', stack |> Array.ofSeq)))
|> Seq.map (fun stack -> stack.Pop())
|> fun chars -> String(chars |> Array.ofSeq) |> printfn "%s"




