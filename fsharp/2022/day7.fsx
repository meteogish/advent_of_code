open System
open System.Collections.Generic

type Dirs = Dictionary<string, int>

let readFileLines path =
    seq {
        use file = System.IO.File.OpenRead(path)
        use reader = new IO.StreamReader(file)

        while not reader.EndOfStream do
            yield reader.ReadLine()
    }
    
let (|Int|_|) (s: string) =
    match Int32.TryParse s with
    | true, n -> Some(Int n)
    | false, _ -> None
    
let split (on: char) (s: string) =
    s.Split(on, StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
    |> Array.toList
    
let folder (path, dirs: Dirs) line =

    let parts = (split ' ' line)
    
    match parts with
    | ["$"; "ls"] ->
        (path, dirs)
    | ["$"; "cd"; nextDir] ->
        match nextDir with
        | ".." -> 
            (path |> List.tail, dirs)
        | "/" -> (List.singleton "/", dirs)
        | _ -> 
            (nextDir:: path, dirs)

    | ["dir"; newDir] ->
        let fullPath = newDir :: path |> List.rev |> Seq.ofList |> String.concat "/"
        printfn $"{fullPath}"
        dirs[fullPath] <- 0
        (path, dirs)
    | [Int size; fileName] -> 
        let rec fulfillSizes path =
            let fullPath = path |> List.rev |> Seq.ofList |> String.concat "/"
            dirs[fullPath] <- dirs[fullPath] + size
            
            printfn $"new size of '{fullPath}' is {dirs[fullPath]}"

            match path with
            | _ :: [] -> dirs
            | [] -> dirs
            | _ :: rest -> fulfillSizes rest

        (path, fulfillSizes path)

    | _ -> failwith ("wrong line " + line)


let dirs = Dirs()
dirs["/"] <- 0

readFileLines "input7"
|> Seq.fold folder (List.empty, dirs)
|> fun (_, dirs) -> 

    let freeupSpace = 30000000 - (70000000 - dirs["/"])

    dirs
    |> Seq.fold (fun (sum, dir2) pair -> 
        let parOne = if pair.Value <= 100000 then sum + pair.Value else sum
        let partTwo = if pair.Value >= freeupSpace then Math.Min(dir2,  pair.Value) else dir2
        (parOne, partTwo)
        ) (0, Int32.MaxValue)
|> printfn "%A"