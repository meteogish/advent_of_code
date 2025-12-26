open System

let readFileLines path =
    seq {
        use file = System.IO.File.OpenRead(path)
        use reader = new IO.StreamReader(file)

        while not reader.EndOfStream do
            yield reader.ReadLine()
    }

let toPriority (char: char) =
    if char >= 'a' && char <= 'z' then
        (int char) - (int 'a') + 1
    else
        (int char) - (int 'A') + 27

let countPartOne line =
    let len = line |> String.length

    let set1 = line.Substring(0, len / 2) |> Seq.toArray |> Set.ofArray
    let set2 = line.Substring(len / 2) |> Seq.toArray |> Set.ofArray

    let inter = Set.intersect set1 set2

    inter |> Seq.head |> toPriority

let processLine2 (partOne, (partTwo, lines: string list)) (line: string) =
    let partOneSum = countPartOne line |> (+) partOne

    if lines.Length = 2 then
        let sets = (line :: lines) |> Seq.map (Seq.toList >> Set.ofList)
        (partOneSum, (sets |> Set.intersectMany |> Seq.head |> toPriority |> (+) partTwo, []))
    else
        (partOneSum, (partTwo, line :: lines))

let (partOne, (partTwo, _)) =
    "input_day3" |> readFileLines |> Seq.fold processLine2 (0, (0, []))

printfn "PartOne: %d" partOne
printfn "PartTwo: %d" partTwo
