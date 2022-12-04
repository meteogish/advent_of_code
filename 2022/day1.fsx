open System

let readFileLines path =
    seq {
        use file = System.IO.File.OpenRead(path)
        use reader = new IO.StreamReader(file)

        while not reader.EndOfStream do
            yield reader.ReadLine()
    }

let processLine (computed, inHand) line =
    if line |> String.IsNullOrEmpty then
        let sumInHand = inHand |> Seq.sum
        (computed @ [ sumInHand ], [])
    else
        (computed, inHand @ [ line |> int ])

let computedResults =
    "input_day1" |> readFileLines |> Seq.fold processLine ([], []) |> fst

let topThree = computedResults |> List.sortDescending |> List.take 3

let sumOfTopThree = topThree |> List.sum

printfn "Part one: %d" topThree.[0]
printfn "Part two: %d" sumOfTopThree
