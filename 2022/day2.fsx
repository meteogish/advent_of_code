open System

let readFileLines path =
    seq {
        use file = System.IO.File.OpenRead(path)
        use reader = new IO.StreamReader(file)

        while not reader.EndOfStream do
            yield reader.ReadLine()
    }

let calcOutcomePart1 oponent me =
    //A for Rock, B for Paper, and C for Scissors
    //X for Rock, Y for Paper, and Z for Scissors

    match (oponent, me) with
    | ('A', 'X') -> 1 + 3
    | ('A', 'Y') -> 2 + 6
    | ('A', 'Z') -> 3 + 0
    | ('B', 'X') -> 1 + 0
    | ('B', 'Y') -> 2 + 3
    | ('B', 'Z') -> 3 + 6
    | ('C', 'X') -> 1 + 6
    | ('C', 'Y') -> 2 + 0
    | ('C', 'Z') -> 3 + 3

let calcOutcomePart2 oponent me =
    //X means you need to lose, Y means you need to end the round in a draw, and Z means you need to win
    match (oponent, me) with
    | ('A', 'X') -> 3 + 0
    | ('A', 'Y') -> 1 + 3
    | ('A', 'Z') -> 2 + 6
    | ('B', 'X') -> 1 + 0
    | ('B', 'Y') -> 2 + 3
    | ('B', 'Z') -> 3 + 6
    | ('C', 'X') -> 2 + 0
    | ('C', 'Y') -> 3 + 3
    | ('C', 'Z') -> 1 + 6


let processLine (score1, score2) (line: string) =
    let oponent = line.[0]
    let me = line.[2]
    let outcome1 = calcOutcomePart1 oponent me
    let outcome2 = calcOutcomePart2 oponent me

    (score1 + outcome1, score2 + outcome2)

let (score1, score2) = "input_day2" |> readFileLines |> Seq.fold processLine (0, 0)

printfn "Part one: %d" score1
printfn "Part two: %d" score2
