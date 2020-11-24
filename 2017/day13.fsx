//https://adventofcode.com/2017/day/13

let mapLine (str: string) =
    let [| depth; range |] = str.Split(':') |> Array.map int 
    (depth, range, 2 * (range - 1))

let day13 = System.IO.File.ReadAllLines("inputs/input_day13.txt") |> Seq.map mapLine 

let caughtAt delay (depth, _, fullCycle) =
    (delay + depth) % fullCycle = 0

let getSeverity (depth, range, fullCycle) =
    if caughtAt 0 (depth, range, fullCycle) then
        depth * range
    else 0

let findMinimumNotCaughtDelay items =
    let rec notCaughtForAllAt i =
        if items |> Seq.forall (caughtAt i >> not) then
            i
        else i + 1 |> notCaughtForAllAt

    notCaughtForAllAt 0


day13 |> Seq.sumBy getSeverity |> printfn "AnswerPartOne: %d"
day13 |> findMinimumNotCaughtDelay |> printfn "AnswerPartTwo: %d"