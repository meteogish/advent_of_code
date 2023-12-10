open System
open System.Text.RegularExpressions

let readFileLines path =
    seq {
        use file = System.IO.File.OpenRead(path)
        use reader = new IO.StreamReader(file)

        while not reader.EndOfStream do
            yield reader.ReadLine()
    }

let rx = Regex("(\\d*)-(\\d*),(\\d*)-(\\d*)")

let parse str =
    let rxResult = rx.Match(str)

    let get (i: int) = rxResult.Groups.[i].Value |> int

    ((get 1, get 2), (get 3, get 4))

let inRange (l1, l2) (r1, r2) = l1 <= r1 && l2 >= r2

let contains range1 range2 =
    range2 |> inRange range1 || range1 |> inRange range2

let overlaps (l1, r1) (l2, r2) =
    (l1 >= l2 && l1 <= r2) || (r1 >= l2 && r1 <= r2)

let overlapsEither range1 range2 =
    range2 |> overlaps range1 || range1 |> overlaps range2

let processLine (countContains, countOverlaps) line =
    let (range1, range2) = parse line

    let addContains = if range1 |> contains range2 then 1 else 0
    let addOverlaps = if range1 |> overlapsEither range2 then 1 else 0

    (countContains + addContains, countOverlaps + addOverlaps)

"input4" |> readFileLines |> Seq.fold processLine (0, 0) |> printfn "%A"
