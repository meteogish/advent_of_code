let toSeatId (lineStr: string) =
    let toInt (str:string) = System.Convert.ToInt32(str, 2)

    let row = lineStr.Substring(0, 7).Replace("F", "0").Replace("B", "1") |> toInt
    let col = lineStr.Substring(7, 3).Replace("L", "0").Replace("R", "1") |> toInt

    row * 8 + col

let findMySeatId (previousSeatId, mySeatIdSoFar) currentSeatId =
    if currentSeatId - previousSeatId = 2 then (currentSeatId, previousSeatId + 1) 
    else (currentSeatId, mySeatIdSoFar)

System.IO.File.ReadAllLines("input_day5.txt")
|> Array.map toSeatId
|> Array.sort
|> Array.fold findMySeatId  (-1, -1)
|> fun (maxId, mySeatId) -> printfn "Answer to part 1: %d; part 2: %d" maxId mySeatId
