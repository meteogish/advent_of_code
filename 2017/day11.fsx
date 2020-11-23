//https://adventofcode.com/2017/day/11

let day11 = System.IO.File.ReadAllText("inputs/input_day11.txt").Split(',') 

let axialMap = [
    ("n", (0, -1));
    ("ne", (1, -1));
    ("se", (1, 0));
    ("s", (0, 1));
    ("sw", (-1, 1));
    ("nw", (-1, 0)); ] |> Map.ofSeq

let axialDistance (q1, r1) (q2, r2) =
    (abs (q1 - q2) + abs (q1 + r1 - q2 - r2) + abs (r1 - r2)) / 2

let third (_, _, t) = t

let getAnswers results =
    (results |> Seq.last |> third, 
        results |> Seq.maxBy third |> third)

let folder (accQ, accR, _) (dirQ, dirR) =
    let (nextQ, nextR) = (accQ + dirQ, accR + dirR)
    let nextDist = axialDistance (nextQ, nextR) (0,0)
    (nextQ, nextR, nextDist) 

day11 
|> Seq.map (fun str -> axialMap.[str]) 
|> Seq.scan folder (0,0,0)
|> getAnswers
|> printfn "(AnswerPartOne, AnswerPartTwo) = %A" 