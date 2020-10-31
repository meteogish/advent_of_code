let lines = System.IO.File.ReadAllLines("input_day5.txt")

let nums = lines |> Seq.map int |> Array.ofSeq

let offsetChangerPartOne offset =
    offset + 1

let offsetChangerPartTwo offset =
    offset + (if offset >= 3 then -1 else 1)

let rec proc offsetChanger acc currentPos =
    if currentPos < 0 || currentPos >= nums.Length then 
        acc 
    else
        let offset = nums.[currentPos]
        nums.[currentPos] <- offset |> offsetChanger 
        proc offsetChanger (acc + 1) (currentPos + offset)

//Remember to uncomment only single part. After each run the numbers are mutating which can lead to invalid results.
//printfn "AnswerPartOne: %d" (proc offsetChangerPartOne 0 0) //378980
printfn "AnswerPartTwo: %d" (proc offsetChangerPartTwo 0 0) //26889114


