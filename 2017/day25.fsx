let states = 
    [
        ('A', ((1, 1, 'B'), (0, 1, 'F')))
        ('B', ((0, -1, 'B'), (1, -1, 'C')))
        ('C', ((1, -1, 'D'), (0, 1, 'C')))
        ('D', ((1, -1, 'E'), (1, 1, 'A')))
        ('E', ((1, -1, 'F'), (0, -1, 'D')))
        ('F', ((1, 1, 'A'), (0, -1, 'E')))
    ] |> Map.ofList
        
let diag = 12964419
let rec proceed tape cursor state stepI =
    if stepI = diag then
        tape
    else
        let currentValue = if tape |> Map.containsKey cursor then tape.[cursor] else 0
        let chooser = if currentValue = 0 then fst else snd

        let (nextValue, step, nextState) = states.[state] |> chooser
        let newTape = tape |> Map.change cursor (fun _ -> nextValue |> Some)
        stepI + 1
        |> proceed newTape (cursor + step) nextState

let resultPartOne = proceed Map.empty 0 'A' 0 |> Map.toList |> List.sumBy snd

resultPartOne |> printfn "PartOne: %d"

           
        
        
    