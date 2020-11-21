//https://adventofcode.com/2017/day/9

open System.Collections.Generic

let day9 = System.IO.File.ReadAllText("input_day9.txt")

day9.Length |> printfn "%d" 

type State =
    {
        Groups: Stack<int>;
        IsGarbage: bool;
        IsIgnored: bool;
        GroupsValue: int;
        CancelledCharacters: int;
    }

let initialState = {
    Groups = Stack<int>(0);
    IsGarbage = false;
    IsIgnored = false;
    GroupsValue = 0;
    CancelledCharacters = 0;
}

let countGroups input =
    let folder state c =
        let newGroup value = 
            state.Groups.Push (value)
            state

        match c with
        | '!' when not state.IsIgnored -> { state with IsIgnored = true }
        | _ when state.IsIgnored -> { state with IsIgnored = false } 
        | '<' when not state.IsGarbage -> { state with IsGarbage = true }
        | '>' when state.IsGarbage -> { state with IsGarbage = false }
        | _ when state.IsGarbage -> { state with CancelledCharacters = state.CancelledCharacters + 1 }
        | '{' when state.Groups.Count > 0 -> newGroup (state.Groups.Peek() + 1)
        | '{' -> newGroup 1 
        | '}' when state.Groups.Count > 0 -> { state with GroupsValue = state.GroupsValue + state.Groups.Pop() } 
        | '}' -> failwith "No groups opened" 
        | _ -> state

    input |> Seq.fold folder initialState

let result = day9 |> countGroups
let answerPartOne = result.GroupsValue 
let answerPartTwo = result.CancelledCharacters

answerPartOne |> printfn "AnswerPartOne: %d"
answerPartTwo |> printfn "AnswerPartTwo: %d"
