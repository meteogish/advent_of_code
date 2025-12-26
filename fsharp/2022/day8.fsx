open System.IO
open FSharp.Core
open System

// This is the HARDEST day for me.
// Even after I copied the solution from the link below and debugged it many times I still coundln't wrap my head around it.
// source: https://www.reddit.com/r/adventofcode/comments/zfpnka/comment/j1pqg1z/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button

let z = (char) 0

type Cell =
    { mutable visible: bool
      mutable left: char
      mutable right: char
      mutable top: char
      mutable bottom: char }

    static member Default =
        { visible = false
          left = z
          right = z
          top = z
          bottom = z }


let apply_tree (height: char) (vision: char array) =
    let hi = int (height - z)
    let v = vision[hi]

    for i in 0 .. vision.Length - 1 do
        if i < hi + 1 then
            vision[i] <- z
        else
            vision[i] <- vision[i] + (char) 1
    v

let update_range_left_top (cell: Cell) height vision_top vision_left =
    cell.left <- apply_tree height vision_left
    cell.top <- apply_tree height vision_top

let update_range_right_bottom (cell: Cell) height vision_bottom vision_right =
    cell.right <- apply_tree height vision_right
    cell.bottom <- apply_tree height vision_bottom


let check_scene (cell: Cell) i j len =
    let (left, top, right, bottom) =
        (int cell.left = j, int cell.top = i, int cell.right = len - j, int cell.bottom = len - i)

    let vis =
        if (not cell.visible) && (left || top || right || bottom) then
            cell.visible <- true
            true
        else
            false
    
    let scenicity = 
        (int cell.left + if left then 0 else 1)
        * (int cell.top + if top then 0 else 1)
        * (int cell.right + if right then 0 else 1)
        * (int cell.bottom + if bottom then 0 else 1)

    vis,scenicity


let read_grid path =
    let lines = File.ReadAllLines(path)

    lines
    |> Array.map (fun lineStr -> lineStr.ToCharArray() |> Array.map (fun ch -> char (ch - '0')))

//let grid = read_grid "input8"
let path = "/Users/yevhenii/Documents/Repositories/leetcode_solutions/leet_rust/src/test8";
//let path = "/Users/yevhenii/Documents/Repositories/leetcode_solutions/leet_rust/src/input8";
let grid = read_grid path


let N = grid.Length
let E = N - 1


let cells = Array.init (N * N) (fun _ -> Cell.Default)

let rovLeft = Array.create 10 z
let rovRight = Array.create 10 z
let prev_row = Array.init N (fun i -> Array.create 10 z)
let next_row = Array.init N (fun i -> Array.create 10 z)

let mutable visibles = 0
let mutable scenic_score = 0

for i in 0..E do
    Array.fill rovLeft 0 10 z
    Array.fill rovRight 0 10 z

    for j in 0..E do

        let doWork updateFunc i j =
            let c = cells[i * N + j]
            let h = grid[i][j]
            updateFunc c h
            let became_visible, scenic = check_scene c i j E
            visibles <- visibles + if became_visible then 1 else 0
            scenic_score <- Math.Max (scenic_score, scenic)
        
        doWork (fun c h ->
            update_range_left_top 
                c 
                h
                prev_row[j] 
                rovLeft) i j
        
        let i = E - i
        let j = E - j
        
        doWork (fun c h ->
            update_range_right_bottom
                c 
                h
                next_row[j] 
                rovRight) i j

printfn "Part1 - visibles: %d" visibles
printfn "Part2 - scenic: %d" scenic_score