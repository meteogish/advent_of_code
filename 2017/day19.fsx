//https://adventofcode.com/2017/day/19

let day19 = System.IO.File.ReadAllLines("inputs/input_day19.txt") |> Array.map (fun str -> str.ToCharArray())

let left = (0, -1)
let right = (0, 1)
let up = (-1, 0)
let down = (1, 0)

let vertical = '|'
let horizontal = '-'
let cross = '+'
let empty = ' '

let outOfGrid (posY, posX) (grid: char array array) =
    posY < 0 || posX < 0 || grid.Length <= posY|| grid.[0].Length <= posX

let getChar (posY, posX) (grid: char array array) =
    if grid |> outOfGrid (posY, posX) then empty else grid.[posY].[posX] 

let getPosition (posY, posX) (dirY, dirX) =
    (posY + dirY, posX + dirX)

let rec walk currentPosition direction (grid: char array array) letters count =
    let currentChar = grid |> getChar currentPosition
    let nextPossiblePosition = direction |> getPosition currentPosition
    let nextCount = count + 1
    
    let chooseDirectionBetween firstDir secondDir =
        let newDir =  
            if getChar (getPosition currentPosition firstDir) grid = empty then secondDir
            else firstDir 
        walk (newDir |> getPosition currentPosition) newDir grid letters nextCount
    
    if currentChar = vertical || currentChar = horizontal then 
        walk nextPossiblePosition direction grid letters nextCount
    else if currentChar = cross && (direction = down || direction = up) then 
        chooseDirectionBetween left right
    else if currentChar = cross && (direction = left || direction = right) then 
        chooseDirectionBetween up down 
    else if currentChar = empty then 
        (letters, count)
    else 
        walk nextPossiblePosition direction grid (currentChar :: letters) nextCount

let position = (0, day19.[0] |> Seq.findIndex ((=) vertical))
let (letters, count) = walk position down day19 List.empty 0
letters |> List.rev |> printfn "AnswerPartOne: %A"
count |> printfn "AnswerPartTwo: %d"