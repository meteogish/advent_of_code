//https://adventofcode.com/2017/day/20

open System.Text.RegularExpressions

let rx = Regex("p=<(.+,.+,.+)>, v=<(.+,.+,.+)>, a=<(.+,.+,.+)>", RegexOptions.Compiled)

type Vector = Vec3 of int64 * int64 * int64

type Particle = { 
    Id: int;
    Position: Vector;
    Velocity: Vector;
    Acceleration: Vector;
}

let (++) (Vec3 (x1, y1, z1)) (Vec3 (x2, y2, z2)) = Vec3 (x1 + x2, y1 + y2, z1 + z2)
let equals (Vec3 (x1, y1, z1)) (Vec3 (x2, y2, z2)) = (x1 = x2 && y1 = y2 && z1 = z2) 
let manhattan (Vec3 (x1, y1, z1)) (Vec3 (x2, y2, z2)) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2) 

let tick (particle: Particle) = 
    {
        particle with
            Position = particle.Position ++ particle.Velocity ++ particle.Acceleration;
            Velocity = particle.Velocity ++ particle.Acceleration;
    }

let toParticle i line =
    let toVec (str: string) =
        let [|x; y; z|] = str.Split(',') |> Array.map int64
        Vec3 (x,y,z)

    let m = rx.Match (line)
    {
        Id = i;
        Position = m.Groups.[1].Value |> toVec;
        Velocity = m.Groups.[2].Value |> toVec;
        Acceleration = m.Groups.[3].Value |> toVec;
    }

let particles = System.IO.File.ReadAllLines("inputs/input_day20.txt") |> Array.mapi toParticle

let rec simulate transform count particles =
    if count = 0 then
        particles
    else 
        particles 
        |> transform 
        |> Array.map tick 
        |> simulate transform (count - 1)

let origin = Vec3 (0L, 0L, 0L)

particles
|> simulate id 600
|> Array.minBy (fun p -> p.Position |> manhattan origin)
|> fun p -> p.Id |> printfn "AnswerPartOne: %d"

let removeDuplicates particles = 
    particles
    |> Array.filter (fun p -> particles |> Array.exists (fun p2 -> p.Id <> p2.Id && p.Position |> equals p2.Position) |> not)

particles
|> simulate removeDuplicates 600
|> Seq.length 
|> printfn "AnswerPartTwo: %d"
