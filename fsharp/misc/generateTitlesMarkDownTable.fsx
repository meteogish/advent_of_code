open System.Net.Http
open System.Text.RegularExpressions
let linkBase = "https://adventofcode.com/%d/day/%d"

let getTitle (client: HttpClient) (rx:Regex) year day =
    let link = sprintf (Printf.StringFormat<int->int->string> linkBase) year day
    
    async {
        let! text = client.GetStringAsync (link) |> Async.AwaitTask
        let m = rx.Match (text)
        let res =  m.Groups.[1].Value
        return (day, res)
    }

let toMd year (day, name) =
    sprintf "| [%s](./%d/day%d.fsx) |" name year day

let getTitlesMarkDown year =
    use client = new HttpClient ()
    let rx = Regex("<h2>--- (Day \w+: .*) ---<\/h2>", RegexOptions.Compiled)

    { 1 .. 25 } 
    |> Seq.map (getTitle client rx year)
    |> Async.Parallel 
    |> Async.RunSynchronously 
    |> Seq.map (toMd year)
    
getTitlesMarkDown 2019
|> Seq.append [ "| |" ; "| - |" ] 
|> String.concat "\n" 
|> printfn "%s"