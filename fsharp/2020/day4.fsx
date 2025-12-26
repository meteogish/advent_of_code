open System.Text.RegularExpressions
let valuesSplitRegex = new Regex(" |\n")
let hairColorRegex = new Regex("#[\w]{6}")

let requiredKeys = ["ecl"; "pid"; "eyr"; "hcl"; "byr"; "iyr"; "hgt"];
let validEyeColors = ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"];

let toPassport str = 
    valuesSplitRegex.Split(str)
    |> Array.map (fun (pairStr: string) -> 
        let pair = pairStr.Split(':')
        (pair.[0], pair.[1]))
    |> Map.ofArray
    
let validate1 map =
    if requiredKeys |> Seq.forall (fun key -> Map.containsKey key map) then 1 else 0

let validate2 map =
    let isValid (validation, key) = map |> Map.find key |> validation
    let inRange left right value = value >= left && value <= right
    let inRangeStr left right (strValue: string) = 
        (System.Int32.TryParse(strValue) |> (fun (isNum, num) -> if isNum then inRange left right num else isNum))

    let checkEyeColor value =  List.contains value validEyeColors
    let checkPassportId idStr = (idStr |> String.length = 9) && (System.Int32.TryParse(idStr) |> fst)
    let checkHeight (hgt: string) =
        if hgt.Contains("cm") then
            hgt.Substring(0, hgt.Length - 2) |> inRangeStr 150 193
        else 
            hgt.Substring(0, hgt.Length - 2) |> inRangeStr 59 76 

    let checkYearInRange left right year =
        year |> String.length = 4 && (inRangeStr left right year)

    let checkBirthYear byr = checkYearInRange 1920 2002 byr
    let checkIssueYear iyr = checkYearInRange 2010 2020 iyr
    let checkExpirationYear eyr = checkYearInRange 2020 2030 eyr
    let checkHairColor hcl = hairColorRegex.IsMatch(hcl)
    
    let getResult () = [ 
        (checkEyeColor, "ecl"); 
        (checkPassportId, "pid");
        (checkHeight, "hgt");
        (checkBirthYear, "byr");
        (checkIssueYear, "iyr");
        (checkExpirationYear, "eyr"); (checkHairColor, "hcl") ] |> List.forall isValid
        
    if (validate1 map = 1) && getResult() then 1 else 0

System.IO.File.ReadAllText("input_day4.txt").Split("\n\n")
|> Array.sumBy (fun str -> str |> toPassport |> validate2)
|> printfn "Answer: %d" 
