open System

let day01 () = 
    let lineNum (line: string) = 
        let lastDigit = Aoclib.charDigit (Seq.findBack Char.IsDigit line)
        let firstDigit = Aoclib.charDigit (Seq.find Char.IsDigit line)
        firstDigit * 10 + lastDigit
    Aoclib.readLines ()
    |> Seq.map lineNum
    |> Seq.fold (+) 0
    |> printfn "%d"

let strToDigit (s: string) = 
    if s.StartsWith("1") then Some(1)
    else if s.StartsWith("2") then Some(2)
    else if s.StartsWith("3") then Some(3)
    else if s.StartsWith("4") then Some(4)
    else if s.StartsWith("5") then Some(5)
    else if s.StartsWith("6") then Some(6)
    else if s.StartsWith("7") then Some(7)
    else if s.StartsWith("8") then Some(8)
    else if s.StartsWith("9") then Some(9)
    else if s.StartsWith("one") then Some(1)
    else if s.StartsWith("two") then Some(2)
    else if s.StartsWith("three") then Some(3)
    else if s.StartsWith("four") then Some(4)
    else if s.StartsWith("five") then Some(5)
    else if s.StartsWith("six") then Some(6)
    else if s.StartsWith("seven") then Some(7)
    else if s.StartsWith("eight") then Some(8)
    else if s.StartsWith("nine") then Some(9)
    else None

let day01Part2 () = 
    let lineNum (line: string) =
        let nums = 
            seq { 0 .. (String.length line) - 1 }
            |> Seq.map(fun i -> line.Substring(i) |> strToDigit)
            |> Seq.choose id
            |> Seq.toArray
        (Array.head nums) * 10 + (Array.last nums)
    Aoclib.readLines ()
    |> Seq.map lineNum
    |> Seq.fold (+) 0
    |> printfn "%d"
    
        
let args = Environment.GetCommandLineArgs()

match args.[1] with
    | "1" -> day01 ()
    | "1_2" -> day01Part2()
    | "2" -> Day02.part1 ()
    | "2_2" -> Day02.part2 ()
    | "3" -> Day03.part1 ()
    | "3_2" -> Day03.part2 ()
    | _ -> failwith "lol nope"
