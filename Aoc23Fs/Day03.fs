module Day03

type Point = {
    Row: int
    Col: int
}

type NumSpec = {
    Start: Point
    Num: string
}

let addCharToNumSpec c t = { t with Num = sprintf "%s%c" t.Num c }

type ParseState = {
    Pos: Point 
    NumRegister: NumSpec option
    Numbers: NumSpec list
    Symbols: Set<Point>
    Stars: Set<Point>
}

let rec parseEngineSpec (acc: ParseState) chars =
    let endReg = { acc with Numbers = acc.Numbers @ (Option.toList acc.NumRegister); NumRegister = None }
    let nextCol = { acc.Pos with Col = acc.Pos.Col + 1 }
    match chars with
    | []    -> acc.Numbers @ (Option.toList acc.NumRegister), acc.Symbols, acc.Stars
    | c::cs -> 
        let acc' =
            match c with
            | '.'  -> { endReg with Pos = nextCol }
            | '\n' -> { endReg with Pos = { Col = 0; Row = acc.Pos.Row + 1 } }
            | digit when digit >= '0' && digit <= '9' -> 
                match acc.NumRegister with
                | None -> { acc with Pos = nextCol; NumRegister = Some({ Start = acc.Pos; Num = sprintf "%c" digit }) }
                | Some(numReg) -> { acc with Pos = nextCol; NumRegister = Some(addCharToNumSpec digit numReg) }
            | symbol -> 
                let stars = if symbol = '*' then Set.add acc.Pos acc.Stars else acc.Stars
                { endReg with Pos = nextCol; Symbols = Set.add acc.Pos acc.Symbols; Stars = stars }

        parseEngineSpec acc' cs

let adjacentPoints numSpec =
    seq {
        for row in numSpec.Start.Row - 1 .. numSpec.Start.Row + 1 do
        for col in numSpec.Start.Col - 1 .. numSpec.Start.Col + (String.length numSpec.Num) do
            { Row = row; Col = col }
    }

let isPartSpec symbols numSpec = 
    adjacentPoints numSpec
    |> Seq.tryFind (fun p -> Set.contains p symbols)
    |> Option.isSome

let gearRatios numSpecs stars = 
    let folder acc ns = 
        adjacentPoints ns
        |> Seq.filter (fun p -> Set.contains p stars)
        |> Seq.fold (fun pacc p -> Map.change p (fun v -> Some(ns.Num :: (Option.defaultValue [] v))) pacc) acc
    List.fold folder Map.empty numSpecs
    |> Map.values
    |> Seq.choose (function 
        | [g1; g2] -> Some(int g1 * int g2)
        | _ -> None
    )
    
let readSpecs () = 
    let initState = { 
        Pos = { Row = 0; Col = 0 };
        NumRegister = None;
        Numbers = [];
        Symbols = Set.empty;
        Stars = Set.empty 
    }
    let input = Aoclib.readChars ()
    parseEngineSpec initState input

let part1 () = 
    let (numbers, symbols, _) = readSpecs ()
    numbers 
    |> List.choose (fun ns -> if isPartSpec symbols ns then Some(int ns.Num) else None)
    |> Seq.sum
    |> printfn "%d"

let part2 () =
    let (numbers, _, stars) = readSpecs ()
    gearRatios numbers stars
    |> Seq.sum
    |> printfn "%d"
