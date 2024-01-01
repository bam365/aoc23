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
}

let rec parseEngineSpec (acc: ParseState) chars =
    let endReg = { acc with Numbers = acc.Numbers @ (Option.toList acc.NumRegister); NumRegister = None }
    let nextCol = { acc.Pos with Col = acc.Pos.Col + 1 }
    match chars with
    | []    -> acc.Numbers @ (Option.toList acc.NumRegister), acc.Symbols
    | c::cs -> 
        let acc' =
            match c with
            | '.'  -> { endReg with Pos = nextCol }
            | '\n' -> { endReg with Pos = { Col = 0; Row = acc.Pos.Row + 1 } }
            | digit when digit >= '0' && digit <= '9' -> 
                match acc.NumRegister with
                | None -> { acc with Pos = nextCol; NumRegister = Some({ Start = acc.Pos; Num = sprintf "%c" digit }) }
                | Some(numReg) -> { acc with Pos = nextCol; NumRegister = Some(addCharToNumSpec digit numReg) }
            | symbol -> { endReg with Pos = nextCol; Symbols = Set.add acc.Pos acc.Symbols }
        parseEngineSpec acc' cs

let isPartSpec numSpec symbols = 
    seq {
        for row in numSpec.Start.Row - 1 .. numSpec.Start.Row + 1 do
        for col in numSpec.Start.Col - 1 .. numSpec.Start.Col + (String.length numSpec.Num) do
            { Row = row; Col = col }
    }
    |> Seq.tryFind (fun p -> Set.contains p symbols)
    |> Option.isSome

let part1 () = 
    let input = Aoclib.readChars ()
    let initState = { Pos = { Row = 0; Col = 0 }; NumRegister = None; Numbers = []; Symbols = Set.empty }
    let (numbers, symbols) = parseEngineSpec initState input
    numbers 
    |> List.choose (fun ns -> if isPartSpec ns symbols then Some(int ns.Num) else None)
    |> Seq.sum
    |> printfn "%d"
