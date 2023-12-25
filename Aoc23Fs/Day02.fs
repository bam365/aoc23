module Day02


type Color = Red | Blue | Green

module PullSet = 
    type T = Map<Color, int>

    let empty: T = Map.empty

    let fromSpecs xs = 
        let folder acc v = 
            Map.change (fst v) (fun x -> Some ((Option.defaultValue 0 x) + (snd v))) acc
        List.fold folder empty xs

    let zeroes = fromSpecs [Red, 0; Green, 0; Blue, 0]

    let minimumNeeded ts = 
        let folder = Map.fold (fun acc key v -> 
            let updater av = Some (max (Option.defaultValue 0 av) v)
            Map.change key updater acc
        )
        List.fold folder zeroes ts

    let power t = 
        let red = Map.tryFind Red t |> Option.defaultValue 0
        let green = Map.tryFind Green t |> Option.defaultValue 0
        let blue = Map.tryFind Blue t |> Option.defaultValue 0
        red * green * blue

    module Parsers =
        open FParsec

        let color: Parser<Color, unit> = 
            (pstring "red" >>. preturn Red) 
            <|> (pstring "green" >>. preturn Green)
            <|> (pstring "blue" >>. preturn Blue)

        let pullSet: Parser<T, unit> = 
            let pullSetPair = parse {
                do! spaces
                let! num = pint32
                do! spaces1
                let! color = color
                do! spaces
                return color, num
            }
            parse {
                let! pairs = sepBy1 pullSetPair (pstring ",")
                return fromSpecs pairs
            }

    
    
module Game =
    type T = { GameNum: int; sets: PullSet.T list }

    let isPossible (maxes: PullSet.T) game = 
        let getMax color = Map.tryFind color maxes |> Option.defaultValue 0
        let setPossible (pullSet : PullSet.T) =
            let getColor color = Map.tryFind color pullSet |> Option.defaultValue 0
            getColor Red <= getMax Red && getColor Green <= getMax Green && getColor Blue <= getMax Blue
        game.sets
        |> List.tryFind (setPossible >> not)
        |> Option.isNone
    
    module Parsers =
        open FParsec

        let game: Parser<T, unit> = parse {
            do! pstring "Game " >>. preturn ()
            let! gnum =  pint32
            do! pstring ": " >>. preturn ()
            let! sets = sepBy PullSet.Parsers.pullSet (pstring ";")
            return { GameNum = gnum; sets = sets }
        }

let readGames () = 
    Aoclib.readLines ()
    |> Seq.map (fun line -> 
        Aoclib.ParseUtils.parseOrExn Game.Parsers.game line
    )
let part1 () = 
    let maxes = PullSet.fromSpecs [Red, 12; Green, 13; Blue, 14]
    readGames ()
    |> Seq.filter (Game.isPossible maxes)
    |> Seq.fold (fun acc v -> acc + v.GameNum) 0
    |> printfn "%d"

let part2 () =
    readGames ()
    |> Seq.map (fun game -> game.sets |> PullSet.minimumNeeded |> PullSet.power)
    |> Aoclib.seqSum
    |> printfn "%d"