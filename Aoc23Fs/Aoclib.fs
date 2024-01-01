module Aoclib

open System


let charDigit (c: char) = Int32.Parse(new String([|c|]))

let readLines () = 
    fun _ -> Console.ReadLine ()
    |> Seq.initInfinite
    |> Seq.takeWhile ((<>) null)

let readStr () = 
    readLines ()
    |> String.concat "\n"

let readChars () = readStr () |> Seq.toList

let seqSum (xs: seq<int>)= Seq.fold (fun acc v -> acc + v) 0 xs


module Monoid =
    type 'a t = {
        Empty: 'a
        Append: 'a -> 'a -> 'a
    }

    let sum = {
        Empty = 0;
        Append = (+)
    }

    let list = {
        Empty = [];
        Append = List.append
    }

    let concatAll monoid xs =
        Seq.fold (monoid.Append) monoid.Empty xs


module MapUtils =
    let union (monoid: 'a Monoid.t) ma mb =
        let folder acc k v = Map.change k (fun oldV -> Some(monoid.Append (Option.defaultValue monoid.Empty oldV) v)) acc
        Map.fold folder ma mb
        
    let singleton k v = Map.add k v Map.empty



module ParseUtils = 
    open FParsec

    let parseOrExn (parser: Parser<'a, unit>) (s: string) =
        match CharParsers.run parser s with
        | Success(result, _, _) -> result
        | Failure(err, _, _) -> failwith err
