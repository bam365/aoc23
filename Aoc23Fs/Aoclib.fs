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

module ParseUtils = 
    open FParsec

    let parseOrExn (parser: Parser<'a, unit>) (s: string) =
        match CharParsers.run parser s with
        | Success(result, _, _) -> result
        | Failure(err, _, _) -> failwith err
