namespace AOC2025

open System.Text.RegularExpressions

module AOC02 =

    let parseRange (rangeStr: string) =
        let segments = rangeStr.Split("-")
        int64 (segments[0]), int64 (segments[1])

    let range (from: int64, toIncl: int64) =
        seq { from .. int64 (1) .. toIncl + int64 (1) } |> Array.ofSeq

    let isInvalidA n =
        let str = string (n)

        if str.Length % 2 = 0 then
            str.Substring(0, str.Length / 2) = str.Substring(str.Length / 2)
        else
            false

    let solveA (lines: string array) =
        lines
        |> Array.collect (fun line -> line.Split(","))
        |> Array.map parseRange
        |> Array.collect range
        |> Array.filter isInvalidA
        |> Array.sum
        |> string

    let matches (str: string) (s: string) = Regex.IsMatch(str, $"^(?:{s})+$")

    let isRepeated (str: string) n =
        if str.Length % n <> 0 then
            false
        else
            str.ToCharArray()
            |> Array.chunkBySize (str.Length / n)
            |> Array.map System.String
            |> Array.distinct
            |> Array.length = 1

    let isInvalidB n =
        let str = string (n)

        if str.Length = 1 then
            false
        else
            [ 2 .. str.Length ] |> Seq.exists (isRepeated str)

    let solveB (lines: string array) =
        lines
        |> Array.collect (fun line -> line.Split(","))
        |> Array.map parseRange
        |> Array.collect range
        |> Array.filter isInvalidB
        |> Array.sum
        |> string
