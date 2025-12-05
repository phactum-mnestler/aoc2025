namespace AOC2025

module AOC05 =

    type Range = { from: int64; toIncl: int64 }

    let contains x range = x >= range.from && x <= range.toIncl

    let merge (a: Range) (b: Range) : Range option =
        if a.toIncl < b.from then
            None
        else
            Some
                { from = min a.from b.from
                  toIncl = max a.toIncl b.toIncl }

    let length (range: Range) = range.toIncl - range.from + int64 (1)

    let parseRange (str: string) : Range =
        let parts = str.Split("-")

        { from = int64 (parts[0])
          toIncl = int64 (parts[1]) }

    let parse (lines: string array) =
        let goodRanges =
            lines |> Array.takeWhile (fun line -> line.Length > 0) |> Array.map parseRange

        let probes =
            lines
            |> Array.skipWhile (fun line -> line.Length > 0)
            |> Array.skip 1
            |> Array.map int64

        goodRanges, probes

    let solveA (lines: string array) =
        let goodRanges, probes = parse lines

        let freshIngredients =
            probes
            |> Array.filter (fun p -> goodRanges |> Array.exists (contains p))
            |> Array.length

        string freshIngredients


    let mergeOverlapping (acc: Range array) (next: Range) : Range array =
        if acc.Length = 0 then
            [| next |]
        else
            match merge (Array.last acc) next with
            | Some merged ->
                Array.set acc (acc.Length - 1) merged
                acc
            | None -> Array.append acc [| next |]

    let solveB (lines: string array) =
        let goodRanges, _ = parse lines

        goodRanges
        |> Array.sortBy _.from
        |> Array.fold mergeOverlapping [||]
        |> Array.sumBy length
        |> string
