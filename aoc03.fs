namespace AOC2025

module AOC03 =

    let biggestTwoDigits (x: string) : string =
        let indexA, valueA =
            x.ToCharArray()
            |> Array.rev
            |> Array.skip 1
            |> Array.rev
            |> Array.mapi (fun i v -> i, v)
            |> Array.maxBy (fun (_, v) -> v)

        let valueB = x.ToCharArray() |> Array.skip (indexA + 1) |> Array.max
        $"{valueA}{valueB}"

    let solveA (lines: string array) =
        lines |> Array.map biggestTwoDigits |> Array.map int |> Array.sum |> string

    let rec biggestNDigits (n: int) (x: char array) : char seq =
        let indexMax, valueMax =
            x
            |> Array.rev
            |> Array.skip (n - 1)
            |> Array.rev
            |> Array.mapi (fun i v -> i, v)
            |> Array.maxBy (fun (i, v) -> v)

        if n = 1 then
            Seq.singleton valueMax
        else
            let head = Seq.singleton valueMax
            let tail = biggestNDigits (n - 1) (x |> Array.skip (indexMax + 1))
            Seq.concat [ head; tail ]

    let solveB (lines: string array) =
        Seq.ofArray lines
        |> Seq.map _.ToCharArray()
        |> Seq.map (biggestNDigits 12)
        |> Seq.map Array.ofSeq
        |> Seq.map System.String
        |> Seq.map int64
        |> Seq.sum
        |> string
