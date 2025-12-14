namespace AOC2025

module AOC12 =
    let parseShape (shapeString: string) =
        let lines = shapeString.Split "\n"
        let id = lines[0].Substring(0, lines[0].Length - 1)

        let shape =
            lines
            |> Array.skip 1
            |> Array.map (fun line -> line.ToCharArray() |> Array.map (fun c -> c = '#'))

        id, shape

    let parseArea (areaString: string) =
        let size = areaString.Substring(0, areaString.IndexOf ":").Split "x"
        let width = int size[0]
        let length = int size[1]

        let amounts =
            areaString
            |> _.Substring(areaString.IndexOf(":") + 2)
            |> _.Split(" ")
            |> Array.map int

        (width, length), amounts

    let countTrues (bools: bool array array) =
        bools |> Array.sumBy (fun x -> x |> Array.sumBy (fun y -> if y then 1 else 0))

    let requiredSize (amounts: int array) (shapes: (string * bool array array) array) =
        Array.zip amounts shapes
        |> Array.sumBy (fun (amount, (_, shape)) -> amount * countTrues shape)

    let solveA (lines: string array) =
        let segments = String.concat "\n" lines |> _.Split("\n\n")
        let shapes = segments |> Array.take (segments.Length - 1) |> Array.map parseShape
        let areas = segments |> Array.last |> _.Split("\n") |> Array.map parseArea

        let definitelyImpossible =
            areas
            |> Array.filter (fun ((width, length), amounts) -> width * length < requiredSize amounts shapes)
            |> Array.length

        let definitelyPossible =
            areas
            |> Array.filter (fun ((width, length), amounts) -> width * length >= Array.sum amounts * 9)
            |> Array.length

        let undetermined = areas.Length - definitelyImpossible - definitelyPossible

        printfn
            "Definitely possible: %d, definitely impossible: %d, undetermined: %d"
            definitelyPossible
            definitelyImpossible
            undetermined

        if undetermined = 0 then
            string definitelyPossible
        else
            sprintf "Between %d and %d" definitelyPossible (definitelyPossible + undetermined)

    let solveB (lines: string array) = "Merry Christmas!"
