namespace AOC2025

module AOC07 =

    let parseLine (line: string) =
        line.ToCharArray() |> Array.map (fun c -> c <> '.')

    let printState state =
        state |> Array.iter (fun b -> if b then printf "%s" "|" else printf "%s" ".")

        printfn ""

    let nextStep ((prevCount: int, prevLine: bool array)) (_, currentLine: bool array) =
        let resultState =
            Array.init prevLine.Length (fun i ->
                if currentLine[i] then
                    false
                else if i > 0 && currentLine[i - 1] then
                    not currentLine[i]
                else if i < prevLine.Length - 1 && currentLine[i + 1] then
                    not currentLine[i]
                else
                    prevLine[i])

        let resultCount =
            [ 1 .. (prevLine.Length - 1) ]
            |> List.filter (fun i -> prevLine[i] && currentLine[i])
            |> List.length
            |> (+) prevCount

        resultCount, resultState

    let solveA (lines: string array) =
        printfn "%d" lines.Length

        let totalCount, _ =
            lines
            |> Array.map parseLine
            |> Array.map (fun x -> 0, x)
            |> Array.reduce nextStep

        string totalCount

    let rec countLeavesInTree rayPos linePos (lines: bool array array) (precomputedResults: int64 ref array array) =
        if lines.Length = linePos then
            int64 (1)
        else
            let precomputedValue = precomputedResults[linePos][rayPos]

            if precomputedValue.Value <> -1 then
                precomputedValue.Value
            else
                let result =
                    if not (lines[linePos][rayPos]) then
                        countLeavesInTree rayPos (linePos + 1) lines precomputedResults
                    else if rayPos = 0 then
                        countLeavesInTree (rayPos + 1) (linePos + 1) lines precomputedResults
                    else if rayPos = lines[linePos].Length - 1 then
                        countLeavesInTree (rayPos - 1) (linePos + 1) lines precomputedResults
                    else
                        countLeavesInTree (rayPos - 1) (linePos + 1) lines precomputedResults
                        + countLeavesInTree (rayPos + 1) (linePos + 1) lines precomputedResults

                precomputedValue.Value <- result
                result

    let solveB (lines: string array) =
        let map = Array.map parseLine lines
        let initialPosition = map[0] |> Array.findIndex (fun x -> x)

        let precomputedResults =
            Array.init map.Length (fun i -> Array.init map[i].Length (fun j -> ref (int64 -1)))

        string (countLeavesInTree initialPosition 1 map precomputedResults)
