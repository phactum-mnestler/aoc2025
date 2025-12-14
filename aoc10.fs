namespace AOC2025

module AOC10 =
    open System
    type Coordinate = (int64 * int64)

    let parseLine (line: string) =
        let segments = line.Split(" ")

        let targetIndicators =
            segments[0]
            |> _.Trim([| '['; ']' |])
            |> _.ToCharArray()
            |> Array.map (fun c -> if c = '#' then true else false)

        let buttons =
            segments
            |> Array.skip 1
            |> Array.take (segments.Length - 2)
            |> Array.map (fun segment -> segment.Trim([| '('; ')' |]).Split(",") |> Array.map int)

        let joltage =
            Array.last segments |> _.Trim([| '{'; '}' |]) |> _.Split(",") |> Array.map int

        targetIndicators, buttons, joltage

    let renderIndicators indicators =
        indicators
        |> Array.map (fun i -> if i then "#" else ".")
        |> String.concat ""
        |> sprintf "[%s]"

    let renderButtons buttons =
        buttons
        |> Array.map (fun button -> button |> Array.map string |> String.concat "," |> sprintf "(%s)")
        |> String.concat " "

    let renderJoltage joltage =
        joltage |> Array.map string |> String.concat "," |> sprintf "{%s}"

    let pressIndicatorButton (indicators: bool array) button =
        button
        |> Array.fold
            (fun state i -> Array.init state.Length (fun j -> if j = i then not state[j] else state[j]))
            indicators

    let rec reachTargetIndicators targetIndicators buttons (state: bool array seq) =
        if Seq.exists (fun indicators -> targetIndicators = indicators) state then
            0
        else
            let nextState =
                state
                |> Seq.collect (fun indicators -> buttons |> Seq.map (pressIndicatorButton indicators))

            1 + reachTargetIndicators targetIndicators buttons nextState

    let solveA (lines: string array) =
        lines
        |> Array.map parseLine
        |> Array.iter (fun (targetIndicators, buttons, joltage) ->
            printfn "%s %s %s" (renderIndicators targetIndicators) (renderButtons buttons) (renderJoltage joltage))

        lines
        |> Array.map parseLine
        |> Array.map (fun (targetIndicators, buttons, _) ->
            let result =
                reachTargetIndicators targetIndicators buttons [ Array.create targetIndicators.Length false ]

            printfn "New result: %d" result
            result)
        |> Array.sum
        |> string

    let solveB (lines: string array) = "unsolved"
