namespace AOC2025

module AOC01 =

    let dialSize = 100

    let parse (expr: string) =
        if expr.StartsWith("R") then
            int (expr.Substring(1))
        else
            -int(expr.Substring(1))


    let norm x d = (x + d) % d

    let solveA (lines: string array) =
        lines
        |> Array.map parse
        |> Array.scan (fun sum next -> norm (sum + next) dialSize) 50
        |> Array.filter (fun dial -> dial = 0)
        |> Array.length
        |> string

    let points (before, after) =
        if after > before then
            int (floor (float (after) / 100.0) - floor (float (before) / 100.0))
        else if before > after then
            int (floor ((float (before) - 1.0) / 100.0) - floor ((float (after) - 1.0) / 100.0))
        else
            0

    let solveB (lines: string array) =
        lines
        |> Array.map parse
        |> Array.scan (fun sum next -> sum + next) 50
        |> Array.pairwise
        |> Array.map points
        |> Array.sum
        |> string
