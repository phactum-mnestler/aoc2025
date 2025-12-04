namespace AOC2025

module AOC04 =

    let parse (lines: string array) : bool array2d =
        Array2D.init lines.Length lines[0].Length (fun i j -> lines[i][j] = '@')

    let isReachable (map: bool array2d) x y _ =
        if not map[x, y] then
            false
        else
            let neighborsX =
                [ x - 1; x; x + 1 ] |> Seq.filter (fun x -> x >= 0 && x < Array2D.length1 map)

            let neighborsY =
                [ y - 1; y; y + 1 ] |> Seq.filter (fun y -> y >= 0 && y < Array2D.length2 map)

            let neighbors =
                neighborsX
                |> Seq.collect (fun nx -> neighborsY |> Seq.map (fun ny -> nx, ny))
                |> Seq.filter (fun (nx, ny) -> x <> nx || y <> ny)

            neighbors |> Seq.filter (fun (nx, ny) -> map[nx, ny]) |> Seq.length < 4

    let print (map: bool array2d) hit miss =
        for i in 0 .. Array2D.length1 map - 1 do
            for j in 0 .. Array2D.length2 map - 1 do
                printf "%s" (if map[i, j] then hit else miss)

            printfn ""

    let count (map: bool array2d) =
        map |> Seq.cast<bool> |> Seq.filter (fun x -> x) |> Seq.length

    let solveA (lines: string array) =
        let initial = parse (lines)
        print initial "@" "."
        printfn ""
        let reachable: bool array2d = initial |> Array2D.mapi (isReachable initial)
        print reachable "X" "."
        count reachable |> string

    let rec cullReachable (map: bool array2d) =
        let culled = map |> Array2D.mapi (fun x y f -> f && not (isReachable map x y f))

        if count map = count culled then
            map
        else
            cullReachable culled

    let solveB (lines: string array) =
        let initial = parse (lines)
        print initial "@" "."
        printfn ""
        let unreachable = cullReachable initial
        print initial "@" "."
        printfn ""
        printfn "Before: %d, after: %d" (count initial) (count unreachable)
        count initial - count unreachable |> string
