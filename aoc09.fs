namespace AOC2025

module AOC09 =
    type Coordinate = (int64 * int64)

    let parseLine (line: string) =
        let segments = line.Split(",") |> Array.map int64
        Coordinate(segments[0], segments[1])

    let areaBetween ((aX, aY), (bX, bY)) =
        (abs (bX - aX) + int64 1) * (abs (bY - aY) + int64 1)

    let generatePairs coordinates =
        coordinates
        |> Array.mapi (fun i from -> coordinates |> Array.skip (i + 1) |> Array.map (fun toCoord -> from, toCoord))
        |> Array.concat

    let solveA (lines: string array) =
        lines
        |> Array.map parseLine
        |> generatePairs
        |> Array.map areaBetween
        |> Array.sortDescending
        |> Array.head
        |> string

    let isOnEdge (coord: Coordinate) ((start: Coordinate, finish: Coordinate)) =
        let startX, startY = start
        let endX, endY = finish
        let coordX, coordY = coord

        coordY = startY
        && coordY = endY
        && coordX >= min startX endX
        && coordX <= max startX endX

    let remapXCoordinates redTiles =
        let forwardMapping =
            redTiles
            |> Array.map (fun (x, _) -> x)
            |> Array.sort
            |> Array.distinct
            |> Array.mapi (fun i x -> x, int64 (i + 1))
            |> Map.ofArray

        let backwardsMapping =
            redTiles
            |> Array.map (fun (x, _) -> x)
            |> Array.sort
            |> Array.distinct
            |> Array.mapi (fun i x -> int64 (i + 1), x)
            |> Map.ofArray

        forwardMapping, backwardsMapping

    let remapYCoordinates redTiles =
        let forwardMapping =
            redTiles
            |> Array.map (fun (_, y) -> y)
            |> Array.sort
            |> Array.distinct
            |> Array.mapi (fun i y -> y, int64 (i + 1))
            |> Map.ofArray

        let backwardsMapping =
            redTiles
            |> Array.map (fun (_, y) -> y)
            |> Array.sort
            |> Array.distinct
            |> Array.mapi (fun i y -> int64 (i + 1), y)
            |> Map.ofArray

        forwardMapping, backwardsMapping

    let remapCoordinate tile (remappedXCoordinates: Map<int64, int64>) (remappedYCoordinates: Map<int64, int64>) =
        let x, y = tile
        remappedXCoordinates[x], remappedYCoordinates[y]

    type CollisionStates =
        | Outside
        | PassingRight // On vertical edge, tiles to the left -> outside if horizontal edge going left is found
        | PassingLeft // On vertical edge, tiles to the right -> outside if horizontal edge going left is found
        | PassingThrough // In middle of the tiles            -> outside if middle of horizontal edge is found
        | Leaving // Was about to leave, but current tile is still within shape

    let scanHorizontalEdges edges collisionState (x, y) =
        let collidedEdge = Array.tryFind (isOnEdge (x, y)) edges

        match collidedEdge with
        | None -> if collisionState = Leaving then Outside else collisionState
        | Some((startX, startY), (endX, endY)) ->
            match collisionState with
            | PassingLeft ->
                if min startX endX < x && max startX endX > x then Outside // Should never happen
                else if min startX endX = x then Leaving
                else if max startX endX = x then PassingThrough
                else Outside // Should never happen
            | PassingRight ->
                if min startX endX < x && max startX endX > x then Outside // Should never happen
                else if min startX endX = x then PassingThrough
                else if max startX endX = x then Leaving
                else Outside // Should never happen
            | PassingThrough ->
                if min startX endX < x && max startX endX > x then Leaving
                else if min startX endX = x then PassingRight
                else if max startX endX = x then PassingLeft
                else Outside // Should never happen
            | Outside ->
                if min startX endX < x && max startX endX > x then
                    PassingThrough
                else if min startX endX = x then
                    PassingLeft
                else if max startX endX = x then
                    PassingRight
                else
                    Outside // Should never happen
            | Leaving ->
                if min startX endX < x && max startX endX > x then
                    PassingThrough
                else if min startX endX = x then
                    PassingLeft
                else if max startX endX = x then
                    PassingRight
                else
                    Outside // Should never happen

    let collectTilesBetween
        (edges: (Coordinate * Coordinate) array)
        ((startX, startY): Coordinate, (endX, endY): Coordinate)
        : Set<Coordinate> =
        let horizontalEdges =
            edges |> Array.filter (fun ((startX, startY), (endX, endY)) -> startY = endY)

        let collectTilesInLine x startY endY horizontalEdges =
            let coords = [| startY..endY |] |> Array.map (fun y -> x, y)

            let states =
                coords
                |> Array.scan (scanHorizontalEdges horizontalEdges) CollisionStates.Outside
                |> Array.skip 1

            Array.zip coords states
            |> Array.filter (fun (_, state) -> state <> Outside)
            |> Array.map (fun (coord, _) -> coord)


        [| startX..endX |]
        |> Array.collect (fun x -> collectTilesInLine x startY endY horizontalEdges)
        |> Set.ofArray

    let fastTileCheckPerimeter isTiled (fromCorner: Coordinate, toCorner: Coordinate) =
        let fromX, fromY = fromCorner
        let toX, toY = toCorner

        let allHorizontalEdges =
            [| fromY; toY |]
            |> Array.allPairs [| (min fromX toX) .. (max fromX toX) |]
            |> Array.forall (fun tile -> Set.contains tile isTiled)

        let allVerticalEdges =
            [| (min fromY toY) .. (max fromY toY) |]
            |> Array.allPairs [| fromX; toX |]
            |> Array.forall (fun tile -> Set.contains tile isTiled)

        allHorizontalEdges && allVerticalEdges

    let checkAllTilesInSquare isTiled (fromCorner, toCorner) = true // Not necessary for 09b

    let solveB (lines: string array) =
        let redTiles = lines |> Array.map parseLine
        let remappedXCoordinates, invXMapping = remapXCoordinates redTiles
        let remappedYCoordinates, invYMapping = remapYCoordinates redTiles

        let remappedRedTiles =
            redTiles
            |> Array.map (fun (x, y) -> remappedXCoordinates[x], remappedYCoordinates[y])

        let edges =
            remappedRedTiles
            |> Array.pairwise
            |> Array.append [| Array.last remappedRedTiles, Array.head remappedRedTiles |]

        let boundingBox =
            (int64 0, int64 0), (int64 (remappedXCoordinates.Count + 1), int64 (remappedYCoordinates.Count + 1))

        let isTiled = collectTilesBetween edges boundingBox

        if lines.Length <= 20 then
            let (startX, startY), (endX, endY) = boundingBox

            for y in [ startY..endY ] do
                for x in [ startX..endX ] do
                    if Set.contains (x, y) isTiled then
                        printf "#"
                    else
                        printf "."

                printfn ""
        else
            printfn "Not printing diagram because input is too large."

        let remappedFromCorner, remappedToCorner =
            redTiles
            |> generatePairs
            |> Array.sortByDescending areaBetween
            |> Array.map (fun (fromCorner, toCorner) ->
                remapCoordinate fromCorner remappedXCoordinates remappedYCoordinates,
                remapCoordinate toCorner remappedXCoordinates remappedYCoordinates)
            |> Seq.ofArray
            |> Seq.filter (fastTileCheckPerimeter isTiled)
            |> Seq.filter (checkAllTilesInSquare isTiled)
            |> Seq.head

        printfn "Largest rectangle: %s to %s" (string remappedFromCorner) (string remappedToCorner)

        areaBetween (
            remapCoordinate remappedFromCorner invXMapping invYMapping,
            remapCoordinate remappedToCorner invXMapping invYMapping
        )
        |> string
